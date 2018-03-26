use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

use lisp::atom::{Atom, Number, Symbol};
use lisp::mequo::Syntax;

pub mod errors {
    use lisp::atom::Symbol;
    use super::Expr;

    error_chain! {
        types {
            EvalError, EvalErrorKind, EvalResultExt, EvalResult;
        }

        errors {
            ArityMismatch(callsite: String, expected: usize, found: usize) {
                description("arity mismatch when attempting to call function")
                display("arity mismatch: function at callsite `{}` expected {} \
                         arguments, but got {}", callsite, expected, found)
            }
        }
    }


    error_chain! {
        types {
            ExpandError, ExpandErrorKind, ExpandResultExt, ExpandResult;
        }

        errors {
            Compound(errors: Vec<ExpandError>, node: String) {
                description("multiple errors while lowering macro result to syntax")
                display("multiple errors while lowering macro result `{}` to syntax{}", node, match errors.split_first() {
                        Some((head, tail)) => tail.iter().fold(head.to_string(), |acc, err| acc + ", " + &err.to_string()),
                        None => String::new()
                    })
            }
            NonSyntactic(value: String) {
                description("non-syntactic form discovered while lowering macro result to syntax")
                display("non-syntactic form discovered while lowering macro \
                         result to syntax: `{}` expected atom or dotted pair instead", value)
            }
        }
    }
}

use self::errors::*;


#[derive(Clone, Debug)]
pub enum Expr {
    Neutral(Value),
    Cons(Box<(Expr, Expr)>),
    Free(Symbol),
    Bound(Slot),
    Apply(Box<Expr>, Vec<Expr>),
    Begin(Vec<Expr>),
}


pub type Slot = Rc<RefCell<Value>>;


/// A `Value` is a neutral term.
///
/// `Expr::Var` is not present in `Value` because all variables must be resolved
///   as a matter of evaluation. Free variables are not valid values.
///
/// `Expr::Apply` is not present in `Value` because `Value` must be a neutral
///   term - impossible to beta-reduce further.
///
/// Cons-exprs are present in both because `Value::Cons` is a "neutral" pair in
///   which both car and cdr are evaluated, and `Expr::Cons` contains possible
///   redexes.
#[derive(Clone, Debug)]
pub enum Value {
    AtomS(Symbol),
    AtomN(Number),
    Cons(Slot, Slot),
    Nil,
    Lambda(Vec<Symbol>, Box<Expr>),
}


fn collect_errors(globals: &HashMap<Symbol, Slot>, spine: &[Expr]) -> Vec<EvalError> {
    spine
        .iter()
        .map(|e| e.eval(globals))
        .filter_map(Result::err)
        .collect()
}


fn eval_and_zip_spine(globals: &HashMap<Symbol, Slot>,
                      binds: &[Symbol],
                      spine: &[Expr])
                      -> Result<Vec<(Symbol, Slot)>, Vec<EvalError>> {
    let mut slots = Vec::new();
    let mut spine_it = spine.iter();

    while let Some(expr) = spine_it.next() {
        match expr.eval(globals) {
            Ok(slot) => slots.push(Rc::new(RefCell::new(slot))),
            Err(err) => {
                let mut errors = vec![err];
                errors.extend(spine_it.map(|e| e.eval(globals)).filter_map(Result::err));
                return Err(errors);
            }
        }
    }

    Ok(binds.iter().cloned().zip(slots.into_iter()).collect())
}


impl Expr {
    pub fn cons(car: Expr, cdr: Expr) -> Expr {
        Expr::Cons(Box::new((car, cdr)))
    }


    fn subst(&self, s: &[(Symbol, Slot)]) -> Expr {
        match *self {
            Expr::Neutral(ref value) => Expr::Neutral(value.subst(s)),
            Expr::Cons(ref cons) => Expr::cons(cons.0.subst(s), cons.1.subst(s)),
            Expr::Free(x) => {
                match s.iter().find(|&&(y, _)| x == y) {
                    Some(&(_, ref e)) => Expr::Bound(e.clone()),
                    None => Expr::Free(x),
                }
            }
            Expr::Bound(ref slot) => Expr::Bound(slot.clone()),
            Expr::Apply(ref head, ref body) => {
                Expr::Apply(Box::new(head.subst(s)),
                            body.iter().map(|e| e.subst(s)).collect())
            }
            Expr::Begin(ref stmts) => Expr::Begin(stmts.iter().map(|e| e.subst(s)).collect()),
        }
    }


    pub fn eval(&self, globals: &HashMap<Symbol, Slot>) -> EvalResult<Value> {
        match *self {
            Expr::Neutral(ref value) => Ok(value.clone()),
            Expr::Cons(ref cons) => {
                let res0 = cons.0.eval(globals);
                let res1 = cons.1.eval(globals);

                match (res0, res1) {
                    (Ok(val0), Ok(val1)) => Ok(Value::cons(val0, val1)),
                    (Err(err0), Err(err1)) => {
                        Err(EvalError::ErrorWhileEvaluating {
                                errors: vec![err0, err1],
                                expr: self.clone(),
                            })
                    }
                    (Err(err), Ok(..)) |
                    (Ok(..), Err(err)) => {
                        Err(EvalError::ErrorWhileEvaluating {
                                errors: vec![err],
                                expr: self.clone(),
                            })
                    }
                }
            }
            Expr::Free(ref var) => {
                match globals.get(var) {
                    Some(val) => Ok(val.borrow().clone()),
                    None => panic!("Unbound variable {:?}!", var),
                }
            }
            Expr::Bound(ref slot) => Ok(slot.borrow().clone()),
            Expr::Apply(ref head, ref spine) => {
                match **head {
                    Expr::Neutral(Value::Lambda(ref binds, ref body)) => {
                        if spine.len() != binds.len() {
                            bail!(EvalErrorKind::ArityMismatch(format!("{:?}", self), binds.len(), spine.len()));
                        } else {
                            let s_res = eval_and_zip_spine(globals, binds, spine).chain_err(|| );

                            match s_res {
                                Ok(s) => body.subst(&s).eval(&globals),
                                Err(errs) => {
                                    Err(EvalError::ErrorWhileEvaluating {
                                            errors: errs,
                                            expr: self.clone(),
                                        })
                                }
                            }
                        }
                    }
                    _ => panic!("Wrong type to apply!"),
                }
            }
            Expr::Begin(ref stmts) => {
                stmts
                    .iter()
                    .fold(Ok(Value::Nil), |_, expr| expr.eval(globals))
            }
        }
    }
}


impl Value {
    pub fn cons(car: Value, cdr: Value) -> Value {
        Value::Cons(Rc::new(RefCell::new(car)), Rc::new(RefCell::new(cdr)))
    }


    fn subst(&self, s: &[(Symbol, Slot)]) -> Value {
        match *self {
            Value::AtomS(..) | Value::AtomN(..) | Value::Nil => self.clone(),
            Value::Cons(ref car, ref cdr) => {
                Value::cons(car.borrow().subst(s), cdr.borrow().subst(s))
            }
            Value::Lambda(ref binds, ref body) => {
                let quot = s.iter()
                    .filter(|&&(x, _)| !binds.contains(&x))
                    .cloned()
                    .collect::<Vec<(Symbol, Slot)>>();
                Value::Lambda(binds.clone(), Box::new(body.subst(&quot)))
            }
        }
    }


    pub fn try_into_syntax(&self) -> ExpandResult<Syntax> {
        match *self {
            Value::AtomS(ref sym) => Ok(Syntax::AtomS(*sym)),
            Value::AtomN(ref num) => Ok(Syntax::AtomN(*num)),
            Value::Cons(ref car, ref cdr) => {
                match (car.borrow().try_into_syntax(), cdr.borrow().try_into_syntax()) {
                    (Ok(car), Ok(cdr)) => Ok(Syntax::cons(car, cdr)),
                    (Err(err0), Err(err1)) => {
                        Err(ExpandError::ErrorWhileExpanding {
                                errors: vec![err0, err1],
                                value: self.clone(),
                            })
                    }
                    (Err(err), Ok(..)) |
                    (Ok(..), Err(err)) => {
                        Err(ExpandError::ErrorWhileExpanding {
                                errors: vec![err],
                                value: self.clone(),
                            })
                    }
                }
            }
            Value::Nil => Ok(Syntax::Nil),
            Value::Lambda { .. } => Err(ExpandError::NonSyntactic(self.clone())),
        }
    }
}
