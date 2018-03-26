use petgraph::graph::{EdgeIndex, NodeIndex};
use petgraph::stable_graph::StableDiGraph;

use lisp::atom::{Keyword, Number, Symbol, Tomb};
use lisp::eval::{Expr, Value};


pub struct Program {
    metadeps: StableDiGraph<RawSyntax, ()>,
    strings: Tomb,
}


impl Program {
    fn finalize(&mut self, expr: &RawSyntax) -> Option<Syntax> {
        match *expr {
            RawSyntax::AtomS(sym) => Some(Syntax::AtomS(sym)),
            RawSyntax::AtomN(num) => Some(Syntax::AtomN(num)),
            RawSyntax::Cons(ref pair) => {
                match (self.finalize(&pair.0), self.finalize(&pair.1)) {
                    (Some(car), Some(cdr)) => Some(Syntax::cons(car, cdr)),
                    _ => None,
                }
            }
            RawSyntax::Nil => Some(Syntax::Nil),
            RawSyntax::Quote(ref expr) => {
                match self.finalize(expr) {
                    Some(value) => {
                        Some(Syntax::cons(Syntax::AtomS(self.strings.symbol("quote")), value))
                    }
                    None => None,
                }
            }
            RawSyntax::Meta(..) => None,
        }
    }
}


#[derive(Clone, Debug)]
pub enum RawSyntax {
    AtomS(Symbol),
    AtomN(Number),
    Cons(Box<(RawSyntax, RawSyntax)>),
    Nil,
    Quote(Box<RawSyntax>),
    Meta(Box<RawSyntax>),
}


#[derive(Clone, Debug)]
pub enum Syntax {
    AtomS(Symbol),
    AtomN(Number),
    Cons(Box<(Syntax, Syntax)>),
    Nil,
}


pub type ParseResult<T> = Result<T, ParseError>;


pub enum ParseError {
    UnknownNode(Symbol),
    ExpectedPair(Syntax),
    ExpectedSymbol(Syntax),
    ErrorWhileParsing {
        errors: Vec<ParseError>,
        syntax: Syntax,
    },
}


impl Syntax {
    fn smoosh<T, U>(&self, left: ParseResult<T>, right: ParseResult<U>) -> ParseResult<(T, U)> {
        match (left, right) {
            (Ok(t), Ok(u)) => Ok((t, u)),
            (Err(err), Ok(..)) |
            (Ok(..), Err(err)) => Err(err),
            (Err(err0), Err(err1)) => {
                Err(ParseError::ErrorWhileParsing {
                        errors: vec![err0, err1],
                        syntax: self.clone(),
                    })
            }
        }
    }


    pub fn cons(car: Syntax, cdr: Syntax) -> Syntax {
        Syntax::Cons(Box::new((car, cdr)))
    }


    fn flatten_subtree(&self, strings: &Tomb) -> ParseResult<Vec<Expr>> {
        fn flatten_subtree_rec(val: &Syntax,
                               strings: &Tomb,
                               mut subtree: Vec<Expr>)
                               -> ParseResult<Vec<Expr>> {
            match *val {
                Syntax::Cons(ref pair) => {
                    subtree.push(pair.0.parse(strings)?);
                    flatten_subtree_rec(&pair.1, strings, subtree)
                }
                _ => {
                    subtree.push(val.parse(strings)?);
                    Ok(subtree)
                }
            }
        }

        flatten_subtree_rec(self, strings, Vec::new())
    }


    pub fn uncons(&self) -> ParseResult<(&Syntax, &Syntax)> {
        match *self {
            Syntax::Cons(ref pair) => Ok((&pair.0, &pair.1)),
            other => Err(ParseError::ExpectedPair(self.clone()))
        }
    }


    pub fn parse_keyword(&self, strings: &Tomb) -> ParseResult<Keyword> {
        let sym = match *self {
            Syntax::AtomS(ref sym) => Ok(sym),
            _ => Err(ParseError::ExpectedSymbol(self.clone())),
        }?;

        match sym.as_keyword(strings) {
            Some(kw) => Ok(kw),
            None => Err(ParseError::UnknownNode(*sym)),
        }
    }


    pub fn parse_node(&self, strings: &Tomb) -> ParseResult<Expr> {
        let (ref head, ref spine) = self.uncons()?;
        let kw = self.parse_keyword();
    }


    pub fn parse(&self, strings: &Tomb) -> ParseResult<Expr> {
        match *self {
            Syntax::Cons(ref pair) => {
                match pair.0 {
                    Syntax::AtomS(ref sym) => {
                        match sym.as_keyword(strings) {
                            Some(Keyword::Apply) => {
                                match pair.1 {
                                    Syntax::Cons(ref app) => {
                                        let head_res = app.0.parse(strings);
                                        let spine_res = app.1.flatten_subtree(strings);

                                        let (head, spine) = self.smoosh(head_res, spine_res)?;

                                        Ok(Expr::Apply(Box::new(head), spine))
                                    }
                                    ref single => single.parse(strings),
                                }
                            }
                            Some(Keyword::Cons) => {
                                let (car_raw, cdr_raw) = pair.1.uncons();
                                match pair.1 {
                                    Syntax::Cons(ref pair) => {
                                        let car_res = pair.0.parse(strings);
                                        let cdr_res = pair.1.parse(strings);

                                        let (car, cdr) = self.smoosh(car_res, cdr_res)?;

                                        Ok(Expr::cons(car, cdr))
                                    }
                                    ref other => Err(ParseError::ExpectedPair(other.clone()))
                                }
                            }
                            Some(Keyword::Lambda) => {
                                
                            }
                            None => Ok(Expr::Free(*sym)),
                        }
                    }
                }
            }
        }
    }
}
