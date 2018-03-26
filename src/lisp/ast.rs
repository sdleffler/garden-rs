use std::fmt;

use lisp::atom::{Symbol, Tomb};


#[derive(Clone, Debug)]
pub enum Expr {
    Atom(Atom),
    Bool(bool),
    Int(i32),
    Cons(Box<(Expr, Expr)>),
    Nil,
}


impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expr::Atom(ref a) => write!(f, "{}", a),
            Expr::Bool(ref b) => write!(f, "{}", b),
            Expr::Int(ref i) => write!(f, "{}", i),
            Expr::Cons(ref boxed) => {
                write!(f, "({}", &boxed.as_ref().0)?;
                let mut prev_cdr = &boxed.as_ref().1;
                while let Some((car, cdr)) = prev_cdr.uncons_ref() {
                    write!(f, " {}", car)?;
                    prev_cdr = cdr;
                }
                if prev_cdr.is_nil() {
                    write!(f, ")")
                } else {
                    write!(f, "{})", prev_cdr)
                }
            },
            Expr::Nil => write!(f, "()"),
        }
    }
}


impl Expr {
    pub fn is_nil(&self) -> bool {
        match *self {
            Expr::Nil => true,
            _ => false,
        }
    }


    pub fn is_atom(&self, target: &Atom) -> bool {
        match *self {
            Expr::Atom(ref atom) if atom == target => true,
            _ => false,
        }
    }


    pub fn unatom(self) -> Result<Atom, Expr> {
        match self {
            Expr::Atom(atom) => Ok(atom),
            _ => Err(self),
        }
    }


    pub fn unatom_ref(&self) -> Option<Atom> {
        match *self {
            Expr::Atom(ref atom) => Some(*atom),
            _ => None,
        }
    }


    pub fn cons(self, cdr: Expr) -> Expr {
        Expr::Cons(Box::new((self, cdr)))
    }


    pub fn is_cons(&self) -> bool {
        match *self {
            Expr::Cons(_) => true,
            _ => false,
        }
    }


    pub fn uncons(self) -> Result<(Expr, Expr), Expr> {
        match self {
            Expr::Cons(boxed) => Ok(*boxed),
            _ => Err(self),
        }
    }


    pub fn uncons_ref(&self) -> Option<(&Expr, &Expr)> {
        match *self {
            Expr::Cons(ref boxed) => {
                let (ref car, ref cdr) = *boxed.as_ref();
                Some((car, cdr))
            }
            _ => None,
        }
    }


    pub fn unnil(self) -> Result<(), Expr> {
        match self {
            Expr::Nil => Ok(()),
            _ => Err(self),
        }
    }


    pub fn flatten(self) -> Result<Vec<Expr>, Expr> {
        self.uncons()
            .and_then(|(car, cdr)| match cdr.flatten() {
                Ok(mut flat) => {
                    flat.push(car);
                    Ok(flat)
                }
                Err(cdr) => Err(car.cons(cdr)),
            })
            .or_else(|expr| {
                expr.unnil().map(|()| Vec::new())
            })
    }


    pub fn flatten_atoms(self) -> Result<Vec<Atom>, Expr> {
        self.uncons().and_then(|(car, cdr)| match cdr.flatten_atoms() {
            Ok(mut atoms) => {
                match car.unatom() {
                    Ok(atom) => {
                        atoms.push(atom);
                        Ok(atoms)
                    }
                    Err(car) => {
                        let cdr = atoms.into_iter().map(Expr::Atom).fold(Expr::Nil, Expr::cons);
                        Err(car.cons(cdr))
                    }
                }
            }
            Err(cdr) => Err(car.cons(cdr)),
        })
    }
}


pub struct Program {
    pub toplevel: Vec<Expr>,
    pub strings: Tomb,
}
