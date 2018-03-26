use std::fmt;

use ordered_float::OrderedFloat;
use string_interner::{StringInterner, NonNegative};


#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Interned(usize);

impl From<usize> for Interned {
    fn from(u: usize) -> Interned {
        Interned(u)
    }
}

impl From<Interned> for usize {
    fn from(a: Interned) -> usize {
        a.0
    }
}

impl NonNegative for Interned {}

impl fmt::Display for Interned {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}


#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Generated(usize);


#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Symbol {
    String(Interned),
    Hinted(Interned, Generated),
    Gensym(Generated),
}


#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Keyword {
    Cons,
    Apply,
    Lambda,
    Quote,
    Meta,
}


#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Number {
    Integer(i32),
    Float(OrderedFloat<f64>),
}


#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Atom {
    Symbol(Symbol),
    Number(Number),
}


#[derive(Clone, Debug)]
pub struct Tomb {
    strings: StringInterner<Interned>,
    fresh: usize,
}


impl Tomb {
    fn generate_symbol(&mut self) -> Generated {
        let gensym = Generated(self.fresh);
        self.fresh += 1;
        gensym
    }


    pub fn symbol<T: Into<String> + AsRef<str>>(&mut self, sym: T) -> Symbol {
        Symbol::String(self.strings.get_or_intern(sym))
    }


    pub fn gensym(&mut self) -> Symbol {
        Symbol::Gensym(self.generate_symbol())
    }


    pub fn gensym_with_hint<T: Into<String> + AsRef<str>>(&mut self, hint: T) -> Symbol {
        Symbol::Hinted(self.strings.get_or_intern(hint), self.generate_symbol())
    }
}


impl Symbol {
    pub fn as_keyword(&self, tomb: &Tomb) -> Option<Keyword> {
        match *self {
            Symbol::String(sym) => tomb.strings.resolve(sym).and_then(|s| match s {
                "apply" => Some(Keyword::Apply),
                "cons" => Some(Keyword::Cons),
                "lambda" => Some(Keyword::Lambda),
                "meta" => Some(Keyword::Meta),
                "quote" => Some(Keyword::Quote),
                _ => None,
            }),
            _ => None,
        }
    }
}
