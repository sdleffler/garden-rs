use std::cell::RefCell;
use std::str::{self, FromStr};

use nom::{digit, IResult};
use string_interner::StringInterner;

use lisp::ast::Expr;
use lisp::atom::{Symbol, Tomb};


fn atom<'a>(input: &'a str, tomb: &'a mut Tomb) -> IResult<&'a str, Symbol> {
    map!(input,
         is_not_s!(" \t\r\n()."),
         |s: &'a str| tomb.symbol(s))
}


fn list<'a>(input: &'a str, tomb: &'a mut Tomb) -> IResult<&'a str, Expr> {
    delimited!(input,
               char!('('),
               fold_many0!(call!(expr, tomb), Expr::Nil, Expr::cons),
               char!(')'))
}


fn pair<'a>(input: &'a str, tomb: &'a mut Tomb) -> IResult<&'a str, Expr> {
    do_parse!(input,
        char!('(')             >>
        car: call!(expr, tomb) >>
        char!('.')             >>
        cdr: call!(expr, tomb) >>
        char!(')')             >>
        (car.cons(cdr))
    )
}


fn sign_to_i32(s: char) -> i32 {
    match s {
        '-' => -1,
        '+' => 1,
        _ => unreachable!(),
    }
}


named!(integer<&str, i32>, do_parse!(
    sign: opt!(alt!(char!('-') | char!('+'))) >>
    body: map_res!(ws!(digit), i32::from_str) >>
    (sign.map_or(1i32, sign_to_i32) * body)
));


fn expr<'a>(input: &'a str, tomb: &'a mut Tomb) -> IResult<&'a str, Expr> {
    alt_complete!(input,
                  map!(integer, Expr::Int) | call!(list, tomb) |
                  map!(call!(atom, tomb), Expr::Atom))
}
