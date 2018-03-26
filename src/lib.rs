//! The Micro-ataumaton (mu-taumaton) - a very, very small VM, intended for use
//! in my WIP game, working name "Swarm". This VM is intended to have thousands
//! of distinct, sandboxed instances running in memory all at once. It also
//! needs to support an odd combination of programming paradigms - backtracking,
//! unification-based logic programming and imperative programming in a
//! Scheme-like language.

#![feature(trace_macros)]
#![recursion_limit = "1024"]

#[macro_use]
extern crate enum_primitive_derive;
extern crate num_traits;

#[macro_use]
mod insn_macros;
mod insn;
#[macro_use]
mod machine_macros;
mod machine;
mod tag;
mod word;
