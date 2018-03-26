use std::ops::Index;

use word::{Function, Pointer, Word};


pub type Address = u32;
pub type Offset = i32;
pub type Local = u16;
pub type Prompt = u32;
pub type Register = u8;


#[derive(Clone, Copy, Debug)]
pub enum Src<W: Word> {
    C(W),
    L(Local),
    R(Register),
}


#[derive(Clone, Copy, Debug)]
pub struct SrcRange<W: Word> {
    pub src: Src<W>,
    pub len: u16,
}


#[derive(Clone, Copy, Debug)]
pub enum Dst {
    L(Local),
    R(Register),
}


#[derive(Clone, Copy, Debug)]
pub enum Trg {
    Absolute(Address),
    Relative(Offset),
}


#[derive(Clone, Copy, Debug)]
pub enum UnpackedInsn<W: Word> {
    // Control instructions
    Call(Src<W>),

    Return,

    Prompt { thunk: Src<W>, tag: Src<W>, handler: Src<W> },

    /// Uncoditional jump to a given static address.
    Jmp(Trg),

    /// Conditional jump to a given static address.
    JmpIf(Src<W>, Trg),

    MatchList { src: Src<W>, dst_car: Dst, dst_cdr: Dst, on_nil: Trg },

    /// Halt the machine entirely. If the machine `Panic`s, its state is
    /// considered invalid.
    ///
    /// TODO: Allow handler installation so that machines can recover from
    /// `Panic`s.
    ///
    /// NOTE: This is *not* for exception handling. Continuations should be used
    /// for exception handling.
    Panic,

    /// Trigger a garbage collection.
    ///
    /// *DANGER! DANGER WILL ROBINSON!*
    Collect,

    // Data instructions

    /// Move a single piece of data from one location to another.
    Move1 { src: Src<W>, dst: Dst },

    // Allocator instructions

    /// Create a closure from a given function and local variables. Closures are
    /// implemented essentially as holding extra arguments to the closure
    /// function, such that calling a closure means counting the arguments which
    /// are passed normally, and then loading the extra argument registers.
    ///
    /// `AllocClosure` takes in a range, which is saved into the local variable
    /// segment of the closure allocation.
    AllocClosure { src_fn: Function, src_locals: SrcRange<W>, dst: Dst },

    /// Create a cons cell from two values and supply a pointer to it.
    AllocCons { src_car: Src<W>, src_cdr: Src<W>, dst: Dst },

    /// Create a float from either a float or an integer and supply a pointer to
    /// it.
    ///
    /// TODO: Support float copying.
    // AllocFloat { src: Src<W>, dst: Dst },

    /// Create a fresh logical variable and supply a pointer to it.
    AllocLVar(Dst),

    // Primitive instructions
    IntAdd { src0: Src<W>, src1: Src<W>, dst: Dst },
    IntDiv { src0: Src<W>, src1: Src<W>, dst: Dst },
    IntMod { src0: Src<W>, src1: Src<W>, dst: Dst },
    IntMul { src0: Src<W>, src1: Src<W>, dst: Dst },
    IntSub { src0: Src<W>, src1: Src<W>, dst: Dst },

    IntNeg { src: Src<W>, dst: Dst },

    CmpIntEq { src0: Src<W>, src1: Src<W>, dst: Dst },
    CmpIntGe { src0: Src<W>, src1: Src<W>, dst: Dst },
    CmpIntGt { src0: Src<W>, src1: Src<W>, dst: Dst },
    CmpIntLe { src0: Src<W>, src1: Src<W>, dst: Dst },
    CmpIntLt { src0: Src<W>, src1: Src<W>, dst: Dst },
    CmpIntNe { src0: Src<W>, src1: Src<W>, dst: Dst },

    BitAnd { src0: Src<W>, src1: Src<W>, dst: Dst },
    BitOr { src0: Src<W>, src1: Src<W>, dst: Dst },
    BitXor { src0: Src<W>, src1: Src<W>, dst: Dst },
    BitLsh { src0: Src<W>, src1: Src<W>, dst: Dst },
    BitRsh { src0: Src<W>, src1: Src<W>, dst: Dst },

    BitNot { src: Src<W>, dst: Dst },

    BoolAnd { src0: Src<W>, src1: Src<W>, dst: Dst },
    BoolOr { src0: Src<W>, src1: Src<W>, dst: Dst },

    BoolNot { src: Src<W>, dst: Dst },

    ListCar { src: Src<W>, dst: Dst },
    ListCdr { src: Src<W>, dst: Dst },
}


#[derive(Clone, Copy, Debug)]
pub enum MvR<W: Word> {
    CR { src: W, dst: Register, range: u8 },
    LR { src: Local, dst: Register, range: u8 },
    CL { src: W, dst: Register, range: u8 },
    RL { src: Register, dst: Local, range: u8 },
}


pub trait Insn: Sized + Copy + Clone + ::std::fmt::Debug {
    type Word: Word;

    fn pack(&UnpackedInsn<Self::Word>) -> Self;
    fn unpack(&self) -> UnpackedInsn<Self::Word>;
}


impl<W: Word> Insn for UnpackedInsn<W> {
    type Word = W;

    fn pack(insn: &UnpackedInsn<W>) -> UnpackedInsn<W> { *insn }
    fn unpack(&self) -> UnpackedInsn<W> { *self }
}


#[derive(Copy, Clone, Debug)]
pub struct FunctionInfo {
    pub addr: u32,
    pub args: u8,
    pub rets: u8,
    pub locals: u16,
}


#[derive(Debug)]
pub struct Program<I: Insn> {
    pub fns: Vec<FunctionInfo>,
    pub code: Vec<I>,
}


impl<I: Insn> Index<Pointer> for Program<I> {
    type Output = I;

    fn index(&self, idx: Pointer) -> &I {
        &self.code[idx as usize]
    }
}


impl<I: Insn> Program<I> {
    pub fn get_function_info(&self, fptr: u32) -> FunctionInfo {
        self.fns[fptr as usize]
    }
}
