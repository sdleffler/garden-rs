use std::fmt::Debug;
use std::mem;

use tag::Tag;


#[cfg(target_endian = "little")]
pub fn f64_to_u32s(f: f64) -> [u32; 2] {
    unsafe { mem::transmute::<f64, [u32; 2]>(f) }
}


#[cfg(target_endian = "big")]
pub fn f64_to_u32s(f: f64) -> [u32; 2] {
    unsafe {
        let tmp = mem::transmute::<f64, [u32; 2]>(f);
        [tmp[1], tmp[0]]
    }
}


#[cfg(target_endian = "little")]
pub fn u32s_to_f64(a: [u32; 2]) -> f64 {
    unsafe { mem::transmute::<[u32; 2], f64>(a) }
}


#[cfg(target_endian = "big")]
pub fn u32s_to_f64(a: [u32; 2]) -> f64 {
    unsafe {
        let tmp = mem::transmute::<[u32; 2], f64>(a);
        [tmp[1], tmp[0]]
    }
}


pub type Function = u32;
pub type Signed = i32;
pub type Unsigned = u32;
pub type Atom = u32;
pub type Pointer = u32;
pub type Prompt = u32;


pub trait Word: Sized + Clone + Copy + Debug + PartialEq + Eq + Default {
    fn tag(&self) -> Tag;

    fn raw_u32(u32) -> Self;

    fn val_integer(Signed) -> Self;
    fn val_atom(Atom) -> Self;
    fn val_special(Special) -> Self;
    fn val_heap(Pointer) -> Self;
    fn val_list(Pointer) -> Self;
    fn val_float(Pointer) -> Self;
    fn val_boxed(Pointer) -> Self;
    fn val_function(u32) -> Self;

    fn hdr_raw(Unsigned) -> Self;
    fn hdr_float() -> Self;
    fn hdr_vector(Unsigned) -> Self;
    fn hdr_record(Atom) -> Self;
    fn hdr_closure(Unsigned) -> Self;
    fn hdr_continuation(u8, u8, bool, u16) -> Self;
    fn hdr_prompt() -> Self;
    fn hdr_moved(Pointer) -> Self;

    fn as_raw_u32(&self) -> Option<u32>;

    fn as_integer(&self) -> Option<Signed>;
    fn as_heap_ptr(&self) -> Option<Pointer>;
    fn as_list_ptr(&self) -> Option<Pointer>;
    fn as_pointer(&self) -> Option<Pointer>;
    fn as_special(&self) -> Option<Special>;
    fn as_boxed_ptr(&self) -> Option<Pointer>;
    fn as_function_ptr(&self) -> Option<Function>;

    fn as_boolean(&self) -> Option<bool> {
        match self.as_special() {
            Some(Special::True) => Some(true),
            Some(Special::False) => Some(false),
            _ => None,
        }
    }

    fn as_hdr_continuation(&self) -> Option<(u8, u8, bool, u16)>;

    fn val_boolean(b: bool) -> Self {
        if b {
            Self::val_special(Special::True)
        } else {
            Self::val_special(Special::False)
        }
    }

    fn is_nil(&self) -> bool {
        match self.as_special() {
            Some(Special::Nil) => true,
            _ => false,
        }
    }

    fn from_heap(ptr: Pointer) -> Self {
        Word::pack(&UnpackedWord::ValHeap(ptr))
    }


    fn from_list(ptr: Pointer) -> Self {
        Word::pack(&UnpackedWord::ValCons(ptr))
    }


    fn from_float(ptr: Pointer) -> Self {
        Word::pack(&UnpackedWord::ValFloat(ptr))
    }


    fn from_boxed(ptr: Pointer) -> Self {
        Word::pack(&UnpackedWord::ValBoxed(ptr))
    }


    fn unpack(&self) -> UnpackedWord;
    fn pack(&UnpackedWord) -> Self;
}


#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Primitive)]
pub enum Special {
    Nil   = 0b00,
    False = 0b01,
    True  = 0b11,
}


#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnpackedWord {
    ValInt(Signed),
    ValAtom(Atom),
    ValSpecial(Special),
    ValHeap(Pointer),
    ValCons(Pointer),
    ValFloat(Pointer),
    ValBoxed(Pointer),
    ValFunction(u32),
    HdrRaw(Unsigned),
    HdrFloat,
    HdrVector(Unsigned),
    HdrRecord(Atom),
    HdrClosure(Unsigned),
    HdrContinuation(u8, u8, bool, u16),
    HdrPrompt,
    HdrMoved(Pointer),

    RawWord(u32),
}


impl Default for UnpackedWord {
    fn default() -> UnpackedWord {
        UnpackedWord::ValSpecial(Special::Nil)
    }
}


impl Word for UnpackedWord {
    fn tag(&self) -> Tag {
        match *self {
            UnpackedWord::ValInt(..) => Tag::ValInt,
            UnpackedWord::ValAtom(..) => Tag::ValAtom,
            UnpackedWord::ValSpecial(..) => Tag::ValSpecial,
            UnpackedWord::ValHeap(..) => Tag::ValHeap,
            UnpackedWord::ValCons(..) => Tag::ValCons,
            UnpackedWord::ValFloat(..) => Tag::ValFloat,
            UnpackedWord::ValBoxed(..) => Tag::ValBoxed,
            UnpackedWord::ValFunction(..) => Tag::ValFunction,
            UnpackedWord::HdrRaw(..) => Tag::HdrRaw,
            UnpackedWord::HdrFloat => Tag::HdrFloat,
            UnpackedWord::HdrVector(..) => Tag::HdrVector,
            UnpackedWord::HdrRecord(..) => Tag::HdrRecord,
            UnpackedWord::HdrClosure(..) => Tag::HdrClosure,
            UnpackedWord::HdrContinuation(..) => Tag::HdrContinuation,
            UnpackedWord::HdrPrompt => Tag::HdrPrompt,
            UnpackedWord::HdrMoved(..) => Tag::HdrMoved,
            UnpackedWord::RawWord(..) => panic!("The tag of a raw word should never be inspected!"),
        }
    }

    fn raw_u32(u: u32) -> UnpackedWord { UnpackedWord::RawWord(u) }

    fn val_integer(i: Signed) -> UnpackedWord { UnpackedWord::ValInt(i) }
    fn val_atom(a: Atom) -> UnpackedWord { UnpackedWord::ValAtom(a) }
    fn val_special(s: Special) -> UnpackedWord { UnpackedWord::ValSpecial(s) }
    fn val_heap(p: Pointer) -> UnpackedWord { UnpackedWord::ValHeap(p) }
    fn val_list(p: Pointer) -> UnpackedWord { UnpackedWord::ValCons(p) }
    fn val_float(p: Pointer) -> UnpackedWord { UnpackedWord::ValFloat(p) }
    fn val_boxed(p: Pointer) -> UnpackedWord { UnpackedWord::ValBoxed(p) }
    fn val_function(a: u32) -> UnpackedWord { UnpackedWord::ValFunction(a) }

    fn hdr_raw(u: Unsigned) -> UnpackedWord { UnpackedWord::HdrRaw(u) }
    fn hdr_float() -> UnpackedWord { UnpackedWord::HdrFloat }
    fn hdr_vector(u: Unsigned) -> UnpackedWord { UnpackedWord::HdrVector(u) }
    fn hdr_record(a: Atom) -> UnpackedWord { UnpackedWord::HdrRecord(a) }
    fn hdr_closure(e: Unsigned) -> UnpackedWord { UnpackedWord::HdrClosure(e) }
    fn hdr_continuation(a: u8, r: u8, n: bool, l: u16) -> UnpackedWord { UnpackedWord::HdrContinuation(a, r, n, l) }
    fn hdr_prompt() -> UnpackedWord { UnpackedWord::HdrPrompt }
    fn hdr_moved(p: Pointer) -> UnpackedWord { UnpackedWord::HdrMoved(p) }


    fn as_raw_u32(&self) -> Option<u32> {
        use self::UnpackedWord::*;

        match *self {
            RawWord(u) => Some(u),
            _ => None,
        }
    }


    fn as_integer(&self) -> Option<Signed> {
        use self::UnpackedWord::*;

        match *self {
            ValInt(i) => Some(i),
            _ => None,
        }
    }


    fn as_heap_ptr(&self) -> Option<Pointer> {
        use self::UnpackedWord::*;

        match *self {
            ValHeap(p) => Some(p),
            _ => None,
        }
    }


    fn as_list_ptr(&self) -> Option<Pointer> {
        use self::UnpackedWord::*;

        match *self {
            ValCons(p) => Some(p),
            _ => None,
        }
    }


    fn as_pointer(&self) -> Option<Pointer> {
        use self::UnpackedWord::*;

        match *self {
            ValHeap(p) | ValCons(p) | ValFloat(p) |  ValBoxed(p) | HdrMoved(p) => {
                Some(p)
            }
            _ => None,
        }
    }


    fn as_special(&self) -> Option<Special> {
        use self::UnpackedWord::*;

        match *self {
            ValSpecial(s) => Some(s),
            _ => None,
        }
    }


    fn as_boxed_ptr(&self) -> Option<Pointer> {
        use self::UnpackedWord::*;

        match *self {
            ValBoxed(p) => Some(p),
            _ => None,
        }
    }


    fn as_function_ptr(&self) -> Option<Function> {
        use self::UnpackedWord::*;

        match *self {
            ValFunction(p) => Some(p),
            _ => None,
        }
    }


    fn as_hdr_continuation(&self) -> Option<(u8, u8, bool, u16)> {
        use self::UnpackedWord::*;

        match *self {
            HdrContinuation(a, r, n, l) => Some((a, r, n, l)),
            _ => None,
        }
    }

    fn unpack(&self) -> UnpackedWord { *self }
    fn pack(w: &UnpackedWord) -> Self { *w }
}


#[derive(Clone, Copy)]
pub struct MachineWord(u32);
