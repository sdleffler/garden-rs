use num_traits::FromPrimitive;

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Primitive)]
pub enum Tag {
    ValInt = 0b0000,
    ValAtom = 0b0001,
    ValSpecial = 0b0010,
    ValHeap = 0b0011,
    ValCons = 0b0100,
    ValFloat = 0b0101,
    ValBoxed = 0b0110,
    ValFunction = 0b0111,
    HdrRaw = 0b1000,
    HdrFloat = 0b1001,
    HdrVector = 0b1010,
    HdrRecord = 0b1011,
    HdrClosure = 0b1100,
    HdrContinuation = 0b1101,
    HdrPrompt = 0b1110,
    HdrMoved = 0b1111,
}

impl Tag {
    /// Check whether the tag is a value (equivalently, whether the fourth least
    /// significant bit is clear.)
    pub fn as_value(&self) -> Option<ValueTag> {
        ValueTag::from_u8(*self as u8)
    }

    /// Check whether the tag is a header (equivalently, whether the fourth
    /// least significant bit is clear.)
    pub fn as_header(&self) -> Option<HeaderTag> {
        HeaderTag::from_u8(*self as u8)
    }

    /// Returns true if `self` is `Tag::ValBoxed`.
    pub fn is_boxed(&self) -> bool {
        match *self {
            Tag::ValBoxed => true,
            _ => false,
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Primitive)]
pub enum ValueTag {
    ValInt = 0b0000,
    ValAtom = 0b0001,
    ValSpecial = 0b0010,
    ValHeap = 0b0011,
    ValCons = 0b0100,
    ValFloat = 0b0101,
    ValBoxed = 0b0110,
    ValFunction = 0b0111,
}

impl From<ValueTag> for Tag {
    fn from(vtag: ValueTag) -> Tag {
        Tag::from_u8(vtag as u8).expect("ValueTag is a strict subset of Tag!")
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Primitive)]
pub enum HeaderTag {
    HdrRaw          = 0b1000,
    HdrFloat        = 0b1001,
    HdrVector       = 0b1010,
    HdrRecord       = 0b1011,
    HdrClosure      = 0b1100,
    HdrContinuation = 0b1101,
    HdrPrompt       = 0b1110,
    HdrMoved        = 0b1111,
}

impl From<HeaderTag> for Tag {
    fn from(htag: HeaderTag) -> Tag {
        Tag::from_u8(htag as u8).expect("HeaderTag is a strict subset of Tag!")
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! vtag_to_tag {
        ($($tag:ident),*) => {
            {
                $(
                    {
                        assert_eq!(Tag::from(ValueTag::$tag), Tag::$tag);
                        assert_eq!(Tag::$tag.as_value(), Some(ValueTag::$tag));
                    }
                )*
            }
        };
    }

    macro_rules! htag_to_tag {
        ($($tag:ident),*) => {
            {
                $(
                    {
                        assert_eq!(Tag::from(HeaderTag::$tag), Tag::$tag);
                        assert_eq!(Tag::$tag.as_header(), Some(HeaderTag::$tag));
                    }
                )*
            }
        };
    }

    #[test]
    fn valuetag_to_tag() {
        vtag_to_tag!(
            ValInt,
            ValAtom,
            ValSpecial,
            ValHeap,
            ValCons,
            ValFloat,
            ValBoxed,
            ValFunction
        );
    }

    #[test]
    fn headertag_to_tag() {
        htag_to_tag!(
            HdrRaw,
            HdrFloat,
            HdrVector,
            HdrRecord,
            HdrClosure,
            HdrContinuation,
            HdrPrompt,
            HdrMoved
        );
    }
}
