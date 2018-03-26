
// Insn::CallObj(sp0) => match_sp0!(sp0; with self { src } => {
//     match src.tag() {
//         Tag::ValFunction => self.call_function(src.as_function_ptr().unwrap()),
//         Tag::ValHeap => {
//             let heap_ptr = src.as_heap_ptr().unwrap();

//             match self.heap[heap_ptr].tag() {
//                 Tag::HdrClosure => self.call_closure(heap_ptr),
//                 Tag::HdrContinuation => self.call_continuation(heap_ptr),
//                 heap_tag => Err(Error::Uncallable(heap_tag)),
//             }
//         }
//         tag => Err(Error::Uncallable(tag))
//     }
// }),
// Insn::CallStatic { addr } => self.call_function(addr),

// Insn::Return => {
//     // Pointers to the previous continuation (re: stack frame) are
//     // stored in the second slot of continuation objects.
//     let cont = match self.heap[self.fp + 2].as_heap_ptr() {
//         Some(p) => p,
//         None => return Err(Error::Uncallable(self.heap[self.fp + 2].tag())),
//     };
//     self.call_continuation(cont)
// }

// Insn::PromptObj { .. } => unimplemented!(),
// Insn::PromptStatic { .. } => unimplemented!(),

// Insn::Jmp(to) => {
//     self.pc = to;
//     Ok(())
// }
// Insn::JmpIf(sp0, to) => match_sp0!(sp0; with self { src } => {
//     match src.as_special() {
//         Some(Special::True) => {
//             self.pc = to;
//             Ok(())
//         }
//         Some(Special::False) => {
//             self.pc += 1;
//             Ok(())
//         }
//         _ => Err(Error::TypeError("boolean".to_string())),
//     }
// }),

// Insn::MatchList { data, on_nil } => {
//     match_sp2!(data; with self { src, set_car, set_cdr } =>
//         {
//             let obj = self.deref_if_boxed(src);

//             match obj.as_list_ptr() {
//                 Some(p) => {
//                     let car = self.heap[p];
//                     let cdr = self.heap[p + 1];

//                     set_car(self, car);
//                     set_cdr(self, cdr);

//                     self.pc += 1;

//                     Ok(())
//                 },
//                 None if obj.is_nil() => {
//                     self.pc = on_nil;

//                     Ok(())
//                 },
//                 _ => Err(Error::TypeError("list".to_string()))
//             }
//         }
//     )
// },

// Insn::Move1(op1) => match_op1_and_inc_pc!(op1; with self { src } => src),

// Insn::MoveR(mvr) => {
//     use self::MvR::*;

//     match mvr {
//         CR { src, dst, range } => {
//             for i in 0..range {
//                 self.xs[(dst + i) as usize] = src;
//             }
//         }
//         LR { src, dst, range } => {
//             for i in 0..range {
//                 self.xs[(dst + i) as usize] = self.heap[self.fp + 3 + src as u32 + i as u32];
//             }
//         }
//         CL { src, dst, range } => {
//             for i in 0..range {
//                 self.heap[self.fp + 3 + dst as u32 + i as u32] = src;
//             }
//         }
//         RL { src, dst, range } => {
//             for i in 0..range {
//                 self.heap[self.fp + 3 + dst as u32 + i as u32] = self.xs[(src + i) as usize];
//             }
//         }
//     }

//     self.pc += 1;

//     Ok(())
// }

// Insn::AllocBox(op1) => match_op1_and_inc_pc!(op1; with self { src } => {
//     self.alloc_boxed(src)?
// }),
// Insn::AllocCons(op2) => match_op2_and_inc_pc!(op2; with self { src0, src1 } => {
//     self.alloc_cons(src0, src1)?
// }),
// Insn::AllocFloat(op1) => match_op1_and_inc_pc!(op1; with self { src } => {
//     match src.as_integer() {
//         Some(i) => self.alloc_float(i as f64)?,
//         _ => return Err(Error::TypeError("integer".to_string())),
//     }
// }),
// Insn::AllocLVar(op0) => match_op0_and_inc_pc!(op0; with self { } => {
//     self.alloc_logical()?
// }),

// Insn::IntAdd(op2) => match_op2_and_inc_pc!(op2; with self { src0, src1 } => {
//     match (src0.as_integer(), src1.as_integer()) {
//         (Some(x), Some(y)) => W::val_integer(x + y),
//         _ => return Err(Error::TypeError("integer".to_string())),
//     } 
// }),
// Insn::IntDiv(op2) => match_op2_and_inc_pc!(op2; with self { src0, src1 } => {
//     match (src0.as_integer(), src1.as_integer()) {
//         (Some(x), Some(y)) => W::val_integer(x / y),
//         _ => return Err(Error::TypeError("integer".to_string())),
//     } 
// }),
// Insn::IntMul(op2) => match_op2_and_inc_pc!(op2; with self { src0, src1 } => {
//     match (src0.as_integer(), src1.as_integer()) {
//         (Some(x), Some(y)) => W::val_integer(x * y),
//         _ => return Err(Error::TypeError("integer".to_string())),
//     } 
// }),
// Insn::IntSub(op2) => match_op2_and_inc_pc!(op2; with self { src0, src1 } => {
//     match (src0.as_integer(), src1.as_integer()) {
//         (Some(x), Some(y)) => W::val_integer(x - y),
//         _ => return Err(Error::TypeError("integer".to_string())),
//     } 
// }),
// Insn::IntRem(op2) => match_op2_and_inc_pc!(op2; with self { src0, src1 } => {
//     match (src0.as_integer(), src1.as_integer()) {
//         (Some(x), Some(y)) => W::val_integer(x % y),
//         _ => return Err(Error::TypeError("integer".to_string())),
//     } 
// }),

// Insn::IntNeg(op1) => match_op1_and_inc_pc!(op1; with self { src } => {
//     match src.as_integer() {
//         Some(i) => W::val_integer(-i),
//         _ => return Err(Error::TypeError("integer".to_string())),
//     }
// }),

// Insn::CmpIntEq(op2) => match_op2_and_inc_pc!(op2; with self { src0, src1 } => {
//     match (src0.as_integer(), src1.as_integer()) {
//         (Some(x), Some(y)) => W::val_boolean(x == y),
//         _ => return Err(Error::TypeError("integer".to_string())),
//     }
// }),
// Insn::CmpIntGe(op2) => match_op2_and_inc_pc!(op2; with self { src0, src1 } => {
//     match (src0.as_integer(), src1.as_integer()) {
//         (Some(x), Some(y)) => W::val_boolean(x >= y),
//         _ => return Err(Error::TypeError("integer".to_string())),
//     }
// }),
// Insn::CmpIntGt(op2) => match_op2_and_inc_pc!(op2; with self { src0, src1 } => {
//     match (src0.as_integer(), src1.as_integer()) {
//         (Some(x), Some(y)) => W::val_boolean(x > y),
//         _ => return Err(Error::TypeError("integer".to_string())),
//     }
// }),
// Insn::CmpIntLe(op2) => match_op2_and_inc_pc!(op2; with self { src0, src1 } => {
//     match (src0.as_integer(), src1.as_integer()) {
//         (Some(x), Some(y)) => W::val_boolean(x <= y),
//         _ => return Err(Error::TypeError("integer".to_string())),
//     }
// }),
// Insn::CmpIntLt(op2) => match_op2_and_inc_pc!(op2; with self { src0, src1 } => {
//     match (src0.as_integer(), src1.as_integer()) {
//         (Some(x), Some(y)) => W::val_boolean(x < y),
//         _ => return Err(Error::TypeError("integer".to_string())),
//     }
// }),
// Insn::CmpIntNe(op2) => match_op2_and_inc_pc!(op2; with self { src0, src1 } => {
//     match (src0.as_integer(), src1.as_integer()) {
//         (Some(x), Some(y)) => W::val_boolean(x != y),
//         _ => return Err(Error::TypeError("integer".to_string())),
//     }
// }),

// Insn::BitAnd(op2) => match_op2_and_inc_pc!(op2; with self { src0, src1 } => {
//     match (src0.as_integer(), src1.as_integer()) {
//         (Some(x), Some(y)) => W::val_integer(x & y),
//         _ => return Err(Error::TypeError("integer".to_string())),
//     } 
// }),
// Insn::BitOr(op2) => match_op2_and_inc_pc!(op2; with self { src0, src1 } => {
//     match (src0.as_integer(), src1.as_integer()) {
//         (Some(x), Some(y)) => W::val_integer(x | y),
//         _ => return Err(Error::TypeError("integer".to_string())),
//     } 
// }),
// Insn::BitXor(op2) => match_op2_and_inc_pc!(op2; with self { src0, src1 } => {
//     match (src0.as_integer(), src1.as_integer()) {
//         (Some(x), Some(y)) => W::val_integer(x ^ y),
//         _ => return Err(Error::TypeError("integer".to_string())),
//     } 
// }),
// Insn::BitLsh(op2) => match_op2_and_inc_pc!(op2; with self { src0, src1 } => {
//     match (src0.as_integer(), src1.as_integer()) {
//         (Some(x), Some(y)) => W::val_integer(x << y),
//         _ => return Err(Error::TypeError("integer".to_string())),
//     } 
// }),
// Insn::BitRsh(op2) => match_op2_and_inc_pc!(op2; with self { src0, src1 } => {
//     match (src0.as_integer(), src1.as_integer()) {
//         (Some(x), Some(y)) => W::val_integer(x >> y),
//         _ => return Err(Error::TypeError("integer".to_string())),
//     } 
// }),

// Insn::BitNot(op1) => match_op1_and_inc_pc!(op1; with self { src } => {
//     match src.as_integer() {
//         Some(i) => W::val_integer(!i),
//         _ => return Err(Error::TypeError("integer".to_string())),
//     }
// }),

// Insn::BoolAnd(op2) => match_op2_and_inc_pc!(op2; with self { src0, src1 } => {
//     match (src0.as_boolean(), src1.as_boolean()) {
//         (Some(x), Some(y)) => W::val_boolean(x && y),
//         _ => return Err(Error::TypeError("boolean".to_string())),
//     } 
// }),
// Insn::BoolOr(op2) => match_op2_and_inc_pc!(op2; with self { src0, src1 } => {
//     match (src0.as_boolean(), src1.as_boolean()) {
//         (Some(x), Some(y)) => W::val_boolean(x || y),
//         _ => return Err(Error::TypeError("boolean".to_string())),
//     } 
// }),

// Insn::BoolNot(op1) => match_op1_and_inc_pc!(op1; with self { src } => {
//     match src.as_boolean() {
//         Some(i) => W::val_boolean(!i),
//         _ => return Err(Error::TypeError("boolean".to_string())),
//     }
// }),

// Insn::ListCar(op1) => match_op1_and_inc_pc!(op1; with self { src } => {
//     match src.as_list_ptr() {
//         Some(p) => self.heap[p],
//         _ => return Err(Error::TypeError("cons".to_string()))
//     } 
// }),
// Insn::ListCdr(op1) => match_op1_and_inc_pc!(op1; with self { src } => {
//     match src.as_list_ptr() {
//         Some(p) => self.heap[p + 1],
//         _ => return Err(Error::TypeError("cons".to_string()))
//     } 
// }),

// Insn::Panic => Err(Error::Panic),
