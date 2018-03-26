use std::fmt;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Index, IndexMut, Mul, Neg, Not, Rem, Shl, Shr, Sub};
use std::sync::{Arc, Mutex, Weak};

use insn::{Dst, FunctionInfo, Insn, Program, Src, SrcRange, Trg, UnpackedInsn};
use tag::Tag;
use word::{self, Atom, Function, Pointer, Prompt, Special, UnpackedWord, Unsigned, Word};

#[derive(Clone, Debug)]
pub enum Error {
    OutOfTime,
    OutOfMemory,
    TypeError(String),
    Uncallable(Tag),
    NotARecordDiscriminant(Atom),
    Panic,
}

pub type MachineResult<T> = Result<T, Error>;

pub enum Machine<I: Insn> {
    Running(Arc<Mutex<Running<I>>>),
    Collecting(Arc<Mutex<Collecting<I>>>),
    Crispy,
}

impl<I: Insn> Machine<I> {
    pub fn fry(&mut self) {
        *self = Machine::Crispy;
    }
}

pub struct Supervisor<I: Insn> {
    running: Vec<Weak<Mutex<Running<I>>>>,
    collecting: Vec<Weak<Mutex<Collecting<I>>>>,
    free: Vec<Space<I::Word>>,
    atoms: AtomTable,
}

impl<I: Insn> Supervisor<I> {
    pub fn new() -> Supervisor<I> {
        Supervisor {
            running: Vec::new(),
            collecting: Vec::new(),
            free: Vec::new(),
            atoms: Vec::new(),
        }
    }

    pub fn get_fresh_space(&mut self, length: Unsigned) -> Space<I::Word> {
        match self.free.binary_search_by_key(&length, Space::size) {
            Ok(idx) => self.free.remove(idx),
            Err(..) => Space(vec![I::Word::default(); length as usize]),
        }
    }

    pub fn reclaim_space(&mut self, space: Space<I::Word>) {
        match self.free.binary_search_by_key(&space.size(), Space::size) {
            Ok(idx) | Err(idx) => self.free.insert(idx, space),
        }
    }
}

pub type AtomTable = Vec<AtomEntry>;

pub struct Space<W: Word>(Vec<W>);

impl<W: Word> fmt::Debug for Space<W> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\n|------------------\n")?;
        for (addr, word) in self.0.iter().enumerate() {
            write!(f, "{:4} | {:?}\n", addr, word)?;
        }
        write!(f, "END: {} words", self.0.len())
    }
}

pub struct Delta<'a, W: Word + 'a> {
    from: &'a Space<W>,
    to: &'a Space<W>,
    scan: usize,
    alloc: usize,
}

impl<'a, W: Word> fmt::Debug for Delta<'a, W> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "\n-----|------------------------------------------|\
             ------------------------------------------|\n"
        )?;
        for (addr, (lword, rword)) in self.from.0.iter().zip(self.to.0.iter()).enumerate() {
            if addr == self.scan || addr == self.alloc {
                write!(
                    f,
                    "{0:4} | {1:<40} | {2:~<40} |",
                    addr,
                    format!("{:?}", lword),
                    format!("{:?}", rword)
                )?;
            } else {
                write!(
                    f,
                    "{0:4} | {1:<40} | {2:<40} |",
                    addr,
                    format!("{:?}", lword),
                    format!("{:?}", rword)
                )?;
            }

            if addr == self.scan {
                write!(f, " SCAN")?;
            }

            if addr == self.alloc {
                write!(f, " ALLOC")?;
            }

            write!(f, "\n")?;
        }
        write!(f, "END: {} words", self.from.0.len())
    }
}

impl<W: Word> Space<W> {
    pub fn size(&self) -> Unsigned {
        self.0.len() as Unsigned
    }
}

impl<W: Word> Index<Pointer> for Space<W> {
    type Output = W;

    fn index(&self, idx: Pointer) -> &W {
        &self.0[idx as usize]
    }
}

impl<W: Word> IndexMut<Pointer> for Space<W> {
    fn index_mut(&mut self, idx: Pointer) -> &mut W {
        &mut self.0[idx as usize]
    }
}

pub struct Running<I: Insn> {
    /// Reference to the running machine's supervisor.
    pub supervisor: Arc<Mutex<Supervisor<I>>>,

    /// Frame Pointer register.
    pub fp: Pointer,

    /// Program Counter register.
    pub pc: Pointer,

    /// Heap Pointer register.
    pub hp: Pointer,

    /// Cross-frame registers.
    pub xs: Vec<I::Word>,

    /// Code segment.
    pub code: Program<I>,

    /// The current heap.
    pub heap: Space<I::Word>,
}

impl<I: Insn> Running<I> {
    pub fn collect(self) -> MachineResult<Collecting<I>> {
        let mut supervisor = self.supervisor.lock().unwrap();

        let heap_size = self.heap.size();
        let to_space = supervisor.get_fresh_space(heap_size);

        let mut collecting = Collecting {
            supervisor: self.supervisor.clone(),
            pc: self.pc,
            code: self.code,
            xs: self.xs,
            alloc: 0,
            scan: 0,
            from: self.heap,
            to: to_space,
        };

        println!("Entering collection - blitting frame pointer.");

        // Blit the current stack frame over, thus making the new frame pointer
        // zero.
        collecting.blit_object(&supervisor.atoms, self.fp)?;

        println!("Entering collection - blitting registers.");

        // Blit objects pointed to by registers.
        for i in 0..collecting.xs.len() {
            let xi = collecting.xs[i];
            let copied = collecting.copy_value(&supervisor.atoms, xi)?;
            println!("Blitting xs[{}] - delta {:?} => {:?}", i, xi, copied);
            collecting.xs[i] = copied;
        }

        Ok(collecting)
    }

    pub fn heap_capacity(&self) -> Unsigned {
        self.heap.size() - self.hp
    }

    pub fn alloc_raw(&mut self, sz: Unsigned) -> MachineResult<I::Word> {
        if self.heap_capacity() >= sz {
            self.heap[self.hp] = I::Word::hdr_raw(sz);

            let addr = self.hp;
            self.hp += sz + 1;

            Ok(I::Word::val_heap(addr))
        } else {
            Err(Error::OutOfMemory)
        }
    }

    pub fn alloc_cons(&mut self, car: I::Word, cdr: I::Word) -> MachineResult<I::Word> {
        if self.heap_capacity() >= 2 {
            self.heap[self.hp + 0] = car;
            self.heap[self.hp + 1] = cdr;

            let addr = self.hp;
            self.hp += 2;

            Ok(I::Word::val_list(addr))
        } else {
            Err(Error::OutOfMemory)
        }
    }

    pub fn alloc_float(&mut self, f: f64) -> MachineResult<I::Word> {
        if self.heap_capacity() >= 3 {
            self.heap[self.hp + 0] = I::Word::hdr_float();

            let raws = word::f64_to_u32s(f);

            self.heap[self.hp + 1] = I::Word::raw_u32(raws[0]);
            self.heap[self.hp + 2] = I::Word::raw_u32(raws[1]);

            let addr = self.hp;
            self.hp += 3;

            Ok(I::Word::val_float(addr + 1))
        } else {
            Err(Error::OutOfMemory)
        }
    }

    pub fn alloc_logical(&mut self) -> MachineResult<I::Word> {
        if self.heap_capacity() >= 1 {
            let var = I::Word::val_boxed(self.hp);

            self.heap[self.hp] = var;
            self.hp += 1;

            Ok(var)
        } else {
            Err(Error::OutOfMemory)
        }
    }

    pub fn alloc_boxed(&mut self, val: I::Word) -> MachineResult<I::Word> {
        if self.heap_capacity() >= 1 {
            self.heap[self.hp] = val;

            let addr = self.hp;
            self.hp += 1;

            Ok(I::Word::val_boxed(addr))
        } else {
            Err(Error::OutOfMemory)
        }
    }

    pub fn alloc_prompt(
        &mut self,
        cont: Pointer,
        tag: I::Word,
        handler: I::Word,
    ) -> MachineResult<I::Word> {
        if self.heap_capacity() >= 4 {
            self.heap[self.hp + 0] = I::Word::hdr_prompt();
            self.heap[self.hp + 1] = tag;
            self.heap[self.hp + 2] = I::Word::val_heap(cont);
            self.heap[self.hp + 3] = handler;

            let addr = self.hp;
            self.hp += 4;

            Ok(I::Word::val_heap(addr))
        } else {
            Err(Error::OutOfMemory)
        }
    }

    pub fn alloc_closure(
        &mut self,
        fun: Function,
        env: SrcRange<I::Word>,
    ) -> MachineResult<I::Word> {
        use self::Src::*;

        let nenv = env.len as Unsigned;

        if self.heap_capacity() >= 2 + nenv {
            self.heap[self.hp + 0] = I::Word::hdr_closure(nenv);
            self.heap[self.hp + 1] = I::Word::raw_u32(fun);

            for i in 0..nenv {
                self.heap[self.hp + 2 + i] = match env.src {
                    C(cst) => cst,
                    L(ptr) => self.heap[ptr as u32 + i],
                    R(reg) => self.heap[reg as u32 + i],
                };
            }

            let addr = self.hp;
            self.hp += 2 + nenv;

            Ok(I::Word::val_heap(addr))
        } else {
            Err(Error::OutOfMemory)
        }
    }

    /// Allocate a "root continuation". This is a convenience function - it is
    /// intended for allocating a continuation such that a function can
    /// instantly be called when initializing the machine. The code should
    /// probably cause an `Error::Panic` - otherwise the machine will loop.
    pub fn initial_continuation(&mut self, code: u32) -> MachineResult<()> {
        let cont = self.alloc_continuation(code, 0, 0, 0, false, 0)?;
        self.heap[cont + 1] = I::Word::val_heap(cont);
        self.fp = cont;

        Ok(())
    }

    /// Allocate a continuation frame. Locals are left uninitialized.
    ///
    /// This function is safe to re-call after an out-of-memory interrupt, but
    /// is non-idempotent in general: if it errors, no modification to the heap
    /// is made.
    pub fn alloc_continuation(
        &mut self,
        code: u32,
        cont: Pointer,
        args: u8,
        rets: u8,
        no_clobber: bool,
        locals: u16,
    ) -> MachineResult<Pointer> {
        let nlocals = locals as Unsigned;

        if self.heap_capacity() >= 3 + nlocals {
            self.heap[self.hp + 0] = I::Word::hdr_continuation(args, rets, no_clobber, locals);
            self.heap[self.hp + 1] = I::Word::raw_u32(code);
            self.heap[self.hp + 2] = I::Word::val_heap(cont);

            let addr = self.hp;
            self.hp += 3 + nlocals;

            Ok(addr)
        } else {
            Err(Error::OutOfMemory)
        }
    }

    /// Allocate a copy of the given continuation. This entails, in full:
    /// - Box all locals of the continuation.
    /// - Allocate a new continuation.
    /// - Set the new continuation's header to be the same as the old, except
    ///   always with a clear no-clobber bit.
    ///
    /// This function is safe to re-call after an out-of-memory interrupt, but
    /// is decidedly non-idempotent: Suppose the function is called once. There
    /// are two possible locations to error and exit:
    /// - Suppose we error and exit while boxing locals of the continuation we
    ///   intend to copy. Boxing only occurs when a local is not already boxed,
    ///   so starting over, we won't accidentally re-box any variables. At worst
    ///   we will have to re-check whether the variables are boxed or not - no
    ///   unchecked modification to the heap occurs, and no extra allocations
    ///   are made.
    /// - Suppose we error and exit after discovering the heap does not hold
    ///   enough capacity for our new frame. Then, suppose we re-call. The heap
    ///   was left unmodified after the error, so the re-call leaves the heap's
    ///   state as it would be after a single call with no errors.
    pub fn alloc_continuation_copy(
        &mut self,
        cont: Pointer,
        args: u8,
        rets: u8,
        locals: u16,
    ) -> MachineResult<Pointer> {
        let nlocals = locals as Unsigned;

        for i in (0..nlocals).map(|n| n + 3) {
            let obj = self.heap[cont + i];

            if !obj.tag().is_boxed() {
                self.heap[cont + i] = self.alloc_boxed(obj)?;
            }
        }

        if self.heap_capacity() >= 3 + nlocals {
            self.heap[self.hp + 0] = I::Word::hdr_continuation(args, rets, false, locals);
            self.heap[self.hp + 1] = self.heap[cont + 1];
            self.heap[self.hp + 2] = self.heap[cont + 2];

            for i in (0..nlocals).map(|n| n + 3) {
                self.heap[self.hp + i] = self.heap[cont + i];
            }

            let addr = self.hp;
            self.hp += 3 + nlocals;

            Ok(addr)
        } else {
            Err(Error::OutOfMemory)
        }
    }

    /// If the current continuation is no-clobber, then we create a new frame
    /// copying the current continuation and set its instruction address to the
    /// current value of the Program Counter. If the continuation is clobberable,
    /// then we skip straight to setting its instruction address to the current
    /// value of the Program Counter.
    ///
    /// This function is safe to re-call: it is completely idempotent. After
    /// being called once, if called again, the current continuation will already
    /// be marked no-clobber. As a result, the only action it will take will be
    /// to set the code slot to the current value of the Program Counter. But we
    /// have already called it once - which means the code slot is already set
    /// to the current value of the Program Counter. Thus, it is effectively a
    /// no-op.
    pub fn guard_current_continuation_and_push_incremented_pc(&mut self) -> MachineResult<()> {
        let (nargs, nrets, no_clobber, nlocals) = self.heap[self.fp].as_hdr_continuation().expect(
            "Expected a continuation to guard, but pointer did not point \
             to an hdr_continuation!",
        );

        if no_clobber {
            // If the continuation is no-clobber, then we need to allocate a new
            // copy.

            let old_fp = self.fp;
            self.fp = self.alloc_continuation_copy(old_fp, nargs, nrets, nlocals)?;
        }

        // If the continuation isn't no-clobber, then we need not make a copy.

        // Push the Program Counter into the code slot of the continuation at
        // the frame pointer.
        self.heap[self.fp + 1] = I::Word::raw_u32(self.pc + 1);

        Ok(())
    }

    /// Call a static function. If there is no memory available to allocate the
    /// new stack frame, we fail with an `Error::OutOfMemory`. `call_function`
    /// is *safe* to immediately re-call after an `Error::OutOfMemory` and a GC.
    pub fn call_function(&mut self, fptr: Function) -> MachineResult<()> {
        let FunctionInfo {
            addr,
            args,
            rets,
            locals,
        } = self.code.get_function_info(fptr);

        // `guard_current_continuation_and_push_pc` is safe to re-call.
        self.guard_current_continuation_and_push_incremented_pc()?;

        let old_frame = self.fp;

        // `alloc_continuation` is safe to re-call.
        let new_frame = self.alloc_continuation(addr, old_frame, args, rets, false, locals)?;

        self.fp = new_frame;
        self.pc = addr;

        Ok(())
    }

    // /// Given a pointer to a closure object on the heap, splat the closure in
    // /// continuation form and perform all necessary guarding operations on the
    // /// current continuation.
    // ///
    // /// This function is non-idempotent but safe to call after an error:
    // /// - Suppose we error when checking space for the new continuation frame.
    // ///   Then, nothing happens to the heap. On re-call we (hopefully) pass the
    // ///   guard, at which point it is as if a single call has occurred without
    // ///   error.
    // /// - Suppose `guard_current_continuation_and_push_pc` errors. Then, up to
    // ///   there, nothing permanent has occurred on the heap, and `guard...` is
    // ///   safe to re-call; thus, we are still safe to re-call.
    // pub fn call_closure(&mut self, closure: Pointer) -> MachineResult<()> {
    //     let (args, rets, locals) = self.heap[closure]
    //         .as_hdr_closure()
    //         .expect("Expected a closure to splat, but pointer did not point \
    //                  to a hdr_closure!");

    //     let nlocals = locals as Unsigned;

    //     // Splat the closure onto the heap as a continuation.
    //     if self.heap_capacity() >= nlocals + 3 {
    //         self.guard_current_continuation_and_push_incremented_pc()?;

    //         let code = self.heap[closure + 1]
    //             .as_raw_u32()
    //             .expect("Expected closure object to be followed by a raw code \
    //                      address (as raw u32), but value was not a raw u32!");

    //         self.heap[self.hp + 0] = I::Word::hdr_continuation(args, rets, false, locals);
    //         self.heap[self.hp + 1] = I::Word::raw_u32(code);
    //         self.heap[self.hp + 2] = I::Word::val_heap(self.fp);

    //         for i in 0..nlocals {
    //             self.heap[self.hp + i + 3] = self.heap[closure + i + 2];
    //         }

    //         self.fp = self.hp;
    //         self.pc = code;

    //         self.hp += 3 + nlocals;

    //         Ok(())
    //     } else {
    //         Err(Error::OutOfMemory)
    //     }
    // }

    /// Call a continuation, either splatting a copy if the no-clobber bit is
    /// set or directly clobbering the continuation.
    ///
    /// This function is safe to re-call but not idempotent:
    /// - `guard_current_continuation_and_push_pc` is re-call safe, and no heap
    ///   modification occurs prior, thus after an error at that point we are
    ///   still safe to re-call.
    /// - Suppose the no-clobber bit is clear. Then, there is no way to error.
    /// - Suppose the no-clobber bit is set. Then, if we error at the heap capacity
    ///   check, we are still re-callable: up to just after the heap capacity
    ///   check we are still idempotent under a re-call.
    pub fn call_continuation(&mut self, cont: Pointer) -> MachineResult<()> {
        let (args, rets, no_clobber, locals) = self.heap[cont].as_hdr_continuation().expect(
            "Expected a continuation to call, but pointer did not point \
             to a hdr_continuation!",
        );

        self.guard_current_continuation_and_push_incremented_pc()?;

        if no_clobber {
            // If the continuation is no-clobber, then we have to splat a copy
            // onto the heap similar to a closure call. The new copy is, of
            // course, clobberable.

            let nlocals = locals as Unsigned;

            if self.heap_capacity() >= nlocals + 3 {
                self.heap[self.hp + 0] = I::Word::hdr_continuation(args, rets, false, locals);

                for i in 1..(nlocals + 3) {
                    self.heap[self.hp + i] = self.heap[cont + i];
                }

                self.fp = self.hp;

                self.hp += 3 + nlocals;
            } else {
                return Err(Error::OutOfMemory);
            }
        } else {
            // If we can clobber the continuation, then execution is as simple
            // as setting the Program Counter and Frame Pointer.
            self.fp = cont;
        }

        let code = self.heap[cont + 1].as_raw_u32().expect(
            "Expected continuation object to be followed by a raw code \
             address (as raw u32), but value was not a raw u32!",
        );

        self.pc = code;

        Ok(())
    }

    pub fn call_object(&mut self, ptr: Pointer) -> MachineResult<()> {
        use self::UnpackedWord::*;

        self.guard_current_continuation_and_push_incremented_pc()?;

        match self.heap[ptr].unpack() {
            HdrClosure(env) => {
                let fptr = self.heap[ptr + 1].as_raw_u32().unwrap();
                let FunctionInfo {
                    addr,
                    args,
                    rets,
                    locals,
                } = self.code.get_function_info(fptr);

                let nlocals = locals as Unsigned;

                // Splat the closure onto the heap as a continuation.
                // This entails a few things:
                // - Splat an empty continuation with uninitialized locals onto
                //   the heap.
                // - Splat captured environment into cross-frame registers. Each
                //   captured environment value in a given index `i` goes into
                //   the respective cross-frame register.
                if self.heap_capacity() >= nlocals + 3 {
                    self.heap[self.hp + 0] = I::Word::hdr_continuation(args, rets, false, locals);
                    // self.heap[self.hp + 1] = I::Word::raw_u32(addr); // Unnecessary? Always set by guard
                    self.heap[self.hp + 2] = I::Word::val_heap(self.fp);

                    self.fp = self.hp;
                    self.pc = addr;

                    self.hp += 3 + nlocals;

                    for i in 0..env {
                        self.xs[i as usize] = self.heap[ptr + 2 + i];
                    }

                    Ok(())
                } else {
                    Err(Error::OutOfMemory)
                }
            }
            HdrContinuation(args, rets, no_clobber, locals) => {
                if no_clobber {
                    // If the continuation is no-clobber, then we have to splat a copy
                    // onto the heap similar to a closure call. The new copy is, of
                    // course, clobberable.

                    let nlocals = locals as Unsigned;

                    if self.heap_capacity() >= nlocals + 3 {
                        self.heap[self.hp + 0] =
                            I::Word::hdr_continuation(args, rets, false, locals);

                        for i in 1..(nlocals + 3) {
                            self.heap[self.hp + i] = self.heap[ptr + i];
                        }

                        self.fp = self.hp;

                        self.hp += 3 + nlocals;
                    } else {
                        return Err(Error::OutOfMemory);
                    }
                } else {
                    // If we can clobber the continuation, then execution is as simple
                    // as setting the Program Counter and Frame Pointer.
                    self.fp = ptr;
                }

                let code = self.heap[ptr + 1].as_raw_u32().expect(
                    "Expected continuation object to be followed by a raw code \
                     address (as raw u32), but value was not a raw u32!",
                );

                self.pc = code;

                Ok(())
            }
            obj => Err(Error::Uncallable(obj.tag())),
        }
    }

    pub fn call_value(&mut self, val: I::Word) -> MachineResult<()> {
        use self::UnpackedWord::*;

        match val.unpack() {
            ValHeap(p) => self.call_object(p),
            ValFunction(fptr) => self.call_function(fptr),
            other => Err(Error::Uncallable(other.tag())),
        }
    }

    pub fn call_with_prompt(
        &mut self,
        thunk: I::Word,
        tag: I::Word,
        handler: I::Word,
    ) -> MachineResult<()> {
        let ret_fp = self.fp;

        self.call_value(thunk)?;

        self.heap[self.fp + 2] = self.alloc_prompt(ret_fp, tag, handler)?;

        Ok(())
    }

    pub fn deref_if_boxed(&self, obj: I::Word) -> I::Word {
        match obj.tag() {
            Tag::ValBoxed => {
                let val = self.heap[obj.as_boxed_ptr().unwrap()];
                self.deref_if_boxed(val)
            }
            _ => obj,
        }
    }

    pub fn get_local(&self, local: u16) -> I::Word {
        let tmp = self.heap[self.fp + 3 + local as Unsigned];
        self.deref_if_boxed(tmp)
    }

    pub fn set_local(&mut self, local: u16, val: I::Word) {
        self.heap[self.fp + 3 + local as Unsigned] = val;
    }

    pub fn get_register(&self, reg: u8) -> I::Word {
        self.xs[reg as usize]
    }

    pub fn set_register(&mut self, reg: u8, val: I::Word) {
        self.xs[reg as usize] = val;
    }

    pub fn get_src(&mut self, src: Src<I::Word>) -> I::Word {
        match src {
            Src::C(c) => c,
            Src::L(l) => self.get_local(l),
            Src::R(r) => self.get_register(r),
        }
    }

    pub fn set_dst(&mut self, dst: Dst, val: I::Word) {
        match dst {
            Dst::L(l) => self.set_local(l, val),
            Dst::R(r) => self.set_register(r, val),
        }
    }

    pub fn update_pc(&mut self, trg: Trg) {
        match trg {
            Trg::Absolute(addr) => self.pc = addr,
            Trg::Relative(offs) => self.pc = (self.pc as i32 + offs) as u32,
        }
    }

    pub fn exec_integer_unop<F: Fn(i32) -> i32>(
        &mut self,
        src: Src<I::Word>,
        dst: Dst,
        f: F,
    ) -> MachineResult<()> {
        use self::UnpackedWord::*;

        let val = self.get_src(src);

        let i = match val.unpack() {
            ValInt(i) => Ok(i),
            _ => Err(Error::TypeError(format!(
                "Expected integer, found {:?}",
                val.tag()
            ))),
        }?;

        self.set_dst(dst, I::Word::val_integer(f(i)));

        self.pc += 1;

        Ok(())
    }

    pub fn exec_boolean_unop<F: Fn(bool) -> bool>(
        &mut self,
        src: Src<I::Word>,
        dst: Dst,
        f: F,
    ) -> MachineResult<()> {
        let val = self.get_src(src);

        let b = match val.as_boolean() {
            Some(b) => Ok(b),
            _ => Err(Error::TypeError(format!(
                "Expected boolean, found {:?}",
                val.tag()
            ))),
        }?;

        self.set_dst(dst, I::Word::val_boolean(f(b)));

        self.pc += 1;

        Ok(())
    }

    pub fn exec_integer_binop<F: Fn(i32, i32) -> i32>(
        &mut self,
        src0: Src<I::Word>,
        src1: Src<I::Word>,
        dst: Dst,
        f: F,
    ) -> MachineResult<()> {
        use self::UnpackedWord::*;

        let val0 = self.get_src(src0);
        let val1 = self.get_src(src1);

        let (i0, i1) = match (val0.unpack(), val1.unpack()) {
            (ValInt(i0), ValInt(i1)) => Ok((i0, i1)),
            _ => Err(Error::TypeError(format!(
                "Expected (integer, integer), found ({:?}, {:?})",
                val0.tag(),
                val1.tag()
            ))),
        }?;

        self.set_dst(dst, I::Word::val_integer(f(i0, i1)));

        self.pc += 1;

        Ok(())
    }

    pub fn exec_boolean_binop<F: Fn(bool, bool) -> bool>(
        &mut self,
        src0: Src<I::Word>,
        src1: Src<I::Word>,
        dst: Dst,
        f: F,
    ) -> MachineResult<()> {
        let val0 = self.get_src(src0);
        let val1 = self.get_src(src1);

        let (b0, b1) = match (val0.as_boolean(), val1.as_boolean()) {
            (Some(b0), Some(b1)) => Ok((b0, b1)),
            _ => Err(Error::TypeError(format!(
                "Expected (boolean, boolean), found ({:?}, {:?})",
                val0.tag(),
                val1.tag()
            ))),
        }?;

        self.set_dst(dst, I::Word::val_boolean(f(b0, b1)));

        self.pc += 1;

        Ok(())
    }

    pub fn exec_integer_binary_predicate<F: Fn(i32, i32) -> bool>(
        &mut self,
        src0: Src<I::Word>,
        src1: Src<I::Word>,
        dst: Dst,
        f: F,
    ) -> MachineResult<()> {
        use self::UnpackedWord::*;

        let val0 = self.get_src(src0);
        let val1 = self.get_src(src1);

        let (i0, i1) = match (val0.unpack(), val1.unpack()) {
            (ValInt(i0), ValInt(i1)) => Ok((i0, i1)),
            _ => Err(Error::TypeError(format!(
                "Expected (integer, integer), found ({:?}, {:?})",
                val0.tag(),
                val1.tag()
            ))),
        }?;

        self.set_dst(dst, I::Word::val_boolean(f(i0, i1)));

        self.pc += 1;

        Ok(())
    }

    pub fn step_once(&mut self) -> MachineResult<()> {
        use self::UnpackedInsn::*;
        use self::UnpackedWord::*;

        // We dispatch on the instruction pointed to by the Program Counter.
        match self.code[self.pc].unpack() {
            Call(src) => {
                let val = self.get_src(src);
                self.call_value(val)
            }

            Return => {
                let cont = self.heap[self.fp + 2].as_pointer().unwrap();
                self.call_continuation(cont)
            }

            Prompt {
                thunk,
                tag,
                handler,
            } => {
                let thunk_val = self.get_src(thunk);
                let tag_val = self.get_src(tag);
                let handler_val = self.get_src(handler);
                self.call_with_prompt(thunk_val, tag_val, handler_val)
            }

            Jmp(addr) => {
                self.update_pc(addr);
                Ok(())
            }

            JmpIf(src, addr) => match self.get_src(src).unpack() {
                ValSpecial(Special::True) => {
                    self.update_pc(addr);
                    Ok(())
                }
                ValSpecial(Special::False) => {
                    self.pc += 1;
                    Ok(())
                }
                other => Err(Error::TypeError(format!(
                    "Expected a boolean, got {:?}",
                    other
                ))),
            },

            MatchList {
                src,
                dst_car,
                dst_cdr,
                on_nil,
            } => match self.get_src(src).unpack() {
                ValSpecial(Special::Nil) => {
                    self.update_pc(on_nil);
                    Ok(())
                }
                ValCons(p) => {
                    let (car, cdr) = (self.heap[p], self.heap[p + 1]);
                    self.set_dst(dst_car, car);
                    self.set_dst(dst_cdr, cdr);

                    self.pc += 1;
                    Ok(())
                }
                other => Err(Error::TypeError(format!(
                    "Expected nil or a list, got {:?}",
                    other
                ))),
            },

            Panic => Err(Error::Panic),
            Collect => {
                self.pc += 1;
                Err(Error::OutOfMemory)
            }

            Move1 { src, dst } => {
                let val = self.get_src(src);
                self.set_dst(dst, val);
                self.pc += 1;
                Ok(())
            }

            AllocClosure {
                src_fn,
                src_locals,
                dst,
            } => {
                let res = self.alloc_closure(src_fn, src_locals)?;
                self.set_dst(dst, res);
                Ok(())
            }

            AllocCons {
                src_car,
                src_cdr,
                dst,
            } => {
                let car = self.get_src(src_car);
                let cdr = self.get_src(src_cdr);

                let cons = self.alloc_cons(car, cdr)?;

                self.set_dst(dst, cons);
                self.pc += 1;

                Ok(())
            }

            AllocLVar(dst) => {
                let lvar = self.alloc_logical()?;
                self.set_dst(dst, lvar);
                self.pc += 1;
                Ok(())
            }

            IntAdd { src0, src1, dst } => self.exec_integer_binop(src0, src1, dst, Add::add),
            IntDiv { src0, src1, dst } => self.exec_integer_binop(src0, src1, dst, Div::div),
            IntMod { src0, src1, dst } => self.exec_integer_binop(src0, src1, dst, Rem::rem),
            IntMul { src0, src1, dst } => self.exec_integer_binop(src0, src1, dst, Mul::mul),
            IntSub { src0, src1, dst } => self.exec_integer_binop(src0, src1, dst, Sub::sub),

            IntNeg { src, dst } => self.exec_integer_unop(src, dst, Neg::neg),

            CmpIntEq { src0, src1, dst } => {
                self.exec_integer_binary_predicate(src0, src1, dst, |i0, i1| i0 == i1)
            }
            CmpIntGe { src0, src1, dst } => {
                self.exec_integer_binary_predicate(src0, src1, dst, |i0, i1| i0 >= i1)
            }
            CmpIntGt { src0, src1, dst } => {
                self.exec_integer_binary_predicate(src0, src1, dst, |i0, i1| i0 > i1)
            }
            CmpIntLe { src0, src1, dst } => {
                self.exec_integer_binary_predicate(src0, src1, dst, |i0, i1| i0 <= i1)
            }
            CmpIntLt { src0, src1, dst } => {
                self.exec_integer_binary_predicate(src0, src1, dst, |i0, i1| i0 < i1)
            }
            CmpIntNe { src0, src1, dst } => {
                self.exec_integer_binary_predicate(src0, src1, dst, |i0, i1| i0 != i1)
            }

            BitAnd { src0, src1, dst } => {
                self.exec_integer_binop(src0, src1, dst, ::std::ops::BitAnd::bitand)
            }
            BitOr { src0, src1, dst } => {
                self.exec_integer_binop(src0, src1, dst, ::std::ops::BitOr::bitor)
            }
            BitXor { src0, src1, dst } => {
                self.exec_integer_binop(src0, src1, dst, ::std::ops::BitXor::bitxor)
            }
            BitLsh { src0, src1, dst } => self.exec_integer_binop(src0, src1, dst, Shl::shl),
            BitRsh { src0, src1, dst } => self.exec_integer_binop(src0, src1, dst, Shr::shr),

            BitNot { src, dst } => self.exec_integer_unop(src, dst, Not::not),

            BoolAnd { src0, src1, dst } => {
                self.exec_boolean_binop(src0, src1, dst, |b0, b1| b0 && b1)
            }
            BoolOr { src0, src1, dst } => {
                self.exec_boolean_binop(src0, src1, dst, |b0, b1| b0 || b1)
            }

            BoolNot { src, dst } => self.exec_boolean_unop(src, dst, Not::not),

            ListCar { src, dst } => {
                let obj = self.get_src(src);

                let res = match obj.unpack() {
                    ValCons(p) => Ok(self.heap[p]),
                    _ => Err(Error::TypeError(format!(
                        "Expected cons-pointer, found {:?}",
                        obj.tag()
                    ))),
                }?;

                self.set_dst(dst, res);
                self.pc += 1;

                Ok(())
            }

            ListCdr { src, dst } => {
                let obj = self.get_src(src);

                let res = match obj.unpack() {
                    ValCons(p) => Ok(self.heap[p + 1]),
                    _ => Err(Error::TypeError(format!(
                        "Expected cons-pointer, found {:?}",
                        obj.tag()
                    ))),
                }?;

                self.set_dst(dst, res);
                self.pc += 1;

                Ok(())
            }
        }
    }

    pub fn debug_current_frame(&self) -> String {
        let (args, rets, no_clobber, locals) = self.heap[self.fp].as_hdr_continuation().unwrap();
        format!(
            "[#args {}, #rets {}, no-clobber? {}, #locals {}, code @{:?}, \
             cont ~{:?} | {:?}]",
            args,
            rets,
            no_clobber,
            locals,
            self.heap[self.fp + 1],
            self.heap[self.fp + 2],
            &self.heap.0[((self.fp + 3) as usize)..((self.fp + 3) as usize + locals as usize)]
        )
    }

    /// TODO: Debug for non integer/cons/special.
    pub fn debug_value(&self, val: I::Word) -> String {
        match val.tag() {
            Tag::ValInt => format!("{}", val.as_integer().unwrap()),
            Tag::ValCons => {
                let ptr = val.as_list_ptr().unwrap();
                let car = self.heap[ptr];
                let cdr = self.heap[ptr + 1];
                format!("({} . {})", self.debug_value(car), self.debug_value(cdr))
            }
            Tag::ValSpecial => match val.as_special().unwrap() {
                Special::True => "#t".to_string(),
                Special::False => "#f".to_string(),
                Special::Nil => "()".to_string(),
            },
            _ => format!("{:?}", val),
        }
    }
}

pub enum AtomEntry {
    Empty,
    Record(Unsigned),
}

pub struct Collecting<I: Insn> {
    /// Reference to the collecting machine's supervisor.
    pub supervisor: Arc<Mutex<Supervisor<I>>>,

    /// Cached Program Counter.
    pub pc: Pointer,

    /// The code segment.
    pub code: Program<I>,

    /// Cross-frame registers.
    pub xs: Vec<I::Word>,

    /// The allocation pointer. After collection is done, this becomes the Heap
    /// Pointer.
    pub alloc: Pointer,

    /// The scan pointer. This does not correspond to any register.
    pub scan: Pointer,

    /// The from-space - the old heap.
    pub from: Space<I::Word>,

    /// The to-space - the new heap.
    pub to: Space<I::Word>,
}

const CLOS_DATA_OFFSET: Unsigned = 2;
const CONT_DATA_OFFSET: Unsigned = 2;
const PRMT_DATA_OFFSET: Unsigned = 4;

const HEAP_SINGLE_SZ: Unsigned = 1;

const HFLT_REGION_SZ: Unsigned = 3;
const PRMT_REGION_SZ: Unsigned = 4;

const HRAW_PREFIX_SZ: Unsigned = 1;
const VECT_PREFIX_SZ: Unsigned = 1;
const RCRD_PREFIX_SZ: Unsigned = 1;
const CLOS_PREFIX_SZ: Unsigned = 2;
const CONT_PREFIX_SZ: Unsigned = 3;

impl<I: Insn> Collecting<I> {
    pub fn run(self) -> Running<I> {
        self.supervisor.lock().unwrap().reclaim_space(self.from);

        Running {
            supervisor: self.supervisor,
            fp: 0,
            pc: self.pc,
            hp: self.alloc,
            heap: self.to,
            xs: self.xs,
            code: self.code,
        }
    }

    pub fn blit_range(&mut self, loc: Pointer, sz: Unsigned) -> MachineResult<Pointer> {
        if self.alloc + sz < self.to.size() {
            for i in 0..sz {
                #[cfg(mutau_debug_gc)]
                println!("Blit: to[{}] = from[{}]", self.alloc + i, loc + i);
                self.to[self.alloc + i] = self.from[loc + i];
            }
            // This must happen *after* blitting - otherwise `HdrMoved` will be
            // copied into to-space.
            self.from[loc] = Word::pack(&UnpackedWord::HdrMoved(self.alloc));
            #[cfg(mutau_debug_gc)]
            println!("Blit: from[{}..{}] moved to #{}", loc, loc + sz, self.alloc);
            let ptr = self.alloc;
            self.alloc += sz;
            Ok(ptr)
        } else {
            Err(Error::OutOfMemory)
        }
    }

    pub fn blit_object(&mut self, atoms: &AtomTable, fr_ptr: Pointer) -> MachineResult<Pointer> {
        use self::UnpackedWord::*;

        let obj = self.from[fr_ptr];

        match obj.unpack() {
            ValInt(..) | ValAtom(..) | ValSpecial(..) | ValHeap(..) | ValCons(..)
            | ValFloat(..) | ValBoxed(..) | ValFunction(..) => {
                self.blit_range(fr_ptr, HEAP_SINGLE_SZ)
            }

            HdrRaw(sz) => self.blit_range(fr_ptr, sz + HRAW_PREFIX_SZ),
            HdrFloat => self.blit_range(fr_ptr, HFLT_REGION_SZ),
            HdrVector(sz) => self.blit_range(fr_ptr, sz + VECT_PREFIX_SZ),
            HdrRecord(at) => match atoms[at as usize] {
                AtomEntry::Empty => Err(Error::NotARecordDiscriminant(at)),
                AtomEntry::Record(sz) => self.blit_range(fr_ptr, sz + RCRD_PREFIX_SZ),
            },
            HdrClosure(env) => self.blit_range(fr_ptr, env + CLOS_PREFIX_SZ),
            HdrContinuation(_args, _rets, _no_clobber, locals) => {
                self.blit_range(fr_ptr, locals as Unsigned + CONT_PREFIX_SZ)
            }
            HdrPrompt => self.blit_range(fr_ptr, PRMT_REGION_SZ),
            HdrMoved(to_ptr) => Ok(to_ptr),

            RawWord(raw) => panic!("Cannot extract object from raw word {} at {}!", raw, fr_ptr),
        }
    }

    pub fn copy_value(&mut self, atoms: &AtomTable, value: I::Word) -> MachineResult<I::Word> {
        use self::UnpackedWord::*;

        let res = match value.unpack() {
            ValInt(..) | ValAtom(..) | ValSpecial(..) | ValFunction(..) => value,
            ValHeap(fr_ptr) => Word::from_heap(self.blit_object(atoms, fr_ptr)?),
            ValCons(fr_ptr) => Word::from_list(self.blit_range(fr_ptr, 2)?),
            ValFloat(fr_ptr) => Word::from_float(self.blit_range(fr_ptr, 3)?),
            ValBoxed(fr_ptr) => Word::from_boxed(self.blit_object(atoms, fr_ptr)?),
            _ => panic!("Expected a value, not a heap header/raw u32 ({:?})!", value),
        };

        Ok(res)
    }

    pub fn scan_value(&mut self, atoms: &AtomTable) -> MachineResult<()> {
        #[cfg(mutau_debug_gc)]
        println!("Scanning value &{} == {:?}", self.scan, self.to[self.scan]);
        let val = self.to[self.scan];
        let new_val = self.copy_value(atoms, val)?;
        self.to[self.scan] = new_val;
        self.scan += 1;
        Ok(())
    }

    pub fn scan_object(&mut self, atoms: &AtomTable) -> MachineResult<()> {
        use self::UnpackedWord::*;

        #[cfg(mutau_debug_gc)]
        println!("Scanning - {} / {}", self.scan, self.alloc);

        let obj = self.to[self.scan];

        match obj.unpack() {
            ValInt(..) | ValAtom(..) | ValSpecial(..) | ValFunction(..) => {
                self.scan += 1;
                Ok(())
            }
            ValHeap(fr_ptr) => {
                self.to[self.scan] = Word::from_heap(self.blit_object(atoms, fr_ptr)?);
                self.scan += 1;
                Ok(())
            }
            ValCons(fr_ptr) => {
                self.to[self.scan] = Word::from_list(self.blit_range(fr_ptr, 2)?);
                self.scan += 1;
                Ok(())
            }
            ValFloat(fr_ptr) => {
                self.to[self.scan] = Word::from_float(self.blit_range(fr_ptr, 3)?);
                self.scan += 1;
                Ok(())
            }
            ValBoxed(fr_ptr) => {
                self.to[self.scan] = Word::from_boxed(self.blit_object(atoms, fr_ptr)?);
                self.scan += 1;
                Ok(())
            }

            HdrRaw(sz) => {
                self.scan += HRAW_PREFIX_SZ + sz;
                Ok(())
            }
            HdrFloat => {
                self.scan += HFLT_REGION_SZ;
                Ok(())
            }
            HdrVector(sz) => {
                #[cfg(mutau_debug_gc)]
                println!("Beginning inner scan - vector ({} values)", sz);
                self.scan += VECT_PREFIX_SZ;
                for _ in 0..sz {
                    self.scan_value(atoms)?;
                }
                Ok(())
            }
            HdrRecord(at) => match atoms[at as usize] {
                AtomEntry::Empty => Err(Error::NotARecordDiscriminant(at)),
                AtomEntry::Record(sz) => {
                    #[cfg(mutau_debug_gc)]
                    println!("Beginning inner scan - record ({} fields)", sz);
                    self.scan += RCRD_PREFIX_SZ;
                    for _ in 0..sz {
                        self.scan_value(atoms)?;
                    }
                    Ok(())
                }
            },
            HdrClosure(env) => {
                #[cfg(mutau_debug_gc)]
                println!(
                    "Beginning inner scan - closure ({} captured environment values)",
                    env
                );
                self.scan += CLOS_DATA_OFFSET;
                for _ in 0..env {
                    self.scan_value(atoms)?;
                }
                Ok(())
            }
            HdrContinuation(_args, _rets, _no_clobber, locals) => {
                #[cfg(mutau_debug_gc)]
                println!("Beginning inner scan - continuation ({} locals)", locals);
                self.scan += CONT_DATA_OFFSET;
                for _ in 0..(locals as u32 + (CONT_PREFIX_SZ - CONT_DATA_OFFSET)) {
                    self.scan_value(atoms)?;
                }
                Ok(())
            }
            HdrPrompt => {
                #[cfg(mutau_debug_gc)]
                println!("Beginning inner scan - prompt");
                self.scan += PRMT_DATA_OFFSET;
                for _ in 0..(PRMT_REGION_SZ - PRMT_DATA_OFFSET) {
                    self.scan_value(atoms)?;
                }
                Ok(())
            }
            HdrMoved(..) => unreachable!(), // Will never find a moved pointer in to-space.
            RawWord(..) => unreachable!(), // Will never find a raw word in a scannable location in to-space.
        }
    }

    pub fn collect_full(&mut self, atoms: &AtomTable) -> MachineResult<()> {
        while self.scan < self.alloc {
            self.scan_object(atoms)?;
        }
        Ok(())
    }

    pub fn collect_partial(&mut self, atoms: &AtomTable, allowance: u32) -> MachineResult<()> {
        let mut work = 0;

        while self.scan < self.alloc {
            #[cfg(mutau_debug_gc)]
            {
                println!("Beginning outer scan...");
                self.print_delta();
            }
            self.scan_object(atoms)?;

            work += 1;

            if work > allowance {
                return Err(Error::OutOfTime);
            }
        }

        Ok(())
    }

    #[cfg(mutau_debug_gc)]
    pub fn print_delta(&self) {
        println!(
            "{:?}",
            Delta {
                from: &self.from,
                to: &self.to,
                scan: self.scan as usize,
                alloc: self.alloc as usize,
            }
        );
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INSANITY: usize = 256;

    #[allow(unused_assignments, unused_variables, unused_mut)]
    fn factorial_program<I: Insn>() -> Program<I> {
        program! {
            fn abort() -> () [] {
                (panic)
                (panic)
            }

            fn fac(n) -> res [acc] {
                (#n > &1 -> #1)
                (jmpif #1 :re)
                (move &1 -> #res)
                (ret)
            :re (move #n -> %acc)
                (#n - &1 -> #n)
                (call @fac)
                (#n * %acc -> #res)
                (ret)
            }
        }
    }

    #[allow(unused_assignments, unused_variables, unused_mut)]
    fn factorial_tco_program<I: Insn>() -> Program<I> {
        program! {
            fn abort() -> () [] {
                (panic)
                (panic)
            }

            fn fac(n) -> res [] {
                (move &1 -> #1)
                (call @fac_rec)
                (ret)
            }

            fn fac_rec(n, accum) -> res [] {
            :ck (#n <= &1 -> #2)
                (jmpif #2 :re)
                (#n * #accum -> #accum)
                (#n - &1 -> #n)
                (jmp :ck)
            :re (move #accum -> #res)
                (ret)
            }
        }
    }

    #[allow(unused_assignments, unused_variables, unused_mut)]
    fn factorial_tco_relative_jumps_program<I: Insn>() -> Program<I> {
        program! {
            fn abort() -> () [] {
                (panic)
                (panic)
            }

            fn fac(n) -> res [] {
                (move &1 -> #1)
                (call @fac_rec)
                (ret)
            }

            fn fac_rec(n, accum) -> res [] {
            :ck (#n <= &1 -> #2)
                (jmpif #2 | 4)
                (#n * #accum -> #accum)
                (#n - &1 -> #n)
                (jmp | -4)
            :re (move #accum -> #res)
                (ret)
            }
        }
    }

    #[allow(unused_assignments, unused_variables, unused_mut)]
    fn factorial_list_program<I: Insn>() -> Program<I> {
        program! {
            fn abort() -> () [] {
                (panic)
                (panic)
            }

            fn facs(n) -> res [r] {
                (#n > &1 -> #2)
                (jmpif #2 :re)
                (cons &1 &() -> #res)
                (ret)
            :re (move #n -> %r)
                (#n - &1 -> #n)
                (call @facs)
                (car #0 -> #1)
                (#1 * %r -> #1)
                (cons #1 #0 -> #res)
                (ret)
            }
        }
    }

    fn short_state<I: Insn>(running: &Running<I>) -> String {
        format!(
            "Running {{ fp: {:?}, pc: {:?}, hp: {:?}, xs: {:?}, .. \
             | Curent instruction: {:?} }}",
            running.fp, running.pc, running.hp, running.xs, running.code.code[running.pc as usize]
        )
    }

    fn run_test<I: Insn>(
        prog: Program<I>,
        heap_size: usize,
        n_regs: usize,
        print_heap: bool,
        insanity: usize,
    ) -> Running<I> {
        let mut running = Running {
            supervisor: Arc::new(Mutex::new(Supervisor::new())),
            fp: 0,
            pc: 0,
            hp: 0,
            xs: vec![I::Word::default(); n_regs],
            code: prog,
            heap: Space(vec![I::Word::default(); heap_size]),
        };

        running.initial_continuation(0).unwrap();
        running.xs[0] = I::Word::val_integer(5);
        running.call_function(1).unwrap();

        let mut state = Ok(());
        let mut sanity = 0;

        println!("Machine state: {}", short_state(&running));
        while sanity < insanity {
            state = running.step_once();
            match state {
                Ok(()) => {}
                _ => break,
            }
            println!("Machine state: {}", short_state(&running));

            if print_heap {
                println!(
                    "Current heap before instruction execution: {:?}",
                    running.heap.0
                );
            }

            sanity += 1;
        }
        println!("Machine state: {}", short_state(&running));
        println!("Terminated: {:?}", state);
        running
    }

    fn run_gc_test<I: Insn>(
        prog: Program<I>,
        heap_size: usize,
        n_regs: usize,
        gc_freq: usize,
        print_heap: bool,
        insanity: usize,
    ) -> Running<I> {
        let mut running = Running {
            supervisor: Arc::new(Mutex::new(Supervisor::new())),
            fp: 0,
            pc: 0,
            hp: 0,
            xs: vec![I::Word::default(); n_regs],
            code: prog,
            heap: Space(vec![I::Word::default(); heap_size]),
        };

        running.initial_continuation(0).unwrap();
        running.xs[0] = I::Word::val_integer(5);
        running.call_function(1).unwrap();

        let mut state = Ok(());
        let mut sanity = 0;
        let mut collections = 0;

        println!("Machine state: {}", short_state(&running));
        while sanity < insanity {
            println!("Machine state before step: {}", short_state(&running));
            state = running.step_once();
            match state {
                Ok(()) => {}
                _ => break,
            }
            println!("Machine state after step: {}", short_state(&running));

            if sanity % gc_freq == gc_freq - 1 {
                println!("TRIGGERING COLLECTION {}", collections);

                if print_heap {
                    println!("Current heap before GC: {:?}", running.heap);
                }

                let mut collecting = running.collect().unwrap();
                collecting.collect_partial(&AtomTable::new(), 50).unwrap();
                running = collecting.run();

                if print_heap {
                    println!("Current heap after GC: {:?}", running.heap);
                }

                println!("Machine state after GC: {}", short_state(&running));

                collections += 1;

            // if collections == 2 {
            //     panic!();
            // }
            } else {
                if print_heap {
                    println!(
                        "Current heap before instruction execution: {:?}",
                        running.heap
                    );
                }
            }

            sanity += 1;
        }
        println!("Machine state: {}", short_state(&running));
        println!("Terminated: {:?}", state);
        running
    }

    fn factorial_test<I: Insn>() {
        let machine = run_test::<I>(factorial_program(), 32, 4, true, INSANITY);
        assert_eq!(machine.xs[0], I::Word::val_integer(120));
    }

    fn factorial_gc_test<I: Insn>() {
        let machine = run_gc_test::<I>(factorial_program(), 32, 4, 16, true, INSANITY);
        assert_eq!(machine.xs[0], I::Word::val_integer(120));
    }

    fn factorial_tco_test<I: Insn>() {
        let machine = run_test::<I>(factorial_tco_program(), 16, 4, true, INSANITY);
        assert_eq!(machine.xs[0], I::Word::val_integer(120));
    }

    fn factorial_tco_gc_test<I: Insn>() {
        let machine = run_gc_test::<I>(factorial_tco_program(), 16, 4, 16, true, INSANITY);
        assert_eq!(machine.xs[0], I::Word::val_integer(120));
    }

    fn factorial_list_test<I: Insn>() {
        let machine = run_test::<I>(factorial_list_program(), 64, 4, true, INSANITY);
        assert_eq!(
            machine.debug_value(machine.xs[0]),
            "(120 . (24 . (6 . (2 . (1 . ())))))"
        );
    }

    fn factorial_list_gc_test<I: Insn>() {
        let machine = run_gc_test::<I>(factorial_list_program(), 32, 4, 16, true, INSANITY);
        assert_eq!(
            machine.debug_value(machine.xs[0]),
            "(120 . (24 . (6 . (2 . (1 . ())))))"
        );
    }

    fn factorial_tco_relative_jumps_test<I: Insn>() {
        let machine = run_test::<I>(
            factorial_tco_relative_jumps_program(),
            16,
            4,
            true,
            INSANITY,
        );
        assert_eq!(machine.xs[0], I::Word::val_integer(120));
    }

    fn factorial_tco_relative_jumps_gc_test<I: Insn>() {
        let machine = run_gc_test::<I>(
            factorial_tco_relative_jumps_program(),
            16,
            4,
            16,
            true,
            INSANITY,
        );
        assert_eq!(machine.xs[0], I::Word::val_integer(120));
    }

    #[test]
    fn factorial_test_safe() {
        factorial_test::<UnpackedInsn<UnpackedWord>>()
    }

    #[test]
    fn factorial_gc_test_safe() {
        factorial_gc_test::<UnpackedInsn<UnpackedWord>>()
    }

    #[test]
    fn factorial_tco_test_safe() {
        factorial_tco_test::<UnpackedInsn<UnpackedWord>>()
    }

    #[test]
    fn factorial_tco_gc_test_safe() {
        factorial_tco_gc_test::<UnpackedInsn<UnpackedWord>>()
    }

    #[test]
    fn factorial_list_test_safe() {
        factorial_list_test::<UnpackedInsn<UnpackedWord>>()
    }

    #[test]
    fn factorial_list_gc_test_safe() {
        factorial_list_gc_test::<UnpackedInsn<UnpackedWord>>()
    }

    #[test]
    fn factorial_tco_relative_jumps_test_safe() {
        factorial_tco_relative_jumps_test::<UnpackedInsn<UnpackedWord>>()
    }

    #[test]
    fn factorial_tco_relative_jumps_gc_test_safe() {
        factorial_tco_relative_jumps_gc_test::<UnpackedInsn<UnpackedWord>>()
    }
}
