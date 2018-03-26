#[macro_export]
macro_rules! bamboozle {
    ($victim:tt $lol:expr) => { $lol };
}

#[macro_export]
macro_rules! bamboozle_insn {
    (( $($stuff:tt)* )) => { 1 };
    (:) => { 0 };
    ($label:ident) => { 0 };
}

#[macro_export]
macro_rules! count_insns {
    ($($insn:tt)*) => {{ 0 $(+ bamboozle_insn!($insn))* }};
}

#[macro_export]
macro_rules! count_names {
    ([$($insn:tt),*]) => {{ 0 $(+ bamboozle!($insn 1))* }};
    (($($insn:tt),*)) => {{ 0 $(+ bamboozle!($insn 1))* }};
    ($($insn:tt),*) => {{ 0 $(+ bamboozle!($insn 1))* }};
}

#[macro_export]
macro_rules! function_info {
    (@fold $fns:ident ($count:expr) [$name:ident $arg:tt $ret:tt $loc:tt $insns:tt] $($rest:tt)*) => {
        $fns.push(
            FunctionInfo {
                addr: $count,
                args: count_names!($arg),
                rets: count_names!($ret),
                locals: count_names!($loc),
            }
        );
        let $name = ($fns.len() - 1) as $crate::insn::Address;
        let current = $count + count_insns! $insns;
        function_info!(@fold $fns (current) $($rest)*);
    };
    (@fold $fns:ident ($count:expr)) => {};
    ($fns:ident $($info:tt)*) => {
        function_info!(@fold $fns (0) $($info)*);
    };
}

#[macro_export]
macro_rules! splat_alias {
    ($t:ty => ($($reg:tt),*)) => {
        let mut acc = 0;
        $(let $reg: $t = acc; acc += 1;)*
    };
    ($t:ty => [$($reg:tt),*]) => {
        let mut acc = 0;
        $(let $reg: $t = acc; acc += 1;)*
    };
    ($t:ty => $reg:ident) => {
        let $reg = 0;
    };
}

#[macro_export]
macro_rules! splat_label {
    (@bamboozle $c:ident ($($stuff:tt)*)) => { $c += 1; };
    (@bamboozle $c:ident :) => {};
    (@bamboozle $c:ident $lab:ident) => { let $lab = $c; };
    ($fns:ident $name:ident { $($insn:tt)* }) => {
        let mut counter = $fns[$name as usize].addr;
        $(splat_label!(@bamboozle counter $insn);)*
    };
}

#[macro_export]
macro_rules! parse_const {
    (true) => { $crate::word::Word::val_boolean(true) };
    (false) => { $crate::word::Word::val_boolean(false) };
    (()) => { $crate::word::Word::val_special($crate::word::Special::Nil) };
    ($n:expr) => { $crate::word::Word::val_integer($n as $crate::word::Signed) };
}

#[macro_export]
macro_rules! parse_src {
    (@$src:tt) => { $crate::insn::Src::C($crate::word::Word::val_function($src)) };
    (&$src:tt) => { $crate::insn::Src::C(parse_const!($src)) };
    (%$src:tt) => { $crate::insn::Src::L($src) };
    (#$src:tt) => { $crate::insn::Src::R($src) };
}

#[macro_export]
macro_rules! parse_dst {
    (%$src:tt) => { $crate::insn::Dst::L($src) };
    (#$src:tt) => { $crate::insn::Dst::R($src) };
}

#[macro_export]
macro_rules! parse_trg {
    (:$lab:expr) => { $crate::insn::Trg::Absolute($lab) };
    (|$off:expr) => { $crate::insn::Trg::Relative($off) };
}

#[macro_export]
macro_rules! parse_insn {
    (call $ftk:tt $fun:ident) => { $crate::insn::UnpackedInsn::Call(parse_src!($ftk $fun)) };
    (car $tok:tt $src:tt -> $dtk:tt $dst:tt) => { $crate::insn::UnpackedInsn::ListCar { src: parse_src!($tok $src), dst: parse_dst!($dtk $dst) } };
    (cons $tok0:tt $src0:tt $tok1:tt $src1:tt -> $dtk:tt $dst:tt) => { $crate::insn::UnpackedInsn::AllocCons { src_car: parse_src!($tok0 $src0), src_cdr: parse_src!($tok1 $src1), dst: parse_dst!($dtk $dst) } };
    (jmp $ltk:tt $lab:expr) => { $crate::insn::UnpackedInsn::Jmp(parse_trg!($ltk $lab)) };
    (jmpif $stk:tt $src:tt $ltk:tt $lab:expr) => { $crate::insn::UnpackedInsn::JmpIf(parse_src!($stk $src), parse_trg!($ltk $lab)) };
    (move $stk:tt $src:tt -> $dtk:tt $dst:tt) => { $crate::insn::UnpackedInsn::Move1 { src: parse_src!($stk $src), dst: parse_dst!($dtk $dst) } };
    (panic) => { $crate::insn::UnpackedInsn::Panic };
    (ret) => { $crate::insn::UnpackedInsn::Return };
    ($tok0:tt $src0:tt +  $tok1:tt $src1:tt -> $dtk:tt $dst:tt) => { $crate::insn::UnpackedInsn::IntAdd { src0: parse_src!($tok0 $src0), src1: parse_src!($tok1 $src1), dst: parse_dst!($dtk $dst) } };
    ($tok0:tt $src0:tt -  $tok1:tt $src1:tt -> $dtk:tt $dst:tt) => { $crate::insn::UnpackedInsn::IntSub { src0: parse_src!($tok0 $src0), src1: parse_src!($tok1 $src1), dst: parse_dst!($dtk $dst) } };
    ($tok0:tt $src0:tt *  $tok1:tt $src1:tt -> $dtk:tt $dst:tt) => { $crate::insn::UnpackedInsn::IntMul { src0: parse_src!($tok0 $src0), src1: parse_src!($tok1 $src1), dst: parse_dst!($dtk $dst) } };
    ($tok0:tt $src0:tt /  $tok1:tt $src1:tt -> $dtk:tt $dst:tt) => { $crate::insn::UnpackedInsn::IntDiv { src0: parse_src!($tok0 $src0), src1: parse_src!($tok1 $src1), dst: parse_dst!($dtk $dst) } };
    ($tok0:tt $src0:tt <  $tok1:tt $src1:tt -> $dtk:tt $dst:tt) => { $crate::insn::UnpackedInsn::CmpIntLt { src0: parse_src!($tok0 $src0), src1: parse_src!($tok1 $src1), dst: parse_dst!($dtk $dst) } };
    ($tok0:tt $src0:tt <= $tok1:tt $src1:tt -> $dtk:tt $dst:tt) => { $crate::insn::UnpackedInsn::CmpIntLe { src0: parse_src!($tok0 $src0), src1: parse_src!($tok1 $src1), dst: parse_dst!($dtk $dst) } };
    ($tok0:tt $src0:tt == $tok1:tt $src1:tt -> $dtk:tt $dst:tt) => { $crate::insn::UnpackedInsn::CmpIntEq { src0: parse_src!($tok0 $src0), src1: parse_src!($tok1 $src1), dst: parse_dst!($dtk $dst) } };
    ($tok0:tt $src0:tt != $tok1:tt $src1:tt -> $dtk:tt $dst:tt) => { $crate::insn::UnpackedInsn::CmpIntNe { src0: parse_src!($tok0 $src0), src1: parse_src!($tok1 $src1), dst: parse_dst!($dtk $dst) } };
    ($tok0:tt $src0:tt >  $tok1:tt $src1:tt -> $dtk:tt $dst:tt) => { $crate::insn::UnpackedInsn::CmpIntGt { src0: parse_src!($tok0 $src0), src1: parse_src!($tok1 $src1), dst: parse_dst!($dtk $dst) } };
    ($tok0:tt $src0:tt >= $tok1:tt $src1:tt -> $dtk:tt $dst:tt) => { $crate::insn::UnpackedInsn::CmpIntGe { src0: parse_src!($tok0 $src0), src1: parse_src!($tok1 $src1), dst: parse_dst!($dtk $dst) } };
}

#[macro_export]
macro_rules! pre_parse_insn {
    ($prog:ident ($($nobamboozle:tt)*)) => { $prog.push($crate::insn::Insn::pack(&parse_insn!($($nobamboozle)*))); }; // no bamboozle
    ($prog:ident :) => {}; // Label colon - ignore
    ($prog:ident $bamboozle:ident) => {}; // Label name - ignore
}

#[macro_export]
macro_rules! splat_insns {
    ($fns:ident $prog:ident $name:ident $arg:tt $ret:tt $loc:tt { $($insn:tt)* }) => {
        splat_alias!(u8 => $arg);
        splat_alias!(u8 => $ret);
        splat_alias!(u16 => $loc);
        splat_label!($fns $name { $($insn)* });

        $(pre_parse_insn!($prog $insn);)*
    }
}

#[macro_export]
macro_rules! program {
    ($(fn $name:ident $arg:tt -> $ret:tt $loc:tt $insns:tt)*) => {
        {
            let mut fns = Vec::new();
            let mut code = Vec::new();

            function_info!(fns $([$name $arg $ret $loc $insns])*);

            $(splat_insns!(fns code $name $arg $ret $loc $insns);)*

            Program { fns, code }
        }
    };
}
