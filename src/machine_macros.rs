#[macro_export]
macro_rules! match_sp0 {
    ($sp0:expr; with $self:ident { $src:ident } => $out:expr) => {
        {
            let $src = match $sp0 {
                $crate::insn::Sp0::C { src } => src,
                $crate::insn::Sp0::L { src } => $self.get_local(src),
                $crate::insn::Sp0::R { src } => $self.xs[src as usize],
            };

            let _ = $out;

            Ok(())
        }
    }
}


#[macro_export]
macro_rules! match_sp2 {
    ($sp2:expr; with $self:ident { $src:ident, $dst0:ident, $dst1:ident } => $out:expr) => {
        {
            let $dst0 = |vm: &mut Running<W>, val: W| {
                // Set dst0.
                match $sp2 {
                    $crate::insn::Sp2::CLL { dst0, .. } | $crate::insn::Sp2::CLR { dst0, .. } |
                    $crate::insn::Sp2::LLL { dst0, .. } | $crate::insn::Sp2::LLR { dst0, .. } |
                    $crate::insn::Sp2::RLL { dst0, .. } | $crate::insn::Sp2::RLR { dst0, .. } => {
                        vm.set_local(dst0, val);
                    }
                    $crate::insn::Sp2::CRL { dst0, .. } | $crate::insn::Sp2::CRR { dst0, .. } |
                    $crate::insn::Sp2::LRL { dst0, .. } | $crate::insn::Sp2::LRR { dst0, .. } |
                    $crate::insn::Sp2::RRL { dst0, .. } | $crate::insn::Sp2::RRR { dst0, .. } => {
                        vm.set_register(dst0, val);
                    }
                }
            };

            let $dst1 = |vm: &mut Running<W>, val: W| {
                // Set dst1.
                match $sp2 {
                    $crate::insn::Sp2::CLL { dst1, .. } | $crate::insn::Sp2::CRL { dst1, .. } |
                    $crate::insn::Sp2::LLL { dst1, .. } | $crate::insn::Sp2::LRL { dst1, .. } |
                    $crate::insn::Sp2::RLL { dst1, .. } | $crate::insn::Sp2::RRL { dst1, .. } => {
                        vm.set_local(dst1, val);
                    }
                    $crate::insn::Sp2::CLR { dst1, .. } | $crate::insn::Sp2::CRR { dst1, .. } |
                    $crate::insn::Sp2::LLR { dst1, .. } | $crate::insn::Sp2::LRR { dst1, .. } |
                    $crate::insn::Sp2::RLR { dst1, .. } | $crate::insn::Sp2::RRR { dst1, .. } => {
                        vm.set_register(dst1, val);
                    }
                }
            };

            let $src = match $sp2 {
                $crate::insn::Sp2::CLL { src, .. } | $crate::insn::Sp2::CRL { src, .. } |
                $crate::insn::Sp2::CLR { src, .. } | $crate::insn::Sp2::CRR { src, .. } => src,
                $crate::insn::Sp2::LLL { src, .. } | $crate::insn::Sp2::LRL { src, .. } |
                $crate::insn::Sp2::LLR { src, .. } | $crate::insn::Sp2::LRR { src, .. } => $self.get_local(src),
                $crate::insn::Sp2::RLL { src, .. } | $crate::insn::Sp2::RRL { src, .. } |
                $crate::insn::Sp2::RLR { src, .. } | $crate::insn::Sp2::RRR { src, .. } => $self.get_register(src),
            };

            $out
        }
    }
}


#[macro_export]
macro_rules! match_op0_and_inc_pc {
    ($op0:expr; with $self:ident { } => $out:expr) => {
        {
            let out = $out;

            match $op0 {
                $crate::insn::Op0::L { dst } => {
                    $self.set_local(dst, out);
                }
                $crate::insn::Op0::R { dst } => {
                    $self.xs[dst as usize] = out;
                }
            }

            $self.pc += 1;

            Ok(())
        }
    }
}


#[macro_export]
macro_rules! match_op1_and_inc_pc {
    ($op1:expr; with $self:ident { $src:ident } => $out:expr) => {
        {
            use self::Op1::*;

            match $op1 {
                CL { src, dst } => {
                    let $src = src;
                    let out = $out;
                    $self.set_local(dst, out);
                }
                LL { src, dst } => {
                    let $src = $self.get_local(src);
                    let out = $out;
                    $self.set_local(dst, out);
                }
                RL { src, dst } => {
                    let $src = $self.xs[src as usize];
                    let out = $out;
                    $self.set_local(dst, out);
                }
                CR { src, dst } => {
                    let $src = src;
                    let out = $out;
                    $self.xs[dst as usize] = out;
                }
                LR { src, dst } => {
                    let $src = $self.get_local(src);
                    let out = $out;
                    $self.xs[dst as usize] = out;
                }
                RR { src, dst } => {
                    let $src = $self.xs[src as usize];
                    let out = $out;
                    $self.xs[dst as usize] = out;
                }
            }

            $self.pc += 1;

            Ok(())
        }
    }
}


/// 3 ways - pick C L or R. 3 ways again - pick C L or R. Then 2 ways - pick L or R.
/// Quotient: no double C.
/// Total: (3 * 3 - 1) * 2 = 8 * 2 = 16
/// LCL; RCL; CLL; LLL; RLL; CRL; LRL; RRL;
/// LCR; RCR; CLR; LLR; RLR; CRR; LRR; RRR;
#[macro_export]
macro_rules! match_op2_and_inc_pc {
    ($op2:expr; with $self:ident { $src0:ident, $src1:ident } => $out:expr) => {
        {
            use self::Op2::*;

            match $op2 {
                CCL { src0, src1, dst } => {
                    let $src0 = src0;
                    let $src1 = src1;
                    let out = $out;
                    $self.set_local(dst, out);
                }
                LCL { src0, src1, dst } => {
                    let $src0 = $self.get_local(src0);
                    let $src1 = src1;
                    let out = $out;
                    $self.set_local(dst, out);
                }
                RCL { src0, src1, dst } => {
                    let $src0 = $self.xs[src0 as usize];
                    let $src1 = src1;
                    let out = $out;
                    $self.set_local(dst, out);
                }
                CLL { src0, src1, dst } => {
                    let $src0 = src0;
                    let $src1 = $self.get_local(src1);
                    let out = $out;
                    $self.set_local(dst, out);
                }
                LLL { src0, src1, dst } => {
                    let $src0 = $self.get_local(src0);
                    let $src1 = $self.get_local(src1);
                    let out = $out;
                    $self.set_local(dst, out);
                }
                RLL { src0, src1, dst } => {
                    let $src0 = $self.xs[src0 as usize];
                    let $src1 = $self.get_local(src1);
                    let out = $out;
                    $self.set_local(dst, out);
                }
                CRL { src0, src1, dst } => {
                    let $src0 = src0;
                    let $src1 = $self.xs[src1 as usize];
                    let out = $out;
                    $self.set_local(dst, out);
                }
                LRL { src0, src1, dst } => {
                    let $src0 = $self.get_local(src0);
                    let $src1 = $self.xs[src1 as usize];
                    let out = $out;
                    $self.set_local(dst, out);
                }
                RRL { src0, src1, dst } => {
                    let $src0 = $self.xs[src0 as usize];
                    let $src1 = $self.xs[src1 as usize];
                    let out = $out;
                    $self.set_local(dst, out);
                }
                CCR { src0, src1, dst } => {
                    let $src0 = src0;
                    let $src1 = src1;
                    let out = $out;
                    $self.xs[dst as usize] = out;
                }
                LCR { src0, src1, dst } => {
                    let $src0 = $self.get_local(src0);
                    let $src1 = src1;
                    let out = $out;
                    $self.xs[dst as usize] = out;
                }
                RCR { src0, src1, dst } => {
                    let $src0 = $self.xs[src0 as usize];
                    let $src1 = src1;
                    let out = $out;
                    $self.xs[dst as usize] = out;
                }
                CLR { src0, src1, dst } => {
                    let $src0 = src0;
                    let $src1 = $self.get_local(src1);
                    let out = $out;
                    $self.xs[dst as usize] = out;
                }
                LLR { src0, src1, dst } => {
                    let $src0 = $self.get_local(src0);
                    let $src1 = $self.get_local(src1);
                    let out = $out;
                    $self.xs[dst as usize] = out;
                }
                RLR { src0, src1, dst } => {
                    let $src0 = $self.xs[src0 as usize];
                    let $src1 = $self.get_local(src1);
                    let out = $out;
                    $self.xs[dst as usize] = out;
                }
                CRR { src0, src1, dst } => {
                    let $src0 = src0;
                    let $src1 = $self.xs[src1 as usize];
                    let out = $out;
                    $self.xs[dst as usize] = out;
                }
                LRR { src0, src1, dst } => {
                    let $src0 = $self.get_local(src0);
                    let $src1 = $self.xs[src1 as usize];
                    let out = $out;
                    $self.xs[dst as usize] = out;
                }
                RRR { src0, src1, dst } => {
                    let $src0 = $self.xs[src0 as usize];
                    let $src1 = $self.xs[src1 as usize];
                    let out = $out;
                    $self.xs[dst as usize] = out;
                }
            }

            $self.pc += 1;

            Ok(())
        }
    }
}
