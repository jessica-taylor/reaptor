use std::collections::BTreeMap;
use std::default::Default;
use std::rc::Rc;
use std::cell::RefCell;
use anyhow::{anyhow, bail};

use serde::{Deserialize, Serialize};

use crate::assembly::{Counts, VMStatement, VMProcedure, VMModule, VMLibrary, WordUnOp, WordBinOp, PtrUnOp};
use crate::error::Res;


#[derive(Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Serialize, Deserialize)]
struct WordIValue(u64);

#[derive(Clone)]
enum PtrIValue {
    Null,
    Rc(Rc<IValues>),
    Fun(usize, usize),
}

impl Default for PtrIValue {
    fn default() -> Self {
        PtrIValue::Null
    }
}

impl PartialEq for PtrIValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (PtrIValue::Null, PtrIValue::Null) => true,
            (PtrIValue::Fun(a, b), PtrIValue::Fun(c, d)) => a == c && b == d,
            (PtrIValue::Rc(a), PtrIValue::Rc(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl Eq for PtrIValue {}

impl PtrIValue {
    fn tag(&self) -> WordIValue {
        match self {
            PtrIValue::Null => WordIValue(0),
            PtrIValue::Rc(_) => WordIValue(1),
            PtrIValue::Fun(_, _) => WordIValue(2),
        }
    }
}

fn eval_word_unop(op: WordUnOp, val: WordIValue) -> WordIValue {
    match op {
        WordUnOp::Neg => WordIValue(-(val.0 as i64) as u64),
        WordUnOp::Not => WordIValue(!val.0),
    }
}

fn bool_to_word(b: bool) -> WordIValue {
    WordIValue((b as u64) * u64::MAX)
}

fn eval_word_binop(op: WordBinOp, lhs: WordIValue, rhs: WordIValue) -> Res<WordIValue> {
    Ok(match op {
        WordBinOp::Add(signed, checked) => {
            if signed {
                if checked {
                    WordIValue((lhs.0 as i64).checked_add(rhs.0 as i64).ok_or(anyhow!("overflow"))? as u64)
                } else {
                    WordIValue((lhs.0 as i64).wrapping_add(rhs.0 as i64) as u64)
                }
            } else {
                if checked {
                    WordIValue(lhs.0.checked_add(rhs.0).ok_or(anyhow!("overflow"))?)
                } else {
                    WordIValue(lhs.0.wrapping_add(rhs.0))
                }
            }
        }
        WordBinOp::Sub(signed, checked) => {
            if signed {
                if checked {
                    WordIValue((lhs.0 as i64).checked_sub(rhs.0 as i64).ok_or(anyhow!("overflow"))? as u64)
                } else {
                    WordIValue((lhs.0 as i64).wrapping_sub(rhs.0 as i64) as u64)
                }
            } else {
                if checked {
                    WordIValue(lhs.0.checked_sub(rhs.0).ok_or(anyhow!("overflow"))?)
                } else {
                    WordIValue(lhs.0.wrapping_sub(rhs.0))
                }
            }
        }
        WordBinOp::Mul(signed, checked) => {
            if signed {
                if checked {
                    WordIValue((lhs.0 as i64).checked_mul(rhs.0 as i64).ok_or(anyhow!("overflow"))? as u64)
                } else {
                    WordIValue((lhs.0 as i64).wrapping_mul(rhs.0 as i64) as u64)
                }
            } else {
                if checked {
                    WordIValue(lhs.0.checked_mul(rhs.0).ok_or(anyhow!("overflow"))?)
                } else {
                    WordIValue(lhs.0.wrapping_mul(rhs.0))
                }
            }
        }
        WordBinOp::Div(signed, checked) => {
            if signed {
                if checked {
                    WordIValue((lhs.0 as i64).checked_div(rhs.0 as i64).ok_or(anyhow!("overflow"))? as u64)
                } else {
                    WordIValue((lhs.0 as i64).wrapping_div(rhs.0 as i64) as u64)
                }
            } else {
                if checked {
                    WordIValue(lhs.0.checked_div(rhs.0).ok_or(anyhow!("overflow"))?)
                } else {
                    WordIValue(lhs.0.wrapping_div(rhs.0))
                }
            }
        }
        WordBinOp::Mod(signed, checked) => {
            if signed {
                if checked {
                    WordIValue((lhs.0 as i64).checked_rem(rhs.0 as i64).ok_or(anyhow!("overflow"))? as u64)
                } else {
                    WordIValue((lhs.0 as i64).wrapping_rem(rhs.0 as i64) as u64)
                }
            } else {
                if checked {
                    WordIValue(lhs.0.checked_rem(rhs.0).ok_or(anyhow!("overflow"))?)
                } else {
                    WordIValue(lhs.0.wrapping_rem(rhs.0))
                }
            }
        }
        WordBinOp::And => WordIValue(lhs.0 & rhs.0),
        WordBinOp::Or => WordIValue(lhs.0 | rhs.0),
        WordBinOp::Xor => WordIValue(lhs.0 ^ rhs.0),
        WordBinOp::Shl => WordIValue(lhs.0 << rhs.0),
        WordBinOp::Shr => WordIValue(lhs.0 >> rhs.0),
        WordBinOp::Eq => bool_to_word(lhs.0 == rhs.0),
        WordBinOp::Ne => bool_to_word(lhs.0 != rhs.0),
        WordBinOp::Lt(signed) => {
            if signed {
                bool_to_word((lhs.0 as i64) < (rhs.0 as i64))
            } else {
                bool_to_word(lhs.0 < rhs.0)
            }
        }
        WordBinOp::Le(signed) => {
            if signed {
                bool_to_word((lhs.0 as i64) <= (rhs.0 as i64))
            } else {
                bool_to_word(lhs.0 <= rhs.0)
            }
        }
        WordBinOp::Gt(signed) => {
            if signed {
                bool_to_word((lhs.0 as i64) > (rhs.0 as i64))
            } else {
                bool_to_word(lhs.0 > rhs.0)
            }
        }
        WordBinOp::Ge(signed) => {
            if signed {
                bool_to_word((lhs.0 as i64) >= (rhs.0 as i64))
            } else {
                bool_to_word(lhs.0 >= rhs.0)
            }
        }
    })
}

fn eval_ptr_unop(op: PtrUnOp, p: &PtrIValue) -> Res<WordIValue> {
    match op {
        PtrUnOp::PtrTag => Ok(p.tag()),
        PtrUnOp::PtrLengthWord => match p {
            PtrIValue::Rc(r) => Ok(WordIValue(r.words.len() as u64)),
            _ => bail!("Not an Rc".to_string())
        },
        PtrUnOp::PtrLengthPtr => match p {
            PtrIValue::Rc(r) => Ok(WordIValue(r.ptrs.len() as u64)),
            _ => bail!("Not an Rc".to_string())
        }
    }
}

#[derive(Default, Clone, Eq, PartialEq)]
struct IValues {
    words: Vec<WordIValue>,
    ptrs: Vec<PtrIValue>,
}

impl IValues {
    fn new(counts: &Counts) -> Self {
        IValues {
            words: vec![WordIValue::default(); counts.words],
            ptrs: vec![PtrIValue::default(); counts.ptrs],
        }
    }
}

enum BlockFrame<'a> {
    Block(usize, &'a [VMStatement<usize>]),
    WhileCond(usize, &'a [VMStatement<usize>], &'a [VMStatement<usize>]),
    WhileBody(usize, &'a [VMStatement<usize>], &'a [VMStatement<usize>]),
}

struct CallFrame<'a> {
    block_frames: Vec<BlockFrame<'a>>,
    local_words: Vec<WordIValue>,
    local_ptrs: Vec<PtrIValue>,
}

struct Interpreter<'a> {
    procedures: Vec<Vec<&'a VMProcedure<usize>>>,
    word_stack: Vec<WordIValue>,
    ptr_stack: Vec<PtrIValue>,
    call_stack: Vec<CallFrame<'a>>,
}

impl<'a> Interpreter<'a> {
    fn done(&self) -> bool {
        self.call_stack.is_empty()
    }
    fn step(&mut self) -> Res<()> {
        let call_frame = self.call_stack.last_mut().ok_or(anyhow!("No frame"))?;
        if call_frame.block_frames.is_empty() {
            self.call_stack.pop();
            return Ok(());
        }
        let block_frame = call_frame.block_frames.last_mut().ok_or(anyhow!("No block"))?;
        let stmt: &'a VMStatement<usize> = match block_frame {
            BlockFrame::Block(ix, stmts) => {
                if *ix >= stmts.len() {
                    call_frame.block_frames.pop();
                    return Ok(());
                }
                let stmt = &stmts[*ix];
                *ix += 1;
                stmt
            }
            BlockFrame::WhileCond(ix, cond, body) => {
                if *ix >= cond.len() {
                    if self.word_stack.pop().ok_or(anyhow!("No value"))?.0 != 0 {
                        *block_frame = BlockFrame::WhileBody(0, cond, body);
                    } else {
                        call_frame.block_frames.pop();
                    }
                    return Ok(());
                }
                let stmt = &cond[*ix];
                *ix += 1;
                stmt
            }
            BlockFrame::WhileBody(ix, cond, body) => {
                if *ix >= body.len() {
                    *block_frame = BlockFrame::WhileCond(0, cond, body);
                    return Ok(());
                }
                let stmt = &body[*ix];
                *ix += 1;
                stmt
            }
        };
        match stmt {
            VMStatement::DelWord => {
                self.word_stack.pop().ok_or(anyhow!("No word"))?;
            }
            VMStatement::DelPtr => {
                self.ptr_stack.pop().ok_or(anyhow!("No ptr"))?;
            }
            VMStatement::PopWord(w) => {
                *call_frame.local_words.get_mut(*w).ok_or(anyhow!("bad local index"))? = self.word_stack.pop().ok_or(anyhow!("No word"))?;
            }
            VMStatement::PopPtr(p) => {
                *call_frame.local_ptrs.get_mut(*p).ok_or(anyhow!("bad local index"))? = self.ptr_stack.pop().ok_or(anyhow!("No ptr"))?;
            }
            VMStatement::PushWord(w) => {
                self.word_stack.push(*call_frame.local_words.get(*w).ok_or(anyhow!("bad local index"))?);
            }
            VMStatement::PushPtr(p) => {
                self.ptr_stack.push(call_frame.local_ptrs.get(*p).ok_or(anyhow!("bad local index"))?.clone());
            }
            VMStatement::SwapPtr(w) => {
                std::mem::swap(self.ptr_stack.last_mut().ok_or(anyhow!("No word"))?, call_frame.local_ptrs.get_mut(*w).ok_or(anyhow!("bad local index"))?);
            }
            VMStatement::Null => {
                self.ptr_stack.push(PtrIValue::Null);
            }
            VMStatement::FunPtr(module, proc) => {
                self.ptr_stack.push(PtrIValue::Fun(*module, *proc));
            }
            VMStatement::ConstWord(w) => {
                self.word_stack.push(WordIValue(*w));
            }
            VMStatement::WordUn(op) => {
                let w = self.word_stack.pop().ok_or(anyhow!("No word"))?;
                self.word_stack.push(eval_word_unop(*op, w));
            }
            VMStatement::WordBin(op) => {
                let rhs = self.word_stack.pop().ok_or(anyhow!("No word"))?;
                let lhs = self.word_stack.pop().ok_or(anyhow!("No word"))?;
                self.word_stack.push(eval_word_binop(*op, lhs, rhs)?);
            }
            VMStatement::PtrUn(op) => {
                let p = self.ptr_stack.last().ok_or(anyhow!("No ptr"))?;
                self.word_stack.push(eval_ptr_unop(*op, &p)?);
            }
            VMStatement::AllocPtr => {
                let ptrs = self.word_stack.pop().ok_or(anyhow!("No word"))?;
                let words = self.word_stack.pop().ok_or(anyhow!("No word"))?;
                self.ptr_stack.push(PtrIValue::Rc(Rc::new(IValues {
                    words: vec![WordIValue::default(); words.0 as usize],
                    ptrs: vec![PtrIValue::default(); ptrs.0 as usize],
                })));
            }
            VMStatement::GetWordAt => {
                let p = self.ptr_stack.pop().ok_or(anyhow!("No ptr"))?;
                let ix = self.word_stack.pop().ok_or(anyhow!("No word"))?;
                match p {
                    PtrIValue::Rc(r) => {
                        self.word_stack.push(*r.words.get(ix.0 as usize).ok_or(anyhow!("Bad index"))?);
                    }
                    _ => bail!("Not an Rc".to_string()),
                }
            }
            VMStatement::GetPtrAt => {
                let p = self.ptr_stack.pop().ok_or(anyhow!("No ptr"))?;
                let ix = self.word_stack.pop().ok_or(anyhow!("No word"))?;
                match p {
                    PtrIValue::Rc(r) => {
                        self.ptr_stack.push(r.ptrs.get(ix.0 as usize).ok_or(anyhow!("Bad index"))?.clone());
                    }
                    _ => bail!("Not an Rc".to_string()),
                }
            }
            VMStatement::SetWordAt => {
                let v = self.word_stack.pop().ok_or(anyhow!("No word"))?;
                let p = self.ptr_stack.last_mut().ok_or(anyhow!("No ptr"))?;
                let ix = self.word_stack.pop().ok_or(anyhow!("No word"))?;
                match p {
                    PtrIValue::Rc(r) => {
                        *Rc::make_mut(r).words.get_mut(ix.0 as usize).ok_or(anyhow!("Bad index"))? = v;
                    }
                    _ => bail!("Not an Rc".to_string()),
                }
            }
            VMStatement::SetPtrAt => {
                let v = self.ptr_stack.pop().ok_or(anyhow!("No ptr"))?;
                let p = self.ptr_stack.last_mut().ok_or(anyhow!("No ptr"))?;
                let ix = self.word_stack.pop().ok_or(anyhow!("No word"))?;
                match p {
                    PtrIValue::Rc(r) => {
                        *Rc::make_mut(r).ptrs.get_mut(ix.0 as usize).ok_or(anyhow!("Bad index"))? = v;
                    }
                    _ => bail!("Not an Rc".to_string()),
                }
            }
            VMStatement::SwapPtrAt => {
                let mut v = self.ptr_stack.pop().ok_or(anyhow!("No ptr"))?;
                let p = self.ptr_stack.last_mut().ok_or(anyhow!("No ptr"))?;
                let ix = self.word_stack.pop().ok_or(anyhow!("No word"))?;
                match p {
                    PtrIValue::Rc(r) => {
                        std::mem::swap(&mut v, Rc::make_mut(r).ptrs.get_mut(ix.0 as usize).ok_or(anyhow!("Bad index"))?);
                    }
                    _ => bail!("Not an Rc".to_string()),
                }
                self.ptr_stack.push(v);
            }
            VMStatement::Call(module, proc) => {
                let proc = &self.procedures[*module][*proc];
                let new_call_frame = CallFrame {
                    local_words: vec![WordIValue::default(); proc.local_counts.words],
                    local_ptrs: vec![PtrIValue::default(); proc.local_counts.ptrs],
                    block_frames: vec![BlockFrame::Block(0, &proc.statements)],
                };
                self.call_stack.push(new_call_frame);
            }
            VMStatement::CallPtr(arg_counts, ret_counts) => {
                let ptr = self.ptr_stack.pop().ok_or(anyhow!("No ptr"))?;
                let proc = match ptr {
                    PtrIValue::Fun(module, proc) => &self.procedures[module][proc],
                    _ => bail!("Not a function".to_string()),
                };
                if proc.param_counts != *arg_counts {
                    bail!("Wrong number of arguments".to_string());
                }
                if proc.return_counts != *ret_counts {
                    bail!("Wrong number of return values".to_string());
                }
                let new_call_frame = CallFrame {
                    local_words: vec![WordIValue::default(); proc.local_counts.words],
                    local_ptrs: vec![PtrIValue::default(); proc.local_counts.ptrs],
                    block_frames: vec![(BlockFrame::Block(0, &proc.statements))],
                };
                self.call_stack.push(new_call_frame);
            }
            VMStatement::If(thn, els) => {
                let cond = self.word_stack.pop().ok_or(anyhow!("No word"))?;
                if cond.0 != 0 {
                    call_frame.block_frames.push(BlockFrame::Block(0, thn));
                } else {
                    call_frame.block_frames.push(BlockFrame::Block(0, els));
                }
            }
            VMStatement::While(cond, body) => {
                call_frame.block_frames.push(BlockFrame::WhileCond(0, cond, body));
            }
        }
        Ok(())
    }
}
