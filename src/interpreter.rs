use std::collections::BTreeMap;
use std::default::Default;
use std::rc::Rc;
use std::cell::RefCell;

use serde::{Deserialize, Serialize};

use crate::assembly::{Counts, VMStatement, VMProcedure, VMModule, VMLibrary, WordUnOp, WordBinOp};


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

fn eval_word_binop(op: WordBinOp, lhs: WordIValue, rhs: WordIValue) -> Result<WordIValue, String> {
    Ok(match op {
        WordBinOp::Add(signed, checked) => {
            if signed {
                if checked {
                    WordIValue((lhs.0 as i64).checked_add(rhs.0 as i64).ok_or("overflow".to_string())? as u64)
                } else {
                    WordIValue((lhs.0 as i64).wrapping_add(rhs.0 as i64) as u64)
                }
            } else {
                if checked {
                    WordIValue(lhs.0.checked_add(rhs.0).ok_or("overflow".to_string())?)
                } else {
                    WordIValue(lhs.0.wrapping_add(rhs.0))
                }
            }
        }
        WordBinOp::Sub(signed, checked) => {
            if signed {
                if checked {
                    WordIValue((lhs.0 as i64).checked_sub(rhs.0 as i64).ok_or("overflow".to_string())? as u64)
                } else {
                    WordIValue((lhs.0 as i64).wrapping_sub(rhs.0 as i64) as u64)
                }
            } else {
                if checked {
                    WordIValue(lhs.0.checked_sub(rhs.0).ok_or("overflow".to_string())?)
                } else {
                    WordIValue(lhs.0.wrapping_sub(rhs.0))
                }
            }
        }
        WordBinOp::Mul(signed, checked) => {
            if signed {
                if checked {
                    WordIValue((lhs.0 as i64).checked_mul(rhs.0 as i64).ok_or("overflow".to_string())? as u64)
                } else {
                    WordIValue((lhs.0 as i64).wrapping_mul(rhs.0 as i64) as u64)
                }
            } else {
                if checked {
                    WordIValue(lhs.0.checked_mul(rhs.0).ok_or("overflow".to_string())?)
                } else {
                    WordIValue(lhs.0.wrapping_mul(rhs.0))
                }
            }
        }
        WordBinOp::Div(signed, checked) => {
            if signed {
                if checked {
                    WordIValue((lhs.0 as i64).checked_div(rhs.0 as i64).ok_or("overflow".to_string())? as u64)
                } else {
                    WordIValue((lhs.0 as i64).wrapping_div(rhs.0 as i64) as u64)
                }
            } else {
                if checked {
                    WordIValue(lhs.0.checked_div(rhs.0).ok_or("overflow".to_string())?)
                } else {
                    WordIValue(lhs.0.wrapping_div(rhs.0))
                }
            }
        }
        WordBinOp::Mod(signed, checked) => {
            if signed {
                if checked {
                    WordIValue((lhs.0 as i64).checked_rem(rhs.0 as i64).ok_or("overflow".to_string())? as u64)
                } else {
                    WordIValue((lhs.0 as i64).wrapping_rem(rhs.0 as i64) as u64)
                }
            } else {
                if checked {
                    WordIValue(lhs.0.checked_rem(rhs.0).ok_or("overflow".to_string())?)
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

struct CallFrame<'a> {
    block_frames: Vec<(&'a [VMStatement<usize>], usize)>,
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
    fn step(&mut self) -> Result<(), String> {
        let call_frame = self.call_stack.last_mut().ok_or("No frame")?;
        if call_frame.block_frames.is_empty() {
            self.call_stack.pop();
            return Ok(());
        }
        let block_frame = call_frame.block_frames.last_mut().ok_or("No block")?;
        if block_frame.1 >= block_frame.0.len() {
            call_frame.block_frames.pop();
            return Ok(());
        }
        let stmt = &block_frame.0[block_frame.1];
        block_frame.1 += 1;
        match stmt {
            VMStatement::DelWord => {
                self.word_stack.pop().ok_or("No word")?;
            }
            VMStatement::DelPtr => {
                self.ptr_stack.pop().ok_or("No ptr")?;
            }
            VMStatement::PopWord(w) => {
                call_frame.local_words[*w] = self.word_stack.pop().ok_or("No word")?;
            }
            VMStatement::PopPtr(p) => {
                call_frame.local_ptrs[*p] = self.ptr_stack.pop().ok_or("No ptr")?;
            }
            VMStatement::PushWord(w) => {
                self.word_stack.push(call_frame.local_words[*w]);
            }
            VMStatement::PushPtr(p) => {
                self.ptr_stack.push(call_frame.local_ptrs[*p].clone());
            }
            VMStatement::SwapPtr(w) => {
                std::mem::swap(self.ptr_stack.last_mut().ok_or("No word")?, &mut call_frame.local_ptrs[*w]);
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
                let w = self.word_stack.pop().ok_or("No word")?;
                self.word_stack.push(eval_word_unop(*op, w));
            }
            VMStatement::WordBin(op) => {
                let rhs = self.word_stack.pop().ok_or("No word")?;
                let lhs = self.word_stack.pop().ok_or("No word")?;
                self.word_stack.push(eval_word_binop(*op, lhs, rhs)?);
            }
            VMStatement::PtrTag => {
                let p = self.ptr_stack.last().ok_or("No ptr")?;
                self.word_stack.push(p.tag());
            }
            VMStatement::PtrLengthWord => {
                let p = self.ptr_stack.last().ok_or("No ptr")?;
                match p {
                    PtrIValue::Rc(r) => self.word_stack.push(WordIValue(r.words.len() as u64)),
                    _ => return Err("Not an Rc".to_string()),
                }
            }
            VMStatement::PtrLengthPtr => {
                let p = self.ptr_stack.last().ok_or("No ptr")?;
                match p {
                    PtrIValue::Rc(r) => self.word_stack.push(WordIValue(r.ptrs.len() as u64)),
                    _ => return Err("Not an Rc".to_string()),
                }
            }
            VMStatement::AllocPtr => {
                let ptrs = self.word_stack.pop().ok_or("No word")?;
                let words = self.word_stack.pop().ok_or("No word")?;
                self.ptr_stack.push(PtrIValue::Rc(Rc::new(IValues {
                    words: vec![WordIValue::default(); words.0 as usize],
                    ptrs: vec![PtrIValue::default(); ptrs.0 as usize],
                })));
            }
            VMStatement::GetWordAt => {
                let p = self.ptr_stack.pop().ok_or("No ptr")?;
                let ix = self.word_stack.pop().ok_or("No word")?;
                match p {
                    PtrIValue::Rc(r) => {
                        self.word_stack.push(r.words[ix.0 as usize]);
                    }
                    _ => return Err("Not an Rc".to_string()),
                }
            }
            VMStatement::GetPtrAt => {
                let p = self.ptr_stack.pop().ok_or("No ptr")?;
                let ix = self.word_stack.pop().ok_or("No word")?;
                match p {
                    PtrIValue::Rc(r) => {
                        self.ptr_stack.push(r.ptrs[ix.0 as usize].clone());
                    }
                    _ => return Err("Not an Rc".to_string()),
                }
            }
            VMStatement::SetWordAt => {
                let v = self.word_stack.pop().ok_or("No word")?;
                let p = self.ptr_stack.last_mut().ok_or("No ptr")?;
                let ix = self.word_stack.pop().ok_or("No word")?;
                match p {
                    PtrIValue::Rc(r) => {
                        Rc::make_mut(r).words[ix.0 as usize] = v;
                    }
                    _ => return Err("Not an Rc".to_string()),
                }
            }
            VMStatement::SetPtrAt => {
                let v = self.ptr_stack.pop().ok_or("No ptr")?;
                let p = self.ptr_stack.last_mut().ok_or("No ptr")?;
                let ix = self.word_stack.pop().ok_or("No word")?;
                match p {
                    PtrIValue::Rc(r) => {
                        Rc::make_mut(r).ptrs[ix.0 as usize] = v;
                    }
                    _ => return Err("Not an Rc".to_string()),
                }
            }
            VMStatement::SwapPtrAt => {
                let mut v = self.ptr_stack.pop().ok_or("No ptr")?;
                let p = self.ptr_stack.last_mut().ok_or("No ptr")?;
                let ix = self.word_stack.pop().ok_or("No word")?;
                match p {
                    PtrIValue::Rc(r) => {
                        std::mem::swap(&mut v, &mut Rc::make_mut(r).ptrs[ix.0 as usize]);
                    }
                    _ => return Err("Not an Rc".to_string()),
                }
                self.ptr_stack.push(v);
            }
            VMStatement::Call(module, proc) => {
                let proc = &self.procedures[*module][*proc];
                let new_call_frame = CallFrame {
                    local_words: vec![WordIValue::default(); proc.local_counts.words],
                    local_ptrs: vec![PtrIValue::default(); proc.local_counts.ptrs],
                    block_frames: vec![(&proc.statements, 0)],
                };
                self.call_stack.push(new_call_frame);
            }
            VMStatement::CallPtr(arg_w, arg_p, ret_w, ret_p) => {
                let ptr = self.ptr_stack.pop().ok_or("No ptr")?;
                let proc = match ptr {
                    PtrIValue::Fun(module, proc) => &self.procedures[module][proc],
                    _ => return Err("Not a function".to_string()),
                };
                if proc.param_counts.words != *arg_w || proc.param_counts.ptrs != *arg_p {
                    return Err("Wrong number of arguments".to_string());
                }
                if proc.return_counts.words != *ret_w || proc.return_counts.ptrs != *ret_p {
                    return Err("Wrong number of return values".to_string());
                }
                let new_call_frame = CallFrame {
                    local_words: vec![WordIValue::default(); proc.local_counts.words],
                    local_ptrs: vec![PtrIValue::default(); proc.local_counts.ptrs],
                    block_frames: vec![(&proc.statements, 0)],
                };
                self.call_stack.push(new_call_frame);
            }
            VMStatement::If(thn, els) => {
                let cond = self.word_stack.pop().ok_or("No word")?;
                if cond.0 != 0 {
                    call_frame.block_frames.push((thn, 0));
                } else {
                    call_frame.block_frames.push((els, 0));
                }
            }
            _ => unimplemented!(),
        }
        Ok(())
    }
}
