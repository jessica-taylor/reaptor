use serde::{Deserialize, Serialize};

use crate::assembly::{VMType, Counts, VMStatement};

trait ExprCtx {
    fn lock_local(&mut self, typ: VMType) -> usize;
    fn unlock_local(&mut self, typ: VMType, idx: usize);
}

trait Expr<V> {
    fn size(&self) -> Counts;
    fn move_to_stack(&self, ctx: &mut ExprCtx, instructions: &mut Vec<VMStatement<V>>) -> bool {
        false
    }
    fn copy_to_stack(&self, ctx: &mut ExprCtx, instructions: &mut Vec<VMStatement<V>>) -> bool {
        false
    }
    fn move_from_stack(&self, ctx: &mut ExprCtx, instructions: &mut Vec<VMStatement<V>>) -> bool {
        false
    }
}

pub struct LocalPtrExpr {
    pub index: usize,
}

impl<V> Expr<V> for LocalPtrExpr {
    fn size(&self) -> Counts {
        Counts { words: 0, ptrs: 1 }
    }
    fn move_to_stack(&self, ctx: &mut ExprCtx, instructions: &mut Vec<VMStatement<V>>) -> bool {
        instructions.push(VMStatement::Null);
        instructions.push(VMStatement::SwapPtr(self.index));
        true
    }
    fn copy_to_stack(&self, ctx: &mut ExprCtx, instructions: &mut Vec<VMStatement<V>>) -> bool {
        instructions.push(VMStatement::PushPtr(self.index));
        true
    }
    fn move_from_stack(&self, ctx: &mut ExprCtx, instructions: &mut Vec<VMStatement<V>>) -> bool {
        instructions.push(VMStatement::PopPtr(self.index));
        true
    }
}

pub struct LocalWordExpr {
    pub index: usize,
}

impl<V> Expr<V> for LocalWordExpr {
    fn size(&self) -> Counts {
        Counts { words: 1, ptrs: 0 }
    }
    fn move_to_stack(&self, ctx: &mut ExprCtx, instructions: &mut Vec<VMStatement<V>>) -> bool {
        instructions.push(VMStatement::PushWord(self.index));
        true
    }
    fn copy_to_stack(&self, ctx: &mut ExprCtx, instructions: &mut Vec<VMStatement<V>>) -> bool {
        instructions.push(VMStatement::PushWord(self.index));
        true
    }
    fn move_from_stack(&self, ctx: &mut ExprCtx, instructions: &mut Vec<VMStatement<V>>) -> bool {
        instructions.push(VMStatement::PopWord(self.index));
        true
    }
}

pub struct PairExpr<A, B> {
    pub first: A,
    pub second: B,
}

impl<V, A : Expr<V>, B : Expr<V>> Expr<V> for PairExpr<A, B> {
    fn size(&self) -> Counts {
        self.first.size() + self.second.size()
    }
    fn move_to_stack(&self, ctx: &mut ExprCtx, instructions: &mut Vec<VMStatement<V>>) -> bool {
        self.first.move_to_stack(ctx, instructions) && self.second.move_to_stack(ctx, instructions)
    }
    fn copy_to_stack(&self, ctx: &mut ExprCtx, instructions: &mut Vec<VMStatement<V>>) -> bool {
        self.first.copy_to_stack(ctx, instructions) && self.second.copy_to_stack(ctx, instructions)
    }
    fn move_from_stack(&self, ctx: &mut ExprCtx, instructions: &mut Vec<VMStatement<V>>) -> bool {
        self.first.move_from_stack(ctx, instructions) && self.second.move_from_stack(ctx, instructions)
    }
}

pub struct IndexExpr<A> {
    pub ptr_expr: A,
    pub start: Counts;
    pub span: Counts;
}

impl<V: A : Expr<V>> Expr<V> for IndexExpr<A> {
    fn size(&self) -> Counts {
        let ptr_size = self.ptr_expr.size();
        if ptr_size != Counts { words: 0, ptrs: 1 } {
            panic!("IndexExpr must have a pointer type");
        }
        self.span
    }
    fn move_to_stack(&self, ctx: &mut ExprCtx, instructions: &mut Vec<VMStatement<V>>) -> bool {
        if !self.ptr_expr.move_to_stack(instructions) {
            return false;
        }
        let ptr_temp = ctx.lock_local(VMType::Ptr);
        for i in 0..self.span.words {
            instructions.push(VMStatement::ConstWord(self.start.words + i));
            instructions.push(VMStatement::GetWordAt);
        }
        for i in 0..self.span.ptrs {
            instructions.push(VMStatement::ConstWord(self.start.ptrs + i));
            instructions.push(VMStatement::Null);
            instructions.push(VMStatement::SwapPtrAt);
            // swap top 2 in stack
            instructions.push(VMStatement::PopPtr(ptr_temp));
            instructions.push(VMStatement::SwapPtr(ptr_temp));
            instructions.push(VMStatement::Null);
            instructions.push(VMStatement::SwapPtr(ptr_temp));
        }
        if !self.ptr_expr.move_from_stack(instructions) {
            return false;
        }
        return true;
    }

    fn copy_to_stack(&self, ctx: &mut ExprCtx, instructions: &mut Vec<VMStatement<V>>) -> bool {
        if !self.ptr_expr.move_to_stack(instructions) {
            return false;
        }
        let ptr_temp = ctx.lock_local(VMType::Ptr);
        for i in 0..self.span.words {
            instructions.push(VMStatement::ConstWord(self.start.words + i));
            instructions.push(VMStatement::GetWordAt);
        }
        for i in 0..self.span.ptrs {
            instructions.push(VMStatement::ConstWord(self.start.ptrs + i));
            instructions.push(VMStatement::GetPtrAt);
            // swap top 2 in stack
            instructions.push(VMStatement::PopPtr(ptr_temp));
            instructions.push(VMStatement::SwapPtr(ptr_temp));
            instructions.push(VMStatement::Null);
            instructions.push(VMStatement::SwapPtr(ptr_temp));
        }
        if !self.ptr_expr.move_from_stack(instructions) {
            return false;
        }
        return true;
    }
    fn move_from_stack(&self, ctx: &mut ExprCtx, instructions: &mut Vec<VMStatement<V>>) -> bool {
        if !self.ptr_expr.move_to_stack(instructions) {
            return false;
        }
        let word_temp = ctx.lock_local(VMType::Word);
        let ptr_temp = ctx.lock_local(VMType::Ptr);
        for i in (0..self.span.words).rev() {
            instructions.push(VMStatement::PopWord(word_temp));
            instructions.push(VMStatement::ConstWord(self.start.words + i));
            instructions.push(VMStatement::PushWord(word_temp));
            instructions.push(VMStatement::SetWordAt);
        }
        for i in (0..self.span.ptrs).rev() {
            // swap top 2 in stack
            instructions.push(VMStatement::PopPtr(ptr_temp));
            instructions.push(VMStatement::SwapPtr(ptr_temp));
            instructions.push(VMStatement::Null);
            instructions.push(VMStatement::SwapPtr(ptr_temp));

            instructions.push(VMStatement::ConstWord(self.start.ptrs + i));
            instructions.push(VMStatement::SetPtrAt);
        }
        if !self.ptr_expr.move_from_stack(instructions) {
            return false;
        }
        return true;
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
pub enum LPtrExpr<V> {
    Local(usize),
    Index(Box<LPtrExpr<V>>, Box<RWordExpr<V>>),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
pub enum LWordExpr<V> {
    Local(usize),
    Index(Box<LPtrExpr<V>>, Box<RWordExpr<V>>),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
pub enum RPtrExpr<V> {
    Local(usize),
    SwapLocal(usize),
    Index(Box<RPtrExpr<V>>, Box<RWordExpr<V>>),
    SwapIndex(Box<RPtrExpr<V>>, Box<RWordExpr<V>>),
    Null,
    Alloc(Box<RWordExpr<V>>, Box<RWordExpr<V>>),
    FunPtr(V, V),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
pub enum RWordExpr<V> {
    Local(usize),
    Index(Box<RPtrExpr<V>>, Box<RWordExpr<V>>),
    ConstWord(u64),
    PtrTag(Box<RPtrExpr<V>>),
    PtrLengthWord(Box<RPtrExpr<V>>),
    PtrLengthPtr(Box<RPtrExpr<V>>),
}
