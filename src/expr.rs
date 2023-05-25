use serde::{Deserialize, Serialize};

use crate::assembly::{VMType, Counts, VMStatement};

trait ExprCtx<V> {
    fn lock_local(&mut self, typ: VMType) -> usize;
    fn unlock_local(&mut self, typ: VMType, idx: usize);
    fn add_instruction(&mut self, stmt: VMStatement<V>);
}

trait Expr<V> {
    fn size(&self) -> Counts;
    fn copy_to_stack(&self, ctx: &mut impl ExprCtx<V>) -> bool {
        false
    }
}

trait LExpr<V> : Expr<V> {
    type Handle;

    fn load(&self, ctx: &mut impl ExprCtx<V>) -> Option<Self::Handle> {
        None
    }
    fn unload(&self, ctx: &mut impl ExprCtx<V>, handle: Self::Handle) -> bool {
        true
    }
    fn move_to_stack(&self, ctx: &mut impl ExprCtx<V>, handle: &mut Self::Handle) -> bool {
        false
    }
    fn move_from_stack(&self, ctx: &mut impl ExprCtx<V>, handle: &mut Self::Handle) -> bool {
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
    fn copy_to_stack(&self, ctx: &mut impl ExprCtx<V>) -> bool {
        ctx.add_instruction(VMStatement::PushPtr(self.index));
        true
    }
}

impl<V> LExpr<V> for LocalPtrExpr {
    type Handle = ();
    fn load(&self, ctx: &mut impl ExprCtx<V>) -> Option<()> {
        Some(())
    }
    fn unload(&self, ctx: &mut impl ExprCtx<V>, handle: ()) -> bool {
        true
    }
    fn move_to_stack(&self, ctx: &mut impl ExprCtx<V>, handle: &mut ()) -> bool {
        ctx.add_instruction(VMStatement::Null);
        ctx.add_instruction(VMStatement::SwapPtr(self.index));
        true
    }
    fn move_from_stack(&self, ctx: &mut impl ExprCtx<V>, handle: &mut ()) -> bool {
        ctx.add_instruction(VMStatement::PopPtr(self.index));
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
    fn copy_to_stack(&self, ctx: &mut impl ExprCtx<V>) -> bool {
        ctx.add_instruction(VMStatement::PushWord(self.index));
        true
    }
}
impl<V> LExpr<V> for LocalWordExpr {
    type Handle = ();
    fn load(&self, ctx: &mut impl ExprCtx<V>) -> Option<()> {
        Some(())
    }
    fn unload(&self, ctx: &mut impl ExprCtx<V>, handle: ()) -> bool {
        true
    }
    fn move_to_stack(&self, ctx: &mut impl ExprCtx<V>, handle: &mut ()) -> bool {
        ctx.add_instruction(VMStatement::PushWord(self.index));
        true
    }
    fn move_from_stack(&self, ctx: &mut impl ExprCtx<V>, handle: &mut ()) -> bool {
        ctx.add_instruction(VMStatement::PopWord(self.index));
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
    fn copy_to_stack(&self, ctx: &mut impl ExprCtx<V>) -> bool {
        self.first.copy_to_stack(ctx) && self.second.copy_to_stack(ctx)
    }
}

impl<V, A : LExpr<V>, B : LExpr<V>> LExpr<V> for PairExpr<A, B> {
    type Handle = (A::Handle, B::Handle);
    fn load(&self, ctx: &mut impl ExprCtx<V>) -> Option<Self::Handle> {
        Some((self.first.load(ctx)?, self.second.load(ctx)?))
    }
    fn unload(&self, ctx: &mut impl ExprCtx<V>, handle: Self::Handle) -> bool {
        self.first.unload(ctx, handle.0) && self.second.unload(ctx, handle.1)
    }
    fn move_to_stack(&self, ctx: &mut impl ExprCtx<V>, handle: &mut Self::Handle) -> bool {
        self.first.move_to_stack(ctx, &mut handle.0) && self.second.move_to_stack(ctx, &mut handle.1)
    }
    fn move_from_stack(&self, ctx: &mut impl ExprCtx<V>, handle: &mut Self::Handle) -> bool {
        self.first.move_from_stack(ctx, &mut handle.0) && self.second.move_from_stack(ctx, &mut handle.1)
    }
}

pub struct IndexExpr<A> {
    pub ptr_expr: A,
    pub start: Counts,
    pub span: Counts
}

impl<V, A : Expr<V>> Expr<V> for IndexExpr<A> {
    fn size(&self) -> Counts {
        let ptr_size = self.ptr_expr.size();
        if ptr_size != (Counts { words: 0, ptrs: 1 }) {
            panic!("IndexExpr must have a pointer type");
        }
        self.span
    }
    fn copy_to_stack(&self, ctx: &mut impl ExprCtx<V>) -> bool {
        if !self.ptr_expr.copy_to_stack(ctx) {
            return false;
        }
        let ptr_temp = ctx.lock_local(VMType::Ptr);
        for i in 0..self.span.words {
            ctx.add_instruction(VMStatement::ConstWord((self.start.words + i) as u64));
            ctx.add_instruction(VMStatement::GetWordAt);
        }
        for i in 0..self.span.ptrs {
            ctx.add_instruction(VMStatement::ConstWord((self.start.ptrs + i) as u64));
            ctx.add_instruction(VMStatement::GetPtrAt);
            // swap top 2 in stack
            ctx.add_instruction(VMStatement::PopPtr(ptr_temp));
            ctx.add_instruction(VMStatement::SwapPtr(ptr_temp));
            ctx.add_instruction(VMStatement::Null);
            ctx.add_instruction(VMStatement::SwapPtr(ptr_temp));
        }
        return true;
    }
}
impl<V, A : LExpr<V>> LExpr<V> for IndexExpr<A> {
    type Handle = (A::Handle, usize);
    fn load(&self, ctx: &mut impl ExprCtx<V>) -> Option<Self::Handle> {
        let mut ptr_handle = self.ptr_expr.load(ctx)?;
        let ptr_var = ctx.lock_local(VMType::Ptr);
        if !self.ptr_expr.move_to_stack(ctx, &mut ptr_handle) {
            ctx.unlock_local(VMType::Ptr, ptr_var);
            return None;
        }
        ctx.add_instruction(VMStatement::PopPtr(ptr_var));
        Some((ptr_handle, ptr_var))
    }
    fn unload(&self, ctx: &mut impl ExprCtx<V>, mut handle: Self::Handle) -> bool {
        if !(self.ptr_expr.move_from_stack(ctx, &mut handle.0) && self.ptr_expr.unload(ctx, handle.0)) {
            return false;
        }
        ctx.unlock_local(VMType::Ptr, handle.1);
        true
    }
    fn move_to_stack(&self, ctx: &mut impl ExprCtx<V>, handle: &mut Self::Handle) -> bool {
        ctx.add_instruction(VMStatement::Null);
        ctx.add_instruction(VMStatement::SwapPtr(handle.1));
        let ptr_temp = ctx.lock_local(VMType::Ptr);
        for i in 0..self.span.words {
            ctx.add_instruction(VMStatement::ConstWord((self.start.words + i) as u64));
            ctx.add_instruction(VMStatement::GetWordAt);
        }
        for i in 0..self.span.ptrs {
            ctx.add_instruction(VMStatement::ConstWord((self.start.ptrs + i) as u64));
            ctx.add_instruction(VMStatement::Null);
            ctx.add_instruction(VMStatement::SwapPtrAt);
            // swap top 2 in stack
            ctx.add_instruction(VMStatement::PopPtr(ptr_temp));
            ctx.add_instruction(VMStatement::SwapPtr(ptr_temp));
            ctx.add_instruction(VMStatement::Null);
            ctx.add_instruction(VMStatement::SwapPtr(ptr_temp));
        }
        ctx.add_instruction(VMStatement::PopPtr(handle.1));
        ctx.unlock_local(VMType::Ptr, ptr_temp);
        return true;
    }

    fn move_from_stack(&self, ctx: &mut impl ExprCtx<V>, handle: &mut Self::Handle) -> bool {
        ctx.add_instruction(VMStatement::Null);
        ctx.add_instruction(VMStatement::SwapPtr(handle.1));
        let word_temp = ctx.lock_local(VMType::Word);
        let ptr_temp = ctx.lock_local(VMType::Ptr);
        for i in (0..self.span.words).rev() {
            ctx.add_instruction(VMStatement::PopWord(word_temp));
            ctx.add_instruction(VMStatement::ConstWord((self.start.words + i) as u64));
            ctx.add_instruction(VMStatement::PushWord(word_temp));
            ctx.add_instruction(VMStatement::SetWordAt);
        }
        for i in (0..self.span.ptrs).rev() {
            // swap top 2 in stack
            ctx.add_instruction(VMStatement::PopPtr(ptr_temp));
            ctx.add_instruction(VMStatement::SwapPtr(ptr_temp));
            ctx.add_instruction(VMStatement::Null);
            ctx.add_instruction(VMStatement::SwapPtr(ptr_temp));

            ctx.add_instruction(VMStatement::ConstWord((self.start.ptrs + i) as u64));
            ctx.add_instruction(VMStatement::SetPtrAt);
        }
        ctx.add_instruction(VMStatement::PopPtr(handle.1));
        ctx.unlock_local(VMType::Ptr, ptr_temp);
        return true;
    }
}

// #[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
// pub enum LPtrExpr<V> {
//     Local(usize),
//     Index(Box<LPtrExpr<V>>, Box<RWordExpr<V>>),
// }
// 
// #[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
// pub enum LWordExpr<V> {
//     Local(usize),
//     Index(Box<LPtrExpr<V>>, Box<RWordExpr<V>>),
// }
// 
// #[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
// pub enum RPtrExpr<V> {
//     Local(usize),
//     SwapLocal(usize),
//     Index(Box<RPtrExpr<V>>, Box<RWordExpr<V>>),
//     SwapIndex(Box<RPtrExpr<V>>, Box<RWordExpr<V>>),
//     Null,
//     Alloc(Box<RWordExpr<V>>, Box<RWordExpr<V>>),
//     FunPtr(V, V),
// }
// 
// #[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
// pub enum RWordExpr<V> {
//     Local(usize),
//     Index(Box<RPtrExpr<V>>, Box<RWordExpr<V>>),
//     ConstWord(u64),
//     PtrTag(Box<RPtrExpr<V>>),
//     PtrLengthWord(Box<RPtrExpr<V>>),
//     PtrLengthPtr(Box<RPtrExpr<V>>),
// }
