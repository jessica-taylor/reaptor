use serde::{Deserialize, Serialize};
use anyhow::bail;

use crate::assembly::{VMType, Counts, VMStatement, WordUnOp, WordBinOp};

use crate::error::Res;

trait ExprCtx<V> {
    fn lock_local(&mut self, typ: VMType) -> usize;
    fn unlock_local(&mut self, typ: VMType, idx: usize);
    fn add_instruction(&mut self, stmt: VMStatement<V>);
}

trait Expr<V> {
    fn size(&self) -> Counts;
    fn copy_to_stack(&self, ctx: &mut impl ExprCtx<V>) -> Res<()> {
        bail!("not implemented");
    }
}

trait LExpr<V> : Expr<V> {
    type Handle;

    fn load(&self, ctx: &mut impl ExprCtx<V>) -> Res<Self::Handle>;
    fn unload(&self, ctx: &mut impl ExprCtx<V>, handle: Self::Handle) -> Res<()>;
    fn move_to_stack(&self, ctx: &mut impl ExprCtx<V>, handle: &mut Self::Handle) -> Res<()>;
    fn move_from_stack(&self, ctx: &mut impl ExprCtx<V>, handle: &mut Self::Handle) -> Res<()>;
}

// impl<V> Expr<V> for Box<dyn Expr<V>> {
//     fn size(&self) -> Counts {
//         self.as_ref().size()
//     }
//     fn copy_to_stack(&self, ctx: &mut impl ExprCtx<V>) -> Res<()> {
//         self.as_ref().copy_to_stack(ctx)
//     }
// }

// rvalues

pub struct ConstWordExpr(pub u64);

impl<V> Expr<V> for ConstWordExpr {
    fn size(&self) -> Counts {
        Counts {words: 1, ptrs: 0}
    }
    fn copy_to_stack(&self, ctx: &mut impl ExprCtx<V>) -> Res<()> {
        ctx.add_instruction(VMStatement::ConstWord(self.0));
        Ok(())
    }
}

pub struct NullExpr();

impl<V> Expr<V> for NullExpr {
    fn size(&self) -> Counts {
        Counts {words: 0, ptrs: 1}
    }
    fn copy_to_stack(&self, ctx: &mut impl ExprCtx<V>) -> Res<()> {
        ctx.add_instruction(VMStatement::Null);
        Ok(())
    }
}

pub struct FunPtrExpr<T>(pub T, pub T);

impl<V: Clone> Expr<V> for FunPtrExpr<V> {
    fn size(&self) -> Counts {
        Counts {words: 0, ptrs: 1}
    }
    fn copy_to_stack(&self, ctx: &mut impl ExprCtx<V>) -> Res<()> {
        ctx.add_instruction(VMStatement::FunPtr(self.0.clone(), self.1.clone()));
        Ok(())
    }
}

pub struct WordUnExpr<T>(pub WordUnOp, pub T);

impl<V, T : Expr<V>> Expr<V> for WordUnExpr<T> {
    fn size(&self) -> Counts {
        assert!(self.1.size() == (Counts {words: 1, ptrs: 0}));
        Counts {words: 1, ptrs: 0}
    }
    fn copy_to_stack(&self, ctx: &mut impl ExprCtx<V>) -> Res<()> {
        self.1.copy_to_stack(ctx)?;
        ctx.add_instruction(VMStatement::WordUn(self.0));
        Ok(())
    }
}

pub struct WordBinExpr<T, U>(pub WordBinOp, pub T, pub U);

impl<V, T : Expr<V>, U : Expr<V>> Expr<V> for WordBinExpr<T, U> {
    fn size(&self) -> Counts {
        assert!(self.1.size() == (Counts {words: 1, ptrs: 0}));
        assert!(self.2.size() == (Counts {words: 1, ptrs: 0}));
        Counts {words: 1, ptrs: 0}
    }
    fn copy_to_stack(&self, ctx: &mut impl ExprCtx<V>) -> Res<()> {
        self.1.copy_to_stack(ctx)?;
        self.2.copy_to_stack(ctx)?;
        ctx.add_instruction(VMStatement::WordBin(self.0));
        Ok(())
    }
}

pub struct AllocExpr<T, U>(pub T, pub U);

impl<V, T : Expr<V>, U : Expr<V>> Expr<V> for AllocExpr<T, U> {
    fn size(&self) -> Counts {
        assert!(self.0.size() == (Counts {words: 1, ptrs: 0}));
        assert!(self.1.size() == (Counts {words: 1, ptrs: 0}));
        Counts {words: 0, ptrs: 1}
    }
    fn copy_to_stack(&self, ctx: &mut impl ExprCtx<V>) -> Res<()> {
        self.1.copy_to_stack(ctx)?;
        self.0.copy_to_stack(ctx)?;
        ctx.add_instruction(VMStatement::AllocPtr);
        Ok(())
    }
}


// lvalues

pub struct LocalPtrExpr {
    pub index: usize,
}

impl<V> Expr<V> for LocalPtrExpr {
    fn size(&self) -> Counts {
        Counts { words: 0, ptrs: 1 }
    }
    fn copy_to_stack(&self, ctx: &mut impl ExprCtx<V>) -> Res<()> {
        ctx.add_instruction(VMStatement::PushPtr(self.index));
        Ok(())
    }
}

impl<V> LExpr<V> for LocalPtrExpr {
    type Handle = ();
    fn load(&self, ctx: &mut impl ExprCtx<V>) -> Res<()> {
        Ok(())
    }
    fn unload(&self, ctx: &mut impl ExprCtx<V>, handle: ()) -> Res<()> {
        Ok(())
    }
    fn move_to_stack(&self, ctx: &mut impl ExprCtx<V>, handle: &mut ()) -> Res<()> {
        ctx.add_instruction(VMStatement::Null);
        ctx.add_instruction(VMStatement::SwapPtr(self.index));
        Ok(())
    }
    fn move_from_stack(&self, ctx: &mut impl ExprCtx<V>, handle: &mut ()) -> Res<()> {
        ctx.add_instruction(VMStatement::PopPtr(self.index));
        Ok(())
    }
}

pub struct LocalWordExpr {
    pub index: usize,
}

impl<V> Expr<V> for LocalWordExpr {
    fn size(&self) -> Counts {
        Counts { words: 1, ptrs: 0 }
    }
    fn copy_to_stack(&self, ctx: &mut impl ExprCtx<V>) -> Res<()> {
        ctx.add_instruction(VMStatement::PushWord(self.index));
        Ok(())
    }
}
impl<V> LExpr<V> for LocalWordExpr {
    type Handle = ();
    fn load(&self, ctx: &mut impl ExprCtx<V>) -> Res<()> {
        Ok(())
    }
    fn unload(&self, ctx: &mut impl ExprCtx<V>, handle: ()) -> Res<()> {
        Ok(())
    }
    fn move_to_stack(&self, ctx: &mut impl ExprCtx<V>, handle: &mut ()) -> Res<()> {
        ctx.add_instruction(VMStatement::PushWord(self.index));
        Ok(())
    }
    fn move_from_stack(&self, ctx: &mut impl ExprCtx<V>, handle: &mut ()) -> Res<()> {
        ctx.add_instruction(VMStatement::PopWord(self.index));
        Ok(())
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
    fn copy_to_stack(&self, ctx: &mut impl ExprCtx<V>) -> Res<()> {
        self.first.copy_to_stack(ctx)?;
        self.second.copy_to_stack(ctx)
    }
}

impl<V, A : LExpr<V>, B : LExpr<V>> LExpr<V> for PairExpr<A, B> {
    type Handle = (A::Handle, B::Handle);
    fn load(&self, ctx: &mut impl ExprCtx<V>) -> Res<Self::Handle> {
        Ok((self.first.load(ctx)?, self.second.load(ctx)?))
    }
    fn unload(&self, ctx: &mut impl ExprCtx<V>, handle: Self::Handle) -> Res<()> {
        self.first.unload(ctx, handle.0)?;
        self.second.unload(ctx, handle.1)
    }
    fn move_to_stack(&self, ctx: &mut impl ExprCtx<V>, handle: &mut Self::Handle) -> Res<()> {
        self.first.move_to_stack(ctx, &mut handle.0)?;
        self.second.move_to_stack(ctx, &mut handle.1)
    }
    fn move_from_stack(&self, ctx: &mut impl ExprCtx<V>, handle: &mut Self::Handle) -> Res<()> {
        self.first.move_from_stack(ctx, &mut handle.0)?;
        self.second.move_from_stack(ctx, &mut handle.1)
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
    fn copy_to_stack(&self, ctx: &mut impl ExprCtx<V>) -> Res<()> {
        self.ptr_expr.copy_to_stack(ctx)?;
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
        Ok(())
    }
}
impl<V, A : LExpr<V>> LExpr<V> for IndexExpr<A> {
    type Handle = (A::Handle, usize);
    fn load(&self, ctx: &mut impl ExprCtx<V>) -> Res<Self::Handle> {
        let mut ptr_handle = self.ptr_expr.load(ctx)?;
        let ptr_var = ctx.lock_local(VMType::Ptr);
        self.ptr_expr.move_to_stack(ctx, &mut ptr_handle)?;
        ctx.add_instruction(VMStatement::PopPtr(ptr_var));
        Ok((ptr_handle, ptr_var))
    }
    fn unload(&self, ctx: &mut impl ExprCtx<V>, mut handle: Self::Handle) -> Res<()> {
        self.ptr_expr.move_from_stack(ctx, &mut handle.0)?;
        self.ptr_expr.unload(ctx, handle.0)?;
        ctx.unlock_local(VMType::Ptr, handle.1);
        Ok(())
    }
    fn move_to_stack(&self, ctx: &mut impl ExprCtx<V>, handle: &mut Self::Handle) -> Res<()> {
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
        Ok(())
    }

    fn move_from_stack(&self, ctx: &mut impl ExprCtx<V>, handle: &mut Self::Handle) -> Res<()> {
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
        Ok(())
    }
}

pub enum DynLExpr {
    LocalPtr(LocalPtrExpr),
    LocalWord(LocalWordExpr),
    Pair(Box<PairExpr<DynLExpr, DynLExpr>>)
}

impl<V> Expr<V> for DynLExpr {
    fn size(&self) -> Counts {
        match self {
            Self::LocalPtr(x) => <LocalPtrExpr as Expr<V>>::size(x),
            Self::LocalWord(x) => <LocalWordExpr as Expr<V>>::size(x),
            Self::Pair(x) => <PairExpr<DynLExpr, DynLExpr> as Expr<V>>::size(x)
        }
    }
    fn copy_to_stack(&self, ctx: &mut impl ExprCtx<V>) -> Res<()> {
        match self {
            Self::LocalPtr(x) => x.copy_to_stack(ctx),
            Self::LocalWord(x) => x.copy_to_stack(ctx),
            Self::Pair(x) => x.as_ref().copy_to_stack(ctx)
        }
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
