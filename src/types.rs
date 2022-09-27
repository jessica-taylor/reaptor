use std::collections::BTreeMap;
use std::ops;

use serde::{Deserialize, Serialize};

use crate::assembly::{Counts, VMStatement, VMProcedure, VMWordLValue, VMWordRValue, VMPtrLValue, VMPtrRValue, WordUnOp, WordBinOp};

#[derive(PartialEq, Eq, Debug, Clone, Copy, Serialize, Deserialize)]
enum Offset {
    Finite(usize),
    Infinite,
}

impl Offset {
    fn zero() -> Offset {
        Offset::Finite(0)
    }
    fn is_finite(&self) -> bool {
        match self {
            Offset::Finite(_) => true,
            Offset::Infinite => false,
        }
    }
    fn add_checked(&self, other: &Offset) -> Result<Offset, String> {
        match (self, other) {
            (Offset::Finite(a), Offset::Finite(b)) => Ok(Offset::Finite(a.checked_add(*b).ok_or("overflow")?)),
            (Offset::Finite(a), Offset::Infinite) => Ok(Offset::Infinite),
            (Offset::Infinite, Offset::Finite(0)) => Ok(Offset::Infinite),
            _ => Err("offset addition fail: infinite + non-zero".to_string()),
        }
    }
    fn mul_checked(&self, other: &Offset) -> Result<Offset, String> {
        match (self, other) {
            (Offset::Finite(a), Offset::Finite(b)) => Ok(Offset::Finite(a.checked_mul(*b).ok_or("overflow")?)),
            (Offset::Finite(a), Offset::Infinite) => {
                if *a == 0 {
                    Ok(Offset::Finite(0))
                } else {
                    Ok(Offset::Infinite)
                }
            }
            (Offset::Infinite, Offset::Finite(a)) => {
                if *a == 0 {
                    Ok(Offset::Finite(0))
                } else if *a == 1 {
                    Ok(Offset::Infinite)
                } else {
                    Err("offset multiplication fail: infinite * >1".to_string())
                }
            }
            _ => Err("offset multiplication fail: infinite * infinite".to_string()),
        }
    }
}

impl ops::Mul<usize> for Offset {
    type Output = Offset;
    fn mul(self, rhs: usize) -> Offset {
        match self {
            Offset::Finite(a) => Offset::Finite(a * rhs),
            Offset::Infinite => Offset::Infinite,
        }
    }
}

impl PartialOrd for Offset {
    fn partial_cmp(&self, other: &Offset) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Offset {
    fn cmp(&self, other: &Offset) -> std::cmp::Ordering {
        match (self, other) {
            (Offset::Finite(a), Offset::Finite(b)) => a.cmp(b),
            (Offset::Infinite, Offset::Infinite) => std::cmp::Ordering::Equal,
            (Offset::Infinite, Offset::Finite(_)) => std::cmp::Ordering::Greater,
            (Offset::Finite(_), Offset::Infinite) => std::cmp::Ordering::Less,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Serialize, Deserialize)]
struct TypedValueOffset {
    words: Offset,
    ptrs: Offset,
}

impl TypedValueOffset {
    fn zero() -> Self {
        Self {
            words: Offset::zero(),
            ptrs: Offset::zero(),
        }
    }
    fn is_finite(&self) -> bool {
        self.words.is_finite() && self.ptrs.is_finite()
    }
    fn add_checked(&self, other: &TypedValueOffset) -> Result<TypedValueOffset, String> {
        Ok(TypedValueOffset {
            words: self.words.add_checked(&other.words)?,
            ptrs: self.ptrs.add_checked(&other.ptrs)?,
        })
    }
    fn max(&self, other: &TypedValueOffset) -> TypedValueOffset {
        TypedValueOffset {
            words: std::cmp::max(self.words, other.words),
            ptrs: std::cmp::max(self.ptrs, other.ptrs),
        }
    }
    fn mul_checked(&self, other: &TypedValueOffset) -> Result<TypedValueOffset, String> {
        Ok(TypedValueOffset {
            words: self.words.mul_checked(&other.words)?,
            ptrs: self.ptrs.mul_checked(&other.ptrs)?,
        })
    }
    fn mul_offset_checked(&self, other: &Offset) -> Result<TypedValueOffset, String> {
        Ok(TypedValueOffset {
            words: self.words.mul_checked(other)?,
            ptrs: self.ptrs.mul_checked(other)?,
        })
    }
    fn to_counts(&self) -> Result<Counts, String> {
        match (self.words, self.ptrs) {
            (Offset::Finite(words), Offset::Finite(ptrs)) => Ok(Counts {
                words,
                ptrs,
            }),
            _ => Err("offset to counts fail: infinite".to_string()),
        }
    }
}

impl ops::Mul<usize> for TypedValueOffset {
    type Output = TypedValueOffset;
    fn mul(self, rhs: usize) -> TypedValueOffset {
        TypedValueOffset {
            words: self.words * rhs,
            ptrs: self.ptrs * rhs,
        }
    }
}



#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
enum SimpleType {
    Word,
    Ptr,
    Tuple(Vec<SimpleType>),
    Array(Offset, Box<SimpleType>),
    Union(Vec<SimpleType>),
}

impl SimpleType {
    fn size(&self) -> Result<TypedValueOffset, String> {
        match self {
            SimpleType::Word => Ok(TypedValueOffset {
                words: Offset::Finite(1),
                ptrs: Offset::Finite(0),
            }),
            SimpleType::Ptr => Ok(TypedValueOffset {
                words: Offset::Finite(0),
                ptrs: Offset::Finite(1),
            }),
            SimpleType::Tuple(t) => {
                let mut off = TypedValueOffset::zero();
                for elem in t {
                    off = off.add_checked(&elem.size()?)?;
                }
                Ok(off)
            },
            SimpleType::Array(len, t) => {
                t.size()?.mul_offset_checked(len)
            },
            SimpleType::Union(t) => {
                let mut off = TypedValueOffset::zero();
                for elem in t {
                    off = off.max(&elem.size()?);
                }
                Ok(off)
            }
        }

    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
enum SimpleTypeIndex<I> {
    This,
    TupleElem(usize, Box<SimpleTypeIndex<I>>),
    ArrayElem(I, Box<SimpleTypeIndex<I>>),
    UnionElem(usize, Box<SimpleTypeIndex<I>>),
    // note: this is postfix
}

impl<I> SimpleTypeIndex<I> {
    fn mapcat<I2>(&self, f: &mut dyn FnMut(&I) -> Result<I2, String>) -> Result<SimpleTypeIndex<I2>, String> {
        Ok(match self {
            SimpleTypeIndex::This => SimpleTypeIndex::This,
            SimpleTypeIndex::TupleElem(i, idx) => SimpleTypeIndex::TupleElem(*i, Box::new(idx.mapcat(f)?)),
            SimpleTypeIndex::ArrayElem(i, idx) => SimpleTypeIndex::ArrayElem(f(i)?, Box::new(idx.mapcat(f)?)),
            SimpleTypeIndex::UnionElem(i, idx) => SimpleTypeIndex::UnionElem(*i, Box::new(idx.mapcat(f)?)),
        })
    }
}


impl SimpleType {
    fn index<I>(&self, ix: &SimpleTypeIndex<I>) -> Result<SimpleType, String> {
        match (self, ix) {
            (_, SimpleTypeIndex::This) => Ok(self.clone()),
            (SimpleType::Tuple(t), SimpleTypeIndex::TupleElem(i, rest)) => t.get(*i).ok_or("bad tuple index".to_string())?.index(rest.as_ref()),
            (SimpleType::Array(_, t), SimpleTypeIndex::ArrayElem(_, rest)) => {
                t.index(rest.as_ref())
            },
            (SimpleType::Union(t), SimpleTypeIndex::UnionElem(i, rest)) => t.get(*i).ok_or("bad union index".to_string())?.index(rest.as_ref()),
            _ => Err("bad index".to_string()),
        }
    }
    fn index_range(&self, ix: &SimpleTypeIndex<usize>) -> Result<(TypedValueOffset, TypedValueOffset), String> {
        match (self, ix) {
            (_, SimpleTypeIndex::This) => Ok((TypedValueOffset::zero(), self.size()?)),
            (SimpleType::Tuple(t), SimpleTypeIndex::TupleElem(i, rest)) => {
                let mut offset = TypedValueOffset::zero();
                for (j, x) in t.iter().enumerate() {
                    if j == *i {
                        let (start, end) = x.index_range(rest.as_ref())?;
                        return Ok((offset.add_checked(&start)?, offset.add_checked(&end)?))
                    }
                    offset = offset.add_checked(&x.size()?)?;
                }
                Err("bad tuple index".to_string())
            },
            (SimpleType::Array(len, t), SimpleTypeIndex::ArrayElem(i, rest)) => {
                if Offset::Finite(*i) < *len {
                    let offset = t.size()? * *i;
                    let (start, end) = t.index_range(rest.as_ref())?;
                    Ok((offset.add_checked(&start)?, offset.add_checked(&end)?))
                } else {
                    Err("bad array index".to_string())
                }
            },
            (SimpleType::Union(t), SimpleTypeIndex::UnionElem(i, rest)) => {
                t.get(*i).ok_or("bad union index".to_string())?.index_range(rest.as_ref())
            },
            _ => Err("bad index".to_string())
        }
    }
}

trait TypecheckCtx<V> {
    fn local_type(&self, v: &V) -> Result<SimpleType, String>;
    fn global_type(&self, v: &V) -> Result<SimpleType, String>;
}

trait CompileCtx<V> : TypecheckCtx<V> {
    fn local_offset(&self, v: &V) -> Result<Counts, String>;
    fn global_offset(&self, v: &V) -> Result<Counts, String>;
}

#[derive(PartialEq, Eq, Serialize, Deserialize, Debug, Clone)]
enum TypedLValue<V> {
    Local(V),
    Global(V),
    TupleIndex(Box<TypedLValue<V>>, usize),
    ArrayIndex(Box<TypedLValue<V>>, Box<TypedRValue<V>>),
    UnionIndex(Box<TypedLValue<V>>, usize),
    DerefPtr(SimpleType, Box<TypedLValue<V>>),
}

impl<V> TypedLValue<V> {
    fn get_type<C: TypecheckCtx<V>>(&self, ctx: &C) -> Result<SimpleType, String> {
        match self {
            TypedLValue::Local(v) => ctx.local_type(v),
            TypedLValue::Global(v) => ctx.global_type(v),
            TypedLValue::TupleIndex(tup, ix) => tup.get_type(ctx)?.index::<usize>(&SimpleTypeIndex::TupleElem(*ix, Box::new(SimpleTypeIndex::This))),
            TypedLValue::ArrayIndex(arr, ix) => {
                if ix.get_type(ctx)? != SimpleType::Word {
                    return Err("array index must be a word".to_string());
                }
                arr.get_type(ctx)?.index::<usize>(&SimpleTypeIndex::ArrayElem(0, Box::new(SimpleTypeIndex::This)))
            },
            TypedLValue::UnionIndex(union, ix) => union.get_type(ctx)?.index::<usize>(&SimpleTypeIndex::UnionElem(*ix, Box::new(SimpleTypeIndex::This))),
            TypedLValue::DerefPtr(typ, ptr) => {
                if ptr.get_type(ctx)? != SimpleType::Ptr {
                    return Err("bad pointer type".to_string());
                }
                Ok(typ.clone())
            }
        }
    }
    fn compile_word_at<C: CompileCtx<V>>(&self, ix: usize, ctx: &C) -> Result<VMWordLValue, String> {
        let typ = self.get_type(ctx)?;
        match self {
            Self::Local(v) => {
                let off = ctx.local_offset(v)?;
                Ok(VMWordLValue::Local(Box::new(VMWordRValue::Const((off.words + ix) as u64))))
            },
            Self::Global(v) => {
                let off = ctx.global_offset(v)?;
                Ok(VMWordLValue::Global(Box::new(VMWordRValue::Const((off.words + ix) as u64))))
            },
            Self::TupleIndex(tup, tup_ix) => {
                let tup_typ = tup.get_type(ctx)?;
                let (start, end) = tup_typ.index_range(&SimpleTypeIndex::TupleElem(*tup_ix, Box::new(SimpleTypeIndex::This)))?;
                match start.words {
                    Offset::Finite(start_words) => {
                        let off = start_words + ix;
                        if Offset::Finite(off) < end.words {
                            tup.compile_word_at(off, ctx)
                        } else {
                            Err("bad tuple index".to_string())
                        }
                    },
                    Offset::Infinite => Err("bad tuple index".to_string()),
                }
            },
            Self::UnionIndex(union, union_ix) => {
                let union_typ = union.get_type(ctx)?;
                let (start, end) = union_typ.index_range(&SimpleTypeIndex::UnionElem(*union_ix, Box::new(SimpleTypeIndex::This)))?;
                match start.words {
                    Offset::Finite(start) => {
                        let off = start + ix;
                        if Offset::Finite(off) < end.words {
                            union.compile_word_at(off, ctx)
                        } else {
                            Err("bad union index".to_string())
                        }
                    },
                    Offset::Infinite => Err("bad union index".to_string()),
                }
            },
            Self::ArrayIndex(arr, ix) => {
                let arr_typ = arr.get_type(ctx)?;
                let (first_start, first_end) = arr_typ.index_range(&SimpleTypeIndex::ArrayElem(0, Box::new(SimpleTypeIndex::This)))?;
                match (first_start.words, first_end.words) {
                    (Offset::Finite(first_start), Offset::Finite(first_end)) => {
                        let first_len = first_end - first_start;
                        let first_lvalue = arr.compile_word_at(first_start, ctx)?;
                        let ix_compiled = Box::new(ix.compile_word_at(0, ctx)?);
                        let increase_offset = |off: Box<VMWordRValue>| -> Box<VMWordRValue> {
                            Box::new(VMWordRValue::BinOp(WordBinOp::Add(false, true), off,
                                Box::new(VMWordRValue::BinOp(WordBinOp::Mul(false, true), ix_compiled,
                                    Box::new(VMWordRValue::Const(first_len as u64))))))

                        };
                        match first_lvalue {
                            VMWordLValue::Local(off) =>
                                Ok(VMWordLValue::Local(increase_offset(off))),
                            VMWordLValue::Global(off) =>
                                Ok(VMWordLValue::Global(increase_offset(off))),
                            VMWordLValue::Index(arr2, arr_ix2) =>
                                Ok(VMWordLValue::Index(arr2, increase_offset(arr_ix2)))
                        }
                    },
                    _ => Err("bad array elem size".to_string()),
                }
            },
            Self::DerefPtr(data_type, ptr) => {
                let ptr_typ = ptr.get_type(ctx)?;
                if ptr_typ != SimpleType::Ptr {
                    return Err("bad pointer type".to_string());
                }
                let (start, end) = data_type.index_range(&SimpleTypeIndex::This)?;
                if let Offset::Finite(start_words) = start.words {
                    let off = start_words + ix;
                    if Offset::Finite(off) < end.words {
                        Ok(VMWordLValue::Index(Box::new(ptr.compile_ptr_at(off, ctx)?), Box::new(VMWordRValue::Const(ix as u64))))
                    } else {
                        Err("bad pointer index".to_string())
                    }
                } else {
                    Err("bad pointer size".to_string())
                }
            }
        }
    }
    fn compile_ptr_at<C: CompileCtx<V>>(&self, ix: usize, ctx: &C) -> Result<VMPtrLValue, String> {
        Err("not implemented".to_string())
    }
}

#[derive(PartialEq, Eq, Serialize, Deserialize, Debug, Clone)]
enum TypedRValue<V> {
    Copy(Box<TypedLValue<V>>),
    ConstWord(u64),
    PtrTag(Box<TypedLValue<V>>),
    PtrLengthWord(Box<TypedLValue<V>>),
    PtrLengthPtr(Box<TypedLValue<V>>),
    FunPtr(V, V), // module, function
    BinOp(WordBinOp, Box<TypedRValue<V>>, Box<TypedRValue<V>>),
    UnOp(WordUnOp, Box<TypedRValue<V>>),
}

impl<V> TypedRValue<V> {
    fn get_type<C: TypecheckCtx<V>>(&self, ctx: &C) -> Result<SimpleType, String> {
        match self {
            Self::Copy(lval) => lval.get_type(ctx),
            Self::ConstWord(_) => Ok(SimpleType::Word),
            Self::PtrTag(lval) | TypedRValue::PtrLengthWord(lval) | TypedRValue::PtrLengthPtr(lval) => {
                if lval.get_type(ctx)? != SimpleType::Ptr {
                    return Err("bad pointer type".to_string());
                }
                Ok(SimpleType::Word)
            },
            Self::FunPtr(_, _) => Ok(SimpleType::Ptr),
            Self::BinOp(_, lhs, rhs) => {
                if lhs.get_type(ctx)? != SimpleType::Word || rhs.get_type(ctx)? != SimpleType::Word {
                    return Err("bad word type".to_string());
                }
                Ok(SimpleType::Word)
            },
            Self::UnOp(_, val) => {
                if val.get_type(ctx)? != SimpleType::Word {
                    return Err("bad word type".to_string());
                }
                Ok(SimpleType::Word)
            }
        }
    }
    fn compile_word_at<C: CompileCtx<V>>(&self, ix: usize, ctx: &C) -> Result<VMWordRValue, String> {
        Err("not implemented".to_string())
    }
    fn compile_ptr_at<C: CompileCtx<V>>(&self, ix: usize, ctx: &C) -> Result<VMPtrRValue<V>, String> {
        Err("not implemented".to_string())
    }
}

#[derive(PartialEq, Eq, Serialize, Deserialize, Debug, Clone)]
enum AllocSpec<V> {
    Full,
    Tuple(Vec<AllocSpec<V>>),
    DynamicArray(Box<TypedRValue<V>>),
    TrailingUnion(usize, Box<AllocSpec<V>>)
}

#[derive(PartialEq, Eq, Serialize, Deserialize, Debug, Clone)]
enum AllocLength<V> {
    Const(usize),
    Dynamic(TypedRValue<V>)
}

impl<V: Eq + Ord + Clone> AllocSpec<V> {
    fn get_size(&self, typ: &SimpleType) -> Result<(AllocLength<V>, AllocLength<V>), String> {
        match self {
            Self::Full => {
                let counts = typ.size()?.to_counts()?;
                Ok((AllocLength::Const(counts.words), AllocLength::Const(counts.ptrs)))
            }
            Self::Tuple(specs) => {
                if let SimpleType::Tuple(typs) = typ {
                    if specs.len() != typs.len() {
                        return Err("bad tuple alloc spec".to_string());
                    }
                    let mut words: usize = 0;
                    let mut ptrs: usize = 0;
                    let mut word_addend = None;
                    let mut ptr_addend = None;
                    for (spec, typ) in specs.iter().zip(typs.iter()) {
                        let (w, p) = spec.get_size(typ)?;
                        match w {
                            AllocLength::Const(ws) => {
                                if ws != 0 && word_addend.is_some() {
                                    return Err("can't add items after a non-constant alloc size".to_string());
                                }
                                words += ws;
                            },
                            AllocLength::Dynamic(w) => {
                                if word_addend.is_some() {
                                    return Err("bad tuple alloc spec".to_string());
                                }
                                word_addend = Some(w);
                            }
                        }

                        match p {
                            AllocLength::Const(ps) => {
                                if ps != 0 && ptr_addend.is_some() {
                                    return Err("can't add items after a non-constant alloc size".to_string());
                                }
                                ptrs += ps;
                            },
                            AllocLength::Dynamic(p) => {
                                if ptr_addend.is_some() {
                                    return Err("bad tuple alloc spec".to_string());
                                }
                                ptr_addend = Some(p);
                            }
                        }
                    }
                    
                    let tot_words = if let Some(w) = word_addend {
                        AllocLength::Dynamic(TypedRValue::BinOp(WordBinOp::Add(false, true), Box::new(TypedRValue::ConstWord(words as u64)), Box::new(w)))
                    } else {
                        AllocLength::Const(words)
                    };
                    let tot_ptrs = if let Some(p) = ptr_addend {
                        AllocLength::Dynamic(TypedRValue::BinOp(WordBinOp::Add(false, true), Box::new(TypedRValue::ConstWord(ptrs as u64)), Box::new(p)))
                    } else {
                        AllocLength::Const(ptrs)
                    };
                    Ok((tot_words, tot_ptrs))
                } else {
                    Err("bad tuple type".to_string())
                }
            },
            Self::DynamicArray(len) => {
                if let SimpleType::Array(Offset::Infinite, elem_type) = typ {
                    let counts = elem_type.size()?.to_counts()?;
                    let words = if counts.words == 0 {
                        AllocLength::Const(0)
                    } else {
                        // TODO: problem that len is repeated?
                        AllocLength::Dynamic(TypedRValue::BinOp(WordBinOp::Mul(false, true), Box::new(TypedRValue::ConstWord(counts.words as u64)), len.clone()))
                    };
                    let ptrs = if counts.ptrs == 0 {
                        AllocLength::Const(0)
                    } else {
                        AllocLength::Dynamic(TypedRValue::BinOp(WordBinOp::Mul(false, true), Box::new(TypedRValue::ConstWord(counts.ptrs as u64)), len.clone()))
                    };
                    Ok((words, ptrs))
                } else {
                    Err("bad array type".to_string())
                }
            },
            Self::TrailingUnion(ix, spec) => {
                if let SimpleType::Union(typs) = typ {
                    if *ix >= typs.len() {
                        return Err("bad union index".to_string());
                    }
                    let (w, p) = spec.get_size(&typs[*ix])?;
                    let words = match w {
                        AllocLength::Const(w) => AllocLength::Dynamic(TypedRValue::ConstWord(w as u64)),
                        AllocLength::Dynamic(w) => AllocLength::Dynamic(w),
                    };
                    let ptrs = match p {
                        AllocLength::Const(p) => AllocLength::Dynamic(TypedRValue::ConstWord(p as u64)),
                        AllocLength::Dynamic(p) => AllocLength::Dynamic(p),
                    };
                    Ok((words, ptrs))
                } else {
                    Err("bad union type".to_string())
                }
            }
        }
    }
}

#[derive(PartialEq, Eq, Serialize, Deserialize, Debug, Clone)]
enum TypedStatement<V> {
    Assign(TypedLValue<V>, TypedRValue<V>),
    AssignClone(TypedLValue<V>, TypedLValue<V>),
    Swap(TypedLValue<V>, TypedLValue<V>),
    Alloc(TypedLValue<V>, TypedRValue<V>, SimpleType, AllocSpec<V>), // dest, ptr type, data type,
                                                                     // spec
    Call(V, V, Vec<TypedLValue<V>>, Vec<TypedLValue<V>>), // module, function, args, returns
    CallPtr(TypedLValue<V>, Vec<TypedLValue<V>>, Vec<TypedLValue<V>>),
    Return(Vec<TypedLValue<V>>),
    If(TypedRValue<V>, Vec<TypedStatement<V>>),
    Block(Vec<TypedStatement<V>>),
    Continue(usize),
    Break(usize)
}

#[derive(PartialEq, Eq, Serialize, Deserialize, Debug, Clone)]
struct TypedProcedure<V: Ord> {
    name: V,
    params: BTreeMap<V, SimpleType>,
    locals: BTreeMap<V, SimpleType>, // must not conflict with params
    returns: Vec<SimpleType>,
    statements: Vec<TypedStatement<V>>,
}

impl<V: Clone + Ord> TypedProcedure<V> {
    fn compile(&self) -> Result<VMProcedure<V>, String> {

        let mut var_offsets: BTreeMap<V, TypedValueOffset> = BTreeMap::new();
        let mut var_types: BTreeMap<V, SimpleType> = BTreeMap::new();

        let mut offset = TypedValueOffset::zero();
        for (k, v) in &self.params {
            var_types.insert(k.clone(), v.clone());
            var_offsets.insert(k.clone(), offset);
            offset = offset.add_checked(&v.size()?)?;
        }
        let param_counts = offset.to_counts()?;

        for (k, v) in &self.locals {
            var_types.insert(k.clone(), v.clone());
            var_offsets.insert(k.clone(), offset);
            offset = offset.add_checked(&v.size()?)?;
        }
        let local_counts = offset.to_counts()? - param_counts;

        let mut ret_offsets: Vec<TypedValueOffset> = Vec::new();
        let mut ret_offset = TypedValueOffset::zero();
        for ret in &self.returns {
            ret_offsets.push(ret_offset);
            ret_offset = ret_offset.add_checked(&ret.size()?)?;
        }
        let return_counts = ret_offset.to_counts()?;

        let mut statements: Vec<VMStatement<V>> = Vec::new();

        Ok(VMProcedure {
            name: self.name.clone(),
            param_counts,
            local_counts,
            return_counts,
            statements
        })
    }
}

#[derive(PartialEq, Eq, Serialize, Deserialize, Debug, Clone)]
struct TypedModule<V: Ord> {
    name: V,
    procedures: Vec<TypedProcedure<V>>,
}

#[derive(PartialEq, Eq, Serialize, Deserialize, Debug, Clone)]
struct TypedLibrary<V: Ord> {
    modules: Vec<TypedModule<V>>,
}
