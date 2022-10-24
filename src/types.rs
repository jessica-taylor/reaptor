use std::collections::BTreeMap;
use std::ops;

use serde::{Deserialize, Serialize};

use crate::assembly::{Counts, VMStatement, VMProcedure, WordUnOp, WordBinOp};

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
enum StackType {
    Word,
    Ptr,
    Tuple(Vec<StackType>),
    Union(Vec<StackType>),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
struct HeapType {
    init: StackType,
    elem: Vec<StackType>, // length <= 2, sizes nonzero, sizes not multiples of each other
}

impl StackType {
    fn size(&self) -> Counts {
        match self {
            StackType::Word => Counts {
                words: 1,
                ptrs: 0
            },
            StackType::Ptr => Counts {
                words: 0,
                ptrs: 1
            },
            StackType::Tuple(t) => {
                let mut tot = Counts::zero();
                for elem in t {
                    tot = tot + elem.size();
                }
                tot
            },
            StackType::Union(t) => {
                let mut off = Counts::zero();
                for elem in t {
                    off = off.max(elem.size());
                }
                off
            }
        }

    }
}

enum StackTypeIndex {
    This,
    Tuple(usize, Box<StackTypeIndex>),
    Union(usize, Box<StackTypeIndex>),
}

impl StackTypeIndex {
    fn index_type<'a>(&self, t: &'a StackType) -> Result<&'a StackType, String> {
        match self {
            StackTypeIndex::This => Ok(t),
            StackTypeIndex::Tuple(i, rest) => match rest.index_type(t)? {
                StackType::Tuple(t) => t.get(*i).ok_or("Tuple index out of bounds".to_string()),
                _ => Err("not a tuple".to_string())
            },
            StackTypeIndex::Union(i, rest) => match rest.index_type(t)? {
                StackType::Union(t) => t.get(*i).ok_or("Union index out of bounds".to_string()),
                _ => Err("not a union".to_string())
            }
        }
    }
}

enum TypedLValue<V> {
    Local(StackTypeIndex, usize), // b[a]
    PtrInit(StackTypeIndex, HeapType, Box<TypedLValue<V>>), // c.init[a]
    PtrElem(StackTypeIndex, HeapType, Box<TypedLValue<V>>, usize, Box<TypedRValue<V>>), // c.elem[d][e][a]
}

enum TypedRValue<V> {
    LValue(Box<TypedLValue<V>>),
    Assign(Box<TypedLValue<V>>, Box<TypedRValue<V>>),
    Swap(Box<TypedLValue<V>>, Box<TypedLValue<V>>),
    Null,
    FunPtr(V, V),
    ConstWord(u64),
    WordUn(WordUnOp, Box<TypedRValue<V>>),
    WordBin(WordBinOp, Box<TypedRValue<V>>, Box<TypedRValue<V>>),
    PtrTag(Box<TypedRValue<V>>),
    PtrElemCount(Box<TypedRValue<V>>, HeapType),
    Call(V, V, Box<TypedRValue<V>>),
    CallPtr(Box<TypedRValue<V>>, Box<TypedRValue<V>>, StackType),
    If(Box<TypedRValue<V>>, Box<TypedRValue<V>>, Box<TypedRValue<V>>),
    While(Box<TypedRValue<V>>, Box<TypedRValue<V>>),
    Sequence(Box<TypedRValue<V>>, Box<TypedRValue<V>>),
}

trait TypecheckCtx<V> {
    fn local_type<'a>(&'a self, v: usize) -> Result<&'a StackType, String>;
    fn function_type<'a>(&'a self, module: &V, name: &V) -> Result<(&'a StackType, &'a StackType), String>;
}

impl<V> TypedLValue<V> {
    fn get_type(&self, ctx: &impl TypecheckCtx<V>) -> Result<StackType, String> {
        Ok(match self {
            TypedLValue::Local(ix, v) => ix.index_type(ctx.local_type(*v)?)?.clone(),
            TypedLValue::PtrInit(ix, typ, ptr) => {
                if ptr.get_type(ctx)? != StackType::Ptr {
                    return Err("not a pointer".to_string());
                }
                ix.index_type(&typ.init)?.clone()
            }
            TypedLValue::PtrElem(ix, typ, ptr, which_arr, arr_ix) => {
                if ptr.get_type(ctx)? != StackType::Ptr {
                    return Err("not a pointer".to_string());
                }
                if arr_ix.get_type(ctx)? != StackType::Word {
                    return Err("not a word".to_string());
                }
                ix.index_type(typ.elem.get(*which_arr).ok_or("no corresponding arary".to_string())?)?.clone()
            }
        })
    }
}

impl<V> TypedRValue<V> {
    fn get_type(&self, ctx: &impl TypecheckCtx<V>) -> Result<StackType, String> {
        Ok(match self {
            TypedRValue::LValue(l) => l.get_type(ctx)?,
            TypedRValue::Assign(l, r) => {
                if l.get_type(ctx)? != r.get_type(ctx)? {
                    return Err("type mismatch".to_string());
                }
                StackType::Tuple(vec![])
            }
            TypedRValue::Swap(l, r) => {
                if l.get_type(ctx)? != r.get_type(ctx)? {
                    return Err("type mismatch".to_string());
                }
                StackType::Tuple(vec![])
            }
            TypedRValue::Null => StackType::Ptr,
            TypedRValue::FunPtr(module, name) => {
                ctx.function_type(module, name)?;
                StackType::Ptr
            },
            TypedRValue::ConstWord(_) => StackType::Word,
            TypedRValue::WordUn(_, r) => {
                if r.get_type(ctx)? != StackType::Word {
                    return Err("not a word".to_string());
                }
                StackType::Word
            }
            TypedRValue::WordBin(_, l, r) => {
                if l.get_type(ctx)? != StackType::Word {
                    return Err("not a word".to_string());
                }
                if r.get_type(ctx)? != StackType::Word {
                    return Err("not a word".to_string());
                }
                StackType::Word
            }
            TypedRValue::PtrTag(r) => {
                if r.get_type(ctx)? != StackType::Ptr {
                    return Err("not a pointer".to_string());
                }
                StackType::Word
            }
            TypedRValue::PtrElemCount(r, _) => {
                if r.get_type(ctx)? != StackType::Ptr {
                    return Err("not a pointer".to_string());
                }
                StackType::Word
            }
            TypedRValue::Call(module, name, arg) => {
                let (arg_t, ret_t) = ctx.function_type(module, name)?;
                if arg.get_type(ctx)? != *arg_t {
                    return Err("type mismatch".to_string());
                }
                ret_t.clone()
            }
            TypedRValue::CallPtr(f, arg, typ) => {
                if f.get_type(ctx)? != StackType::Ptr {
                    return Err("not a pointer".to_string());
                }
                typ.clone()
            }
            TypedRValue::If(cond, then, els) => {
                if cond.get_type(ctx)? != StackType::Word {
                    return Err("not a word".to_string());
                }
                let then_t = then.get_type(ctx)?;
                let els_t = els.get_type(ctx)?;
                if then_t != els_t {
                    return Err("type mismatch".to_string());
                }
                then_t
            }
            TypedRValue::While(cond, body) => {
                if cond.get_type(ctx)? != StackType::Word {
                    return Err("not a word".to_string());
                }
                if body.get_type(ctx)? != StackType::Tuple(vec![]) {
                    return Err("not a unit".to_string());
                }
                StackType::Tuple(vec![])
            }
            TypedRValue::Sequence(l, r) => {
                l.get_type(ctx)?;
                r.get_type(ctx)?
            }
        })
    }
}

impl<V: Clone> TypedRValue<V> {
    pub fn to_statements(&self, ctx: &impl TypecheckCtx<V>) -> Result<Vec<VMStatement<V>>, String> {
        Ok(match self {
            Self::Null => vec![VMStatement::Null],
            Self::FunPtr(f, a) => vec![VMStatement::FunPtr(f.clone(), a.clone())],
            Self::ConstWord(w) => vec![VMStatement::ConstWord(*w)],
            Self::WordUn(op, v) => {
                let mut stmts = v.to_statements(ctx)?;
                stmts.push(VMStatement::WordUn(*op));
                stmts
            },
            Self::WordBin(op, v1, v2) => {
                let mut stmts = v1.to_statements(ctx)?;
                stmts.append(&mut v2.to_statements(ctx)?);
                stmts.push(VMStatement::WordBin(*op));
                stmts
            },
            Self::PtrTag(v) => {
                let mut stmts = v.to_statements(ctx)?;
                stmts.push(VMStatement::PtrTag);
                stmts
            },
            // PtrElemCount(v, typ) => {
            //     let mut stmts = v.to_statements(ctx)?;
            //     stmts.push(VMStatement::PtrElemCount);
            //     stmts
            // },
            Self::Call(module, procedure, v) => {
                let mut stmts = v.to_statements(ctx)?;
                stmts.push(VMStatement::Call(module.clone(), procedure.clone()));
                stmts
            },
            Self::CallPtr(v1, v2, typ) => {
                let mut stmts = v1.to_statements(ctx)?;
                stmts.append(&mut v2.to_statements(ctx)?);
                let arg_size = v2.get_type(ctx)?.size();
                let ret_size = typ.size();
                stmts.push(VMStatement::CallPtr(arg_size.words, arg_size.ptrs, ret_size.words, ret_size.ptrs));
                stmts
            },
            Self::If(cond, thn, els) => {
                let mut stmts = cond.to_statements(ctx)?;
                stmts.push(VMStatement::If(
                    thn.to_statements(ctx)?,
                    els.to_statements(ctx)?,
                ));
                stmts
            },
            Self::While(cond, body) => vec![VMStatement::While(
                cond.to_statements(ctx)?,
                body.to_statements(ctx)?
            )],
            _ => unimplemented!(),
        })
    }
}

// #[derive(PartialEq, Eq, Serialize, Deserialize, Debug, Clone)]
// enum TypedLValue<V> {
//     Local(V),
//     Global(V),
//     TupleIndex(Box<TypedLValue<V>>, usize),
//     ArrayIndex(Box<TypedLValue<V>>, Box<TypedRValue<V>>),
//     UnionIndex(Box<TypedLValue<V>>, usize),
//     DerefPtr(SimpleType, Box<TypedLValue<V>>),
// }
// 
// impl<V> TypedLValue<V> {
//     fn get_type<C: TypecheckCtx<V>>(&self, ctx: &C) -> Result<SimpleType, String> {
//         match self {
//             TypedLValue::Local(v) => ctx.local_type(v),
//             TypedLValue::Global(v) => ctx.global_type(v),
//             TypedLValue::TupleIndex(tup, ix) => tup.get_type(ctx)?.index::<usize>(&SimpleTypeIndex::TupleElem(*ix, Box::new(SimpleTypeIndex::This))),
//             TypedLValue::ArrayIndex(arr, ix) => {
//                 if ix.get_type(ctx)? != SimpleType::Word {
//                     return Err("array index must be a word".to_string());
//                 }
//                 arr.get_type(ctx)?.index::<usize>(&SimpleTypeIndex::ArrayElem(0, Box::new(SimpleTypeIndex::This)))
//             },
//             TypedLValue::UnionIndex(union, ix) => union.get_type(ctx)?.index::<usize>(&SimpleTypeIndex::UnionElem(*ix, Box::new(SimpleTypeIndex::This))),
//             TypedLValue::DerefPtr(typ, ptr) => {
//                 if ptr.get_type(ctx)? != SimpleType::Ptr {
//                     return Err("bad pointer type".to_string());
//                 }
//                 Ok(typ.clone())
//             }
//         }
//     }
//     fn compile_word_at<C: CompileCtx<V>>(&self, ix: usize, ctx: &C) -> Result<VMWordLValue, String> {
//         let typ = self.get_type(ctx)?;
//         match self {
//             Self::Local(v) => {
//                 let off = ctx.local_offset(v)?;
//                 Ok(VMWordLValue::Local(Box::new(VMWordRValue::Const((off.words + ix) as u64))))
//             },
//             Self::Global(v) => {
//                 let off = ctx.global_offset(v)?;
//                 Ok(VMWordLValue::Global(Box::new(VMWordRValue::Const((off.words + ix) as u64))))
//             },
//             Self::TupleIndex(tup, tup_ix) => {
//                 let tup_typ = tup.get_type(ctx)?;
//                 let (start, end) = tup_typ.index_range(&SimpleTypeIndex::TupleElem(*tup_ix, Box::new(SimpleTypeIndex::This)))?;
//                 match start.words {
//                     Offset::Finite(start_words) => {
//                         let off = start_words + ix;
//                         if Offset::Finite(off) < end.words {
//                             tup.compile_word_at(off, ctx)
//                         } else {
//                             Err("bad tuple index".to_string())
//                         }
//                     },
//                     Offset::Infinite => Err("bad tuple index".to_string()),
//                 }
//             },
//             Self::UnionIndex(union, union_ix) => {
//                 let union_typ = union.get_type(ctx)?;
//                 let (start, end) = union_typ.index_range(&SimpleTypeIndex::UnionElem(*union_ix, Box::new(SimpleTypeIndex::This)))?;
//                 match start.words {
//                     Offset::Finite(start) => {
//                         let off = start + ix;
//                         if Offset::Finite(off) < end.words {
//                             union.compile_word_at(off, ctx)
//                         } else {
//                             Err("bad union index".to_string())
//                         }
//                     },
//                     Offset::Infinite => Err("bad union index".to_string()),
//                 }
//             },
//             Self::ArrayIndex(arr, ix) => {
//                 let arr_typ = arr.get_type(ctx)?;
//                 let (first_start, first_end) = arr_typ.index_range(&SimpleTypeIndex::ArrayElem(0, Box::new(SimpleTypeIndex::This)))?;
//                 match (first_start.words, first_end.words) {
//                     (Offset::Finite(first_start), Offset::Finite(first_end)) => {
//                         let first_len = first_end - first_start;
//                         let first_lvalue = arr.compile_word_at(first_start, ctx)?;
//                         let ix_compiled = Box::new(ix.compile_word_at(0, ctx)?);
//                         let increase_offset = |off: Box<VMWordRValue>| -> Box<VMWordRValue> {
//                             Box::new(VMWordRValue::BinOp(WordBinOp::Add(false, true), off,
//                                 Box::new(VMWordRValue::BinOp(WordBinOp::Mul(false, true), ix_compiled,
//                                     Box::new(VMWordRValue::Const(first_len as u64))))))
// 
//                         };
//                         match first_lvalue {
//                             VMWordLValue::Local(off) =>
//                                 Ok(VMWordLValue::Local(increase_offset(off))),
//                             VMWordLValue::Global(off) =>
//                                 Ok(VMWordLValue::Global(increase_offset(off))),
//                             VMWordLValue::Index(arr2, arr_ix2) =>
//                                 Ok(VMWordLValue::Index(arr2, increase_offset(arr_ix2)))
//                         }
//                     },
//                     _ => Err("bad array elem size".to_string()),
//                 }
//             },
//             Self::DerefPtr(data_type, ptr) => {
//                 let ptr_typ = ptr.get_type(ctx)?;
//                 if ptr_typ != SimpleType::Ptr {
//                     return Err("bad pointer type".to_string());
//                 }
//                 let (start, end) = data_type.index_range(&SimpleTypeIndex::This)?;
//                 if let Offset::Finite(start_words) = start.words {
//                     let off = start_words + ix;
//                     if Offset::Finite(off) < end.words {
//                         Ok(VMWordLValue::Index(Box::new(ptr.compile_ptr_at(off, ctx)?), Box::new(VMWordRValue::Const(ix as u64))))
//                     } else {
//                         Err("bad pointer index".to_string())
//                     }
//                 } else {
//                     Err("bad pointer size".to_string())
//                 }
//             }
//         }
//     }
//     fn compile_ptr_at<C: CompileCtx<V>>(&self, ix: usize, ctx: &C) -> Result<VMPtrLValue, String> {
//         Err("not implemented".to_string())
//     }
// }
// 
// #[derive(PartialEq, Eq, Serialize, Deserialize, Debug, Clone)]
// enum TypedRValue<V> {
//     Copy(Box<TypedLValue<V>>),
//     ConstWord(u64),
//     PtrTag(Box<TypedLValue<V>>),
//     PtrLengthWord(Box<TypedLValue<V>>),
//     PtrLengthPtr(Box<TypedLValue<V>>),
//     FunPtr(V, V), // module, function
//     BinOp(WordBinOp, Box<TypedRValue<V>>, Box<TypedRValue<V>>),
//     UnOp(WordUnOp, Box<TypedRValue<V>>),
// }
// 
// impl<V> TypedRValue<V> {
//     fn get_type<C: TypecheckCtx<V>>(&self, ctx: &C) -> Result<SimpleType, String> {
//         match self {
//             Self::Copy(lval) => lval.get_type(ctx),
//             Self::ConstWord(_) => Ok(SimpleType::Word),
//             Self::PtrTag(lval) | TypedRValue::PtrLengthWord(lval) | TypedRValue::PtrLengthPtr(lval) => {
//                 if lval.get_type(ctx)? != SimpleType::Ptr {
//                     return Err("bad pointer type".to_string());
//                 }
//                 Ok(SimpleType::Word)
//             },
//             Self::FunPtr(_, _) => Ok(SimpleType::Ptr),
//             Self::BinOp(_, lhs, rhs) => {
//                 if lhs.get_type(ctx)? != SimpleType::Word || rhs.get_type(ctx)? != SimpleType::Word {
//                     return Err("bad word type".to_string());
//                 }
//                 Ok(SimpleType::Word)
//             },
//             Self::UnOp(_, val) => {
//                 if val.get_type(ctx)? != SimpleType::Word {
//                     return Err("bad word type".to_string());
//                 }
//                 Ok(SimpleType::Word)
//             }
//         }
//     }
//     fn compile_word_at<C: CompileCtx<V>>(&self, ix: usize, ctx: &C) -> Result<VMWordRValue, String> {
//         Err("not implemented".to_string())
//     }
//     fn compile_ptr_at<C: CompileCtx<V>>(&self, ix: usize, ctx: &C) -> Result<VMPtrRValue<V>, String> {
//         Err("not implemented".to_string())
//     }
// }
// 
// #[derive(PartialEq, Eq, Serialize, Deserialize, Debug, Clone)]
// enum AllocSpec<V> {
//     Full,
//     Tuple(Vec<AllocSpec<V>>),
//     DynamicArray(Box<TypedRValue<V>>),
//     TrailingUnion(usize, Box<AllocSpec<V>>)
// }
// 
// #[derive(PartialEq, Eq, Serialize, Deserialize, Debug, Clone)]
// enum AllocLength<V> {
//     Const(usize),
//     Dynamic(TypedRValue<V>)
// }
// 
// impl<V: Eq + Ord + Clone> AllocSpec<V> {
//     fn get_size(&self, typ: &SimpleType) -> Result<(AllocLength<V>, AllocLength<V>), String> {
//         match self {
//             Self::Full => {
//                 let counts = typ.size()?.to_counts()?;
//                 Ok((AllocLength::Const(counts.words), AllocLength::Const(counts.ptrs)))
//             }
//             Self::Tuple(specs) => {
//                 if let SimpleType::Tuple(typs) = typ {
//                     if specs.len() != typs.len() {
//                         return Err("bad tuple alloc spec".to_string());
//                     }
//                     let mut words: usize = 0;
//                     let mut ptrs: usize = 0;
//                     let mut word_addend = None;
//                     let mut ptr_addend = None;
//                     for (spec, typ) in specs.iter().zip(typs.iter()) {
//                         let (w, p) = spec.get_size(typ)?;
//                         match w {
//                             AllocLength::Const(ws) => {
//                                 if ws != 0 && word_addend.is_some() {
//                                     return Err("can't add items after a non-constant alloc size".to_string());
//                                 }
//                                 words += ws;
//                             },
//                             AllocLength::Dynamic(w) => {
//                                 if word_addend.is_some() {
//                                     return Err("bad tuple alloc spec".to_string());
//                                 }
//                                 word_addend = Some(w);
//                             }
//                         }
// 
//                         match p {
//                             AllocLength::Const(ps) => {
//                                 if ps != 0 && ptr_addend.is_some() {
//                                     return Err("can't add items after a non-constant alloc size".to_string());
//                                 }
//                                 ptrs += ps;
//                             },
//                             AllocLength::Dynamic(p) => {
//                                 if ptr_addend.is_some() {
//                                     return Err("bad tuple alloc spec".to_string());
//                                 }
//                                 ptr_addend = Some(p);
//                             }
//                         }
//                     }
//                     
//                     let tot_words = if let Some(w) = word_addend {
//                         AllocLength::Dynamic(TypedRValue::BinOp(WordBinOp::Add(false, true), Box::new(TypedRValue::ConstWord(words as u64)), Box::new(w)))
//                     } else {
//                         AllocLength::Const(words)
//                     };
//                     let tot_ptrs = if let Some(p) = ptr_addend {
//                         AllocLength::Dynamic(TypedRValue::BinOp(WordBinOp::Add(false, true), Box::new(TypedRValue::ConstWord(ptrs as u64)), Box::new(p)))
//                     } else {
//                         AllocLength::Const(ptrs)
//                     };
//                     Ok((tot_words, tot_ptrs))
//                 } else {
//                     Err("bad tuple type".to_string())
//                 }
//             },
//             Self::DynamicArray(len) => {
//                 if let SimpleType::Array(Offset::Infinite, elem_type) = typ {
//                     let counts = elem_type.size()?.to_counts()?;
//                     let words = if counts.words == 0 {
//                         AllocLength::Const(0)
//                     } else {
//                         // TODO: problem that len is repeated?
//                         AllocLength::Dynamic(TypedRValue::BinOp(WordBinOp::Mul(false, true), Box::new(TypedRValue::ConstWord(counts.words as u64)), len.clone()))
//                     };
//                     let ptrs = if counts.ptrs == 0 {
//                         AllocLength::Const(0)
//                     } else {
//                         AllocLength::Dynamic(TypedRValue::BinOp(WordBinOp::Mul(false, true), Box::new(TypedRValue::ConstWord(counts.ptrs as u64)), len.clone()))
//                     };
//                     Ok((words, ptrs))
//                 } else {
//                     Err("bad array type".to_string())
//                 }
//             },
//             Self::TrailingUnion(ix, spec) => {
//                 if let SimpleType::Union(typs) = typ {
//                     if *ix >= typs.len() {
//                         return Err("bad union index".to_string());
//                     }
//                     let (w, p) = spec.get_size(&typs[*ix])?;
//                     let words = match w {
//                         AllocLength::Const(w) => AllocLength::Dynamic(TypedRValue::ConstWord(w as u64)),
//                         AllocLength::Dynamic(w) => AllocLength::Dynamic(w),
//                     };
//                     let ptrs = match p {
//                         AllocLength::Const(p) => AllocLength::Dynamic(TypedRValue::ConstWord(p as u64)),
//                         AllocLength::Dynamic(p) => AllocLength::Dynamic(p),
//                     };
//                     Ok((words, ptrs))
//                 } else {
//                     Err("bad union type".to_string())
//                 }
//             }
//         }
//     }
// }
// 
// #[derive(PartialEq, Eq, Serialize, Deserialize, Debug, Clone)]
// enum TypedStatement<V> {
//     Assign(TypedLValue<V>, TypedRValue<V>),
//     AssignClone(TypedLValue<V>, TypedLValue<V>),
//     Swap(TypedLValue<V>, TypedLValue<V>),
//     Alloc(TypedLValue<V>, SimpleType, AllocSpec<V>), // dest, data type, spec
//     Call(V, V, Vec<TypedLValue<V>>, Vec<TypedLValue<V>>), // module, function, args, returns
//     CallPtr(TypedLValue<V>, Vec<TypedLValue<V>>, Vec<TypedLValue<V>>),
//     Return(Vec<TypedLValue<V>>),
//     If(TypedRValue<V>, Vec<TypedStatement<V>>),
//     Block(Vec<TypedStatement<V>>),
//     Continue(usize),
//     Break(usize)
// }
// 
// #[derive(PartialEq, Eq, Serialize, Deserialize, Debug, Clone)]
// struct TypedProcedure<V: Ord> {
//     name: V,
//     params: BTreeMap<V, SimpleType>,
//     locals: BTreeMap<V, SimpleType>, // must not conflict with params
//     returns: Vec<SimpleType>,
//     statements: Vec<TypedStatement<V>>,
// }
// 
// impl<V: Clone + Ord> TypedProcedure<V> {
//     fn compile(&self) -> Result<VMProcedure<V>, String> {
// 
//         let mut var_offsets: BTreeMap<V, TypedValueOffset> = BTreeMap::new();
//         let mut var_types: BTreeMap<V, SimpleType> = BTreeMap::new();
// 
//         let mut offset = TypedValueOffset::zero();
//         for (k, v) in &self.params {
//             var_types.insert(k.clone(), v.clone());
//             var_offsets.insert(k.clone(), offset);
//             offset = offset.add_checked(&v.size()?)?;
//         }
//         let param_counts = offset.to_counts()?;
// 
//         for (k, v) in &self.locals {
//             var_types.insert(k.clone(), v.clone());
//             var_offsets.insert(k.clone(), offset);
//             offset = offset.add_checked(&v.size()?)?;
//         }
//         let local_counts = offset.to_counts()? - param_counts;
// 
//         let mut ret_offsets: Vec<TypedValueOffset> = Vec::new();
//         let mut ret_offset = TypedValueOffset::zero();
//         for ret in &self.returns {
//             ret_offsets.push(ret_offset);
//             ret_offset = ret_offset.add_checked(&ret.size()?)?;
//         }
//         let return_counts = ret_offset.to_counts()?;
// 
//         let mut statements: Vec<VMStatement<V>> = Vec::new();
// 
//         Ok(VMProcedure {
//             name: self.name.clone(),
//             param_counts,
//             local_counts,
//             return_counts,
//             statements
//         })
//     }
// }
// 
// #[derive(PartialEq, Eq, Serialize, Deserialize, Debug, Clone)]
// struct TypedModule<V: Ord> {
//     name: V,
//     procedures: Vec<TypedProcedure<V>>,
// }
// 
// #[derive(PartialEq, Eq, Serialize, Deserialize, Debug, Clone)]
// struct TypedLibrary<V: Ord> {
//     modules: Vec<TypedModule<V>>,
// }
