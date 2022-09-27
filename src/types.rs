use std::collections::BTreeMap;
use std::ops;

use serde::{Deserialize, Serialize};

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

enum SimpleTypeIndex<I> {
    This,
    TupleElem(usize, Box<SimpleTypeIndex<I>>),
    ArrayElem(I, Box<SimpleTypeIndex<I>>),
    UnionElem(usize, Box<SimpleTypeIndex<I>>),
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
    fn index<I>(&self, ix: &SimpleTypeIndex<I>) -> Result<&SimpleType, String> {
        match (self, ix) {
            (_, SimpleTypeIndex::This) => Ok(self),
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

enum TypedLValue {
    Local(SimpleType, TypedValueOffset),
    Global(SimpleType, TypedValueOffset),
    TupleIndex(SimpleType, Box<TypedLValue>, usize),
    ArrayIndex(SimpleType, Box<TypedRValue>),
    UnionIndex(SimpleType, Box<TypedLValue>, usize),
    DerefPtr(SimpleType, Box<TypedLValue>),
}

impl TypedLValue {
    fn get_type(&self) -> Result<&SimpleType, String> {
        match self {
            TypedLValue::Local(t, _) => Ok(t),
            TypedLValue::Global(t, _) => Ok(t),
            TypedLValue::TupleIndex(tup_typ, tup, ix) => tup.get_type()?.index::<usize>(&SimpleTypeIndex::TupleElem(*ix, Box::new(SimpleTypeIndex::This))),
            TypedLValue::ArrayIndex(arr_typ, arr) => arr.get_type()?.index::<usize>(&SimpleTypeIndex::ArrayElem(0, Box::new(SimpleTypeIndex::This))),
            TypedLValue::UnionIndex(union_typ, union, ix) => union.get_type()?.index::<usize>(&SimpleTypeIndex::UnionElem(*ix, Box::new(SimpleTypeIndex::This))),
            TypedLValue::DerefPtr(typ, ptr) => {
                if ptr.get_type()? != &SimpleType::Ptr {
                    return Err("bad pointer type".to_string());
                }
                Ok(typ)
            }
        }
    }
}

enum TypedRValue {
    Copy(Box<TypedLValue>),
    Clone(Box<TypedLValue>),
    PtrTag(Box<TypedLValue>),
    PtrLengthWord(Box<TypedLValue>),
    PtrLengthPtr(Box<TypedLValue>),
    Closure(usize, usize, Vec<(TypedValueOffset, TypedRValue)>), // module, function, args, rets, curries
}

impl TypedRValue {
    fn get_type(&self) -> Result<&SimpleType, String> {
        match self {
            TypedRValue::Copy(lval) => lval.get_type(),
            TypedRValue::PtrTag(lval) | TypedRValue::PtrLengthWord(lval) | TypedRValue::PtrLengthPtr(lval) => {
                if lval.get_type()? != &SimpleType::Ptr {
                    return Err("bad pointer type".to_string());
                }
                Ok(&SimpleType::Word)
            },
            TypedRValue::Clone(lval) => lval.get_type(),
            TypedRValue::Closure(_, _, curries) => {
                for (offset, curry) in curries.iter() {
                    curry.get_type()?;
                }
                Ok(&SimpleType::Ptr)
            }
        }
    }
}

