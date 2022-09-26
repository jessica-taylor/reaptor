use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
enum SimpleType {
    Word,
    Ptr,
    Tuple(Vec<SimpleType>),
    Array(usize, Box<SimpleType>),
    Union(Vec<SimpleType>),
}

impl SimpleType {
    fn size_words(&self) -> usize {
        match self {
            SimpleType::Word => 1,
            SimpleType::Ptr => 0,
            SimpleType::Tuple(t) => t.iter().map(|x| x.size_words()).sum(),
            SimpleType::Array(n, t) => n * t.size_words(),
            SimpleType::Union(t) => t.iter().map(|x| x.size_words()).max().unwrap(),
        }
    }
    fn size_ptrs(&self) -> usize {
        match self {
            SimpleType::Word => 0,
            SimpleType::Ptr => 1,
            SimpleType::Tuple(t) => t.iter().map(|x| x.size_ptrs()).sum(),
            SimpleType::Array(n, t) => n * t.size_ptrs(),
            SimpleType::Union(t) => t.iter().map(|x| x.size_ptrs()).max().unwrap(),
        }
    }
}

enum SimpleTypeIndex {
    This,
    TupleElem(usize, Box<SimpleTypeIndex>),
    ArrayElem(usize, Box<SimpleTypeIndex>),
    UnionElem(usize, Box<SimpleTypeIndex>),
}

impl SimpleType {
    fn index(&self, ix: &SimpleTypeIndex) -> Option<&SimpleType> {
        match (self, ix) {
            (_, SimpleTypeIndex::This) => Some(self),
            (SimpleType::Tuple(t), SimpleTypeIndex::TupleElem(i, rest)) => t.get(*i)?.index(rest.as_ref()),
            (SimpleType::Array(ref n, t), SimpleTypeIndex::ArrayElem(ref i, rest)) => {
                if i < n {
                    t.index(rest.as_ref())
                } else {
                    None
                }
            },
            (SimpleType::Union(t), SimpleTypeIndex::UnionElem(i, rest)) => t.get(*i)?.index(rest.as_ref()),
            _ => None,
        }
    }
    fn index_range(&self, ix: &SimpleTypeIndex) -> Option<((usize, usize), (usize, usize))> {
        match (self, ix) {
            (_, SimpleTypeIndex::This) => Some(((0, self.size_words()), (0, self.size_ptrs()))),
            (SimpleType::Tuple(t), SimpleTypeIndex::TupleElem(i, rest)) => {
                let mut word_offset = 0;
                let mut ptr_offset = 0;
                for (j, x) in t.iter().enumerate() {
                    if j == *i {
                        let ((word_start, word_end), (ptr_start, ptr_end)) = x.index_range(rest.as_ref())?;
                        return Some(((word_start + word_offset, word_end + word_offset), (ptr_start + ptr_offset, ptr_end + ptr_offset)));
                    }
                    word_offset += x.size_words();
                    ptr_offset += x.size_ptrs();
                }
                None
            },
            (SimpleType::Array(n, t), SimpleTypeIndex::ArrayElem(i, rest)) => {
                if i < n {
                    let word_offset = i * t.size_words();
                    let ptr_offset = i * t.size_ptrs();
                    let ((word_start, word_end), (ptr_start, ptr_end)) = t.index_range(rest.as_ref())?;
                    Some(((word_start + word_offset, word_end + word_offset), (ptr_start + ptr_offset, ptr_end + ptr_offset)))
                } else {
                    None
                }
            },
            (SimpleType::Union(t), SimpleTypeIndex::UnionElem(i, rest)) => {
                t.get(*i)?.index_range(rest.as_ref())
            },
            _ => None
        }
    }
}
