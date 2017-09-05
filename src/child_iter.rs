use std::slice;

/// Iterator over parented slices.
#[derive(Clone, Debug)]
pub struct ChildIter<'a, T: ChildBuilder<'a>> {
    /// The parent struct.
    parent: T::Parent,

    /// The internal iterator.
    iter: slice::Iter<'a, T::Json>,
}

impl<'a, T: 'a + ChildBuilder<'a>> ChildIter<'a, T> {
    pub fn new(parent: T::Parent, iter: slice::Iter<'a, T::Json>) -> ChildIter<'a, T> {
        ChildIter {
            parent: parent,
            iter: iter,
        }
    }
}

impl<'a, T: 'a> Iterator for ChildIter<'a, T>
where
    T: ChildBuilder<'a>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(
            |json| T::new(self.parent.clone(), json),
        )
    }
}

pub trait ChildBuilder<'a> {
    type Parent: 'a + Clone;
    type Json: 'a;

    fn new(Self::Parent, &'a Self::Json) -> Self;
}
