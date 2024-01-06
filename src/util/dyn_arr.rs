use core::{ops::{Index, AddAssign}, fmt::Debug};

pub const DYN_ARR_CAPACITY: usize = 16;

pub trait DynArrElem: Default + Copy + Debug { }
impl<T: Default + Copy + Debug> DynArrElem for T { }

/// A dynamically sized array with a maximum capacity known at compile time.
/// Used in the early boot environment where allocation is not available yet.
#[derive(Default, Clone)]
pub struct DynArr<Elem: DynArrElem> {
    count: usize,
    buffer: [Elem; DYN_ARR_CAPACITY],
}

/// A `DynArray` iterator
pub struct DynArrIter<'arr, Elem: DynArrElem> {
    position: usize,
    array: &'arr DynArr<Elem>,
}

impl<Elem: DynArrElem> DynArr<Elem> {
    /// Creates a DynArray
    pub fn new() -> DynArr<Elem> {
        Default::default()
    }

    /// Appends an element to the array. Fails in case the capacity is reached.
    pub fn push(&mut self, elem: Elem) -> Result<(), ()> {
        if self.count == DYN_ARR_CAPACITY { return Err(()); }
        self.buffer[self.count] = elem;
        self.count += 1;
        Ok(())
    }

    /// Returns how many elements are in the array
    pub fn len(&self) -> usize {
        self.count
    }

    /// Creates an array iterator
    pub fn iter<'arr>(&'arr self) -> DynArrIter<'arr, Elem> {
        DynArrIter {
            position: 0,
            array: self
        }
    }
}

/// Index implementation
impl<Elem: DynArrElem> Index<usize> for DynArr<Elem> {
    type Output = Elem;
    fn index(&self, index: usize) -> &Self::Output {
        if index >= self.count {
            panic!("DynArr index ({index}) out of bounds ({})", self.count);
        }
        &self.buffer[index]
    }
}

/// Iterator implementation
impl<'arr, Elem: DynArrElem> Iterator for DynArrIter<'arr, Elem> {
    type Item = Elem;
    fn next(&mut self) -> Option<Self::Item> {
        if self.position == self.array.count { return None; }
        self.position += 1;
        Some(self.array[self.position - 1])
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let size = self.array.count - self.position;
        (size, Some(size))
    }
}

/// AddAssign implementation
impl<Elem: DynArrElem> AddAssign for DynArr<Elem> {
    fn add_assign(&mut self, rhs: Self) {
        for elem in rhs.iter() {
            if self.push(elem) == Err(()) { break; }
        }
    }
}

/// Debug implementation
impl<Elem: DynArrElem> Debug for DynArr<Elem> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "DynArr<{}> {{", core::any::type_name::<Elem>())?;
        for (i, elem) in self.iter().enumerate() {
            write!(f, "{:?}", elem)?;
            if i != self.count - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "}}")
    }
}
