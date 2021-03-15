use core::{
    convert::{TryFrom, TryInto},
    fmt,
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct VirtualAddress(usize);

impl VirtualAddress {
    pub const fn from_addr(addr: usize) -> Self {
        Self(addr)
    }

    pub const fn zero() -> Self {
        Self(0)
    }

    pub fn from_pointer<T>(ptr: *const T) -> Self {
        Self(ptr as usize)
    }

    pub fn from_ref<T>(rf: &T) -> Self {
        Self::from_pointer(rf as *const T)
    }

    pub const fn addr(&self) -> usize {
        self.0
    }

    pub fn pd_index(&self) -> usize {
        self.0 >> 22
    }

    pub fn pt_index(&self) -> usize {
        (self.0 >> 12) & (1024 - 1)
    }

    pub const fn as_ptr<T>(&self) -> *const T {
        self.0 as *const T
    }

    pub const fn as_mut_ptr<T>(&self) -> *mut T {
        self.0 as *mut T
    }
}

impl fmt::Debug for VirtualAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_fmt(format_args!("VirtualAddress({:#x})", self.addr()))
    }
}

impl<T> From<*const T> for VirtualAddress {
    fn from(ptr: *const T) -> Self {
        Self::from_pointer(ptr)
    }
}

impl<T> From<&T> for VirtualAddress {
    fn from(ptr: &T) -> Self {
        Self::from_ref(ptr)
    }
}

impl<T> From<&mut T> for VirtualAddress {
    fn from(ptr: &mut T) -> Self {
        Self::from_ref(ptr)
    }
}

impl TryFrom<usize> for VirtualAddress {
    type Error = core::convert::Infallible;
    fn try_from(u: usize) -> Result<Self, Self::Error> {
        Ok(Self(u))
    }
}
