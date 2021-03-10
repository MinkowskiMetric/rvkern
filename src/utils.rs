pub const PAGE_SIZE: usize = 4096;

pub fn align_down(addr: usize, align: usize) -> usize {
    if align.is_power_of_two() {
        addr & !(align - 1)
    } else if align == 0 {
        addr
    } else {
        panic!("`align` must be a power of 2");
    }
}

/// Align upwards. Returns the smallest x with alignment `align`
/// so that x >= addr. The alignment must be a power of 2.
pub fn align_up(addr: usize, align: usize) -> usize {
    align_down(addr + align - 1, align)
}

pub fn round_down_to_page(addr: usize) -> usize {
    align_down(addr, PAGE_SIZE)
}

pub fn round_up_to_page(addr: usize) -> usize {
    align_up(addr, PAGE_SIZE)
}
