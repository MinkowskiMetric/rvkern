pub const PAGE_SIZE: usize = 4096;

pub fn round_down_to_page(addr: usize) -> usize {
    addr & !(PAGE_SIZE - 1)
}

pub fn round_up_to_page(addr: usize) -> usize {
    round_down_to_page(addr + PAGE_SIZE - 1)
}
