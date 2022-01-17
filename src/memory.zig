pub fn grow_capacity(capacity: usize) usize {
    if (capacity < 8) {
        return 8;
    } else {
        return capacity * 2;
    }
}
