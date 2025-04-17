syscall 1: write(int, *char, int);

fn int2str(x: int, buf: *char) -> *char {
    if x < 10 {
        *buf = '0' + x;
        return buf + 1;
    }
    buf = int2str(x / 10, buf);
    *buf = '0' + (x % 10);
    return buf + 1;
}

fn print(x: int) {
    decl buf: char[32];
    let ptr = &buf;
    if x < 0 {
        *ptr = '-';
        ptr += 1;
        x = -x;
    }
    let end = int2str(x, ptr);
    write(1, &buf, end - &buf);
}

fn print(x: str) {
    write(1, x as *char, len(x));
}

fn print(x: bool) {
    if (x) {
        print("true");
    } else {
        print("false");
    }
}

fn print(x: char) {
    write(1, &x, 1);
}

fn print(x: *char) {
    let original_x = x;
    let len = 0;
    while *x != 0 {
        len += 1;
        x += 1;
    }
    write(1, original_x, len);
}

syscall 9: mmap(int, int, int, int, int, int) -> *any;
syscall 10: munmap(*any, int);

decl allocator: (base: *any, size: int, offset: int, stack: *any);
let SPACE_SIZE = 16;

fn alloc(size: int) -> *any {
    if allocator.base == null {
        allocator.base = mmap(0, *SPACE_SIZE, 3, 34, 0, 0);
        allocator.size = *SPACE_SIZE;
        allocator.offset = 0;
        allocator.stack = sp() + 16;
    }
    let new_offset = allocator.offset + size + 8;
    if new_offset > allocator.size {
        collect();
        new_offset = allocator.offset + size + 8;
        if new_offset > allocator.size {
            print("DOUBLE");
            *SPACE_SIZE *= 2;
            print('\n');
            let to_space = mmap(0, *SPACE_SIZE, 3, 34, 0, 0);
            copy(allocator.base, to_space, allocator.size);
            munmap(allocator.base, allocator.size);
            allocator.base = to_space;
            allocator.size = *SPACE_SIZE;
        }
    }
    let res = allocator.base + allocator.offset;
    *(res as *int) = size + 8;
    allocator.offset = new_offset;
    return res + 8;
}

fn collect() {
    print("gc");
    print('\n');
    print("base: {(allocator.base) as int}\n");

    let stack_end = sp();
    let stack_start = allocator.stack;
    let to_space = mmap(0, *SPACE_SIZE, 3, 34, 0, 0);
    let offset = 0;
    while stack_start > stack_end {
        stack_start -= 8;
        let ptr = *(stack_start as **any) - 8;
        if ptr >= allocator.base && ptr < allocator.base + allocator.size {
            print("found: ");
            print(ptr as int);
            print('\n');
            let size = *(ptr as *int);
            print("size: ");
            print(size);
            print('\n');
            copy(ptr, to_space + offset, size);
            *(stack_start as **any) = to_space + offset + 8;
            offset += size;
        }
    }
    munmap(allocator.base, allocator.size);
    allocator.base = to_space;
    allocator.offset = 0;
}
