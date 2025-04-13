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
    write(1, x as *char, x as int);
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
