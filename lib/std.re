syscall 1: write(int, *char, int);

fn int2str(x_: int, buf_: *char) -> *char {
    if x_ < 10 {
        *buf_ = '0' + x_;
        return buf_ + 1;
    }
    buf_ = int2str(x_ / 10, buf_);
    *buf_ = '0' + (x_ % 10);
    return buf_ + 1;
}

fn print(x_: int) {
    decl buf: char[32];
    let end = int2str(x_, &buf);
    write(1, &buf, end - &buf);
}

fn print(x_: str) {
    write(1, *(&x_ as **char+1), x_ as int);
}

fn print(x_: bool) {
    if (x_) {
        print("true");
    } else {
        print("false");
    }
}

fn print(x_: char) {
    write(1, &x_, 1);
}
