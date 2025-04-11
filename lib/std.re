syscall 1: write(int, *char, int);

fn print(x: int) {

}

fn print(x: str) {
    write(1, *(&x as **char+1), x as int);
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
