syscall 1: write(int, *int, int);

fn print(x: int) {

}

fn print(x: str) {
    write(1, *(&x as **int+1), x as int);
}
