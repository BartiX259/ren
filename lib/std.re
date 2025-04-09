syscall 1: write(int, ref[int], int);

fn print(x: int) {

}

fn print(x: str) {
    write(1, *(&x as ref[ref[int]]+1), x as int);
}

fn main() {
    print("asdfgask hjl");
}