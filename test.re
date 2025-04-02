
fn main() {
    let x = [1, 2, 3];
    let y = 5;
    *(&y) += 1;
    print(x[2]);
    print(y);
}
