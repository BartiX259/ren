struct x {
    x: int,
    y: int
}
fn main() {
    let z = [x {x: 1, y: 2}, x{x:3, y:4}];
    z[0].y = 3;
    print(z[1].x);
}
