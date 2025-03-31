struct x {
    x: int,
    y: arr[int, 2]
}
fn main() {
    let z = x { x: 10, y: [3, 6] };
    print(z.x);
    z.y[0] = 1;
    print(z.y[1]);
}
