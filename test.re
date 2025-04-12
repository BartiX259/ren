import lib/std

type x = (x: str, y: int)
type res = int

fn main() {
    let z = ("asd", 69) as x;
    b(z);
}

fn b(z: x) {
    print(z.y);
    print(z.x);
}
