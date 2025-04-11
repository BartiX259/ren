import lib/std

type x = (x: str, y: int)
type res = int

fn main() {
    let z = (x: "asd", y: 69);
    b(z);
}

fn b(z: x) {
    print(z.x);
}
