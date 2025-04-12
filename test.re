import lib/std
import fib

type x = (x: str, y: int)
type res = int

fn main() {
    let z = (x: "asd", y: 69);
    b(z);
    print(fib(40));
}

fn b(z: x) {
    print(z.x);
}
