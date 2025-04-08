struct t {
    yo: int,
    b: int
}

fn a (y: int, x: t) -> t {
    x.yo = y;
    return x;
}

fn main() {
    let x = a(60, t {
        yo: 5, b: 1
    });
    print(a(4, x).yo);
}