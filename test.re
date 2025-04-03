struct st {
    x: int,
    y: int,
    z: int
}

fn t(z: st) {
    print(z.x);
}

fn main() {
    let x = st {
        x: 10, y: 20, z: 30
    };
    let y = 2;
    x.x <<= y;
    t(x);
}
