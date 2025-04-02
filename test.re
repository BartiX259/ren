struct st {
    x: int,
    y: int,
    z: int
}

fn t(z: st) {
    print(z.z);
}

fn main() {
    let x = st {
        x: 10, y: 20, z: 30
    };
    t(x);
}
