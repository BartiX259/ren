struct small {
    x: int,
    y: int
}
fn get_big(b: int) -> small {
    let x = small { x: 10, y: 50 };
    if b == 1 {
        return x;
    }
    return small { x: 6, y: 20 };
}

fn set(s: small) {
    print(s.x);
}

fn main() {
    set(get_big(1));
    let x = get_big(0);
    print(x.x);
}
