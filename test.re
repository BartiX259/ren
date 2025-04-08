struct big {
    x: int
}
fn get_big(b: int) -> big {
    let x = big { x: 10 };
    if b == 1 {
        return x;
    }
    return big { x: 6 };
}

fn set(s: big) {
    print(s.x);
}

fn main() {
    set(get_big(1));
    let x = get_big(0);
    set(x);
}
