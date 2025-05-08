import lib/std
fn main() {
    for x in 0..5 {
        let z = 3;
        if x == z {
            continue;
        }
        print(x);
    }
    print("out");
}

