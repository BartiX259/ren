import lib/std

fn t() -> int ? <char> {
    let x = read("test.re")?;
    print_stack(0, 20);
    print(x);
}

fn main() {
    t();
}

