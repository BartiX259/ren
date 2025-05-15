import lib/std

enum animal {
    dog(<char>),
    cat,
    gif
}

fn main() {
    let c = animal.cat;
    if let animal.cat = c {
        print("cat");
    } else if let animal.dog(a) = c {
        print_stack(0, 12);
        print(a);
    } else {
        print("xaxa");
    }
}
