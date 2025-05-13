import lib/std

enum animal {
    dog(<char>),
    cat
}

fn main() {
    let a = animal.dog("asd");
    print(a == animal.cat);
}
