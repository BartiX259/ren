import lib/json
import lib/std

fn main() {
    let test = to_json((a: 5));
    write("asd", test);
    decl a: (a: int);
    from_json(read("asd")!, &a)!;
    print(a);
}

