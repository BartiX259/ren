import lib/std

fn main() {
    let path = "ttt";
    let x = read(path) else +"";
    print(x);
    print(len(x));
    push(&x, "alo\n");
    write(path, x);
}

