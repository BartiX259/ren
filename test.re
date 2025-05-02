import lib/std

fn t() {
    let x = read("test.rea") else err {
        print("ERROR: {err}");
        return;
    };
    print(x);
}

fn main() {
    t();
}

