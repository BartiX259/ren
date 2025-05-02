import lib/std

fn t() {
    let x = read("test.re") else err {
        print("ERROR: {err}");
        return;
    };
    print(x);
    print(len(x));
}

fn main() {
    t();
}

