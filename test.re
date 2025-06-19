import lib/std


fn main() {
    decl x: {int: <char>};
    x[2] = "nie";
    print(x[2]);
    print(x[1]);
}
