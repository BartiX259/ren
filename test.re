import lib/std

fn main() {
    decl list: [(x: int, y: (int, <char>))];
    for i in 0..100 {
        let s = "q{i}";
        push(&list, (x: i, y: (100 - i, s[..])));
    }
    for x, y in list {
        let i, str = y;
        print(x + i);
        print(str);
        print('\n');
    }
}
