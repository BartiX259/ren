import lib/std

fn newmap<K, V>(x: K, y: V, z: (x: K, y: V)) {
    print(x);
    print(y);
    print(z.x);
    print(z.y);
}

fn newmap2(x: int, y: <char>, z: (x: int, y: <char>)) {
    print(x);
    print(y);
    print(z.x);
    print(z.y);
}

fn main() {
    newmap(5, "str", (x: 1, y: "asd"));
    print('\n');
    newmap2(5, "str", (x: 1, y: "asd"));
}

