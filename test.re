import lib/std

fn main() {
    let names = ["A", "B"];
    let scores = [99, 88, 77];
    for name, score in zip(names, scores) {
        print("{name}: {score}\n");
    }
}
