import lib/std

fn main(argc: int, argv: **char) {
    for let i = 0; i < argc; i += 1 {
        print(argv[i]);
    }
}
