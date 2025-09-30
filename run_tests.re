import lib/std

fn main() -> int {
    let success = true;
    run("/usr/bin/cargo", ["cargo", "build"])!;
    for file in list_dir("tests/suite")! {
        if cmp(file, ".") || cmp(file, "..") {
            continue;
        }
        let path = "tests/suite/{file}";
        let content = read(path)!;
        write("tmp.re", content);
        print("pre {file}\n");
        let compiler_code = run("./target/debug/renc", ["renc", "tmp.re", "-o", "tmp"])!;
        print("post {file}\n");
        if compiler_code != 0 {
            print("Compiler fail! (code {compiler_code})\n");
            success = false;
            continue;
        }
        let out_code = run("./tmp", ["./tmp"])!;
        if out_code != 0 {
            print("Compiler fail! (code {out_code})\n");
            success = false;
            continue;
        }
    }
    rm("tmp.re");
    rm("tmp");
    if success {
        print("Tests passed!\n");
        return 0;
    } else {
        print("A test failed!\n");
        return 1;
    }
}