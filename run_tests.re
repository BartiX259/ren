import lib/std

fn main() {
    if exists("tmp.re") {
        rm("tmp.re");
    }
    for file in list_dir("tests/random")! {
        if cmp(file, ".") || cmp(file, "..") {
            continue;
        }
        let path = "tests/random/{file}";
        let content = read(path)!;
        write("tmp.re", content);
        print("pre {file}\n");
        run("/usr/bin/cargo", ["cargo", "run", "tmp.re"]);
        print("post {file}\n");
        run("./out", ["./out"]);
    }
    print("done\n");
}