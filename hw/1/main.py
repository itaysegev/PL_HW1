import random
import argparse
from w2 import process_words


def generate_test(file: str, s: str) -> None:
    with open(file, "w") as f:
        sets = process_words(s.split(";"))
        common = sets[0]
        for x in sets:
            common &= x
        f.write("\n".join(str(len(x)) for x in sets) + "\n")
        f.write("\n".join(common) + ("\n" if common else ""))


def main():
    args = argparse.ArgumentParser()
    args.add_argument("name")
    args = args.parse_args()
    suff = args.name
    chars = list("abcdefghijklmnopqrstuvwxyz:12-=+_') ")
    s = (
        ";".join(
            "".join(random.choices(chars, k=int(1 + random.expovariate(0.2))))
            for _ in range(int(1 + random.expovariate(0.1)))
        )
        + "."
    )
    with open(f"test-{suff}.in", "w") as f:
        f.write(s)
    generate_test(f"test-{suff}.out", s)


if __name__ == "__main__":
    main()
