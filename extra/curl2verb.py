# Tested on: Python 3.10.6

import argparse
import sys


def main() -> None:
    if len(sys.argv) < 2 or sys.argv[1] != "curl":
        print("usage: python3 curl2verb.py curl <url> <args...>", file=sys.stderr)
        exit(1)

    cmd = sys.argv[2:]

    parser = argparse.ArgumentParser()
    parser.add_argument("url")
    parser.add_argument("-H", action="append", dest="headers")
    parser.add_argument("-X", dest="verb", default="GET")
    parser.add_argument("--compressed", action="store_true")

    args = parser.parse_args(cmd)

    print(f"{args.verb.lower()} {args.url}")
    for header in args.headers:
        print(header)

    print()


if __name__ == "__main__":
    main()
