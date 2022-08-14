import sys

sys.path.insert(0, '.')
from unidef.__main__ import *


def try_parse(fm, to):
    args = parser.parse_args([f'examples/{fm}', '-l', 'javascript', '-t', to])
    config = CommandLineConfig.from_args(args)
    main(config, open(args.file).read())


def test_parse_js():
    for target in ['rust_lang']:
        try_parse("transpile_js_example.js", target)
