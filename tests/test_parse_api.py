import sys

sys.path.insert(0, '.')
from unidef.__main__ import *


def try_parse(fm, to):
    args = parser.parse_args([f'examples/{fm}.yaml', '-t', to])
    config = CommandLineConfig.from_args(args)
    main(config, open(args.file).read())


def test_parse_model():
    for target in ['rust']:
        try_parse("model_example", target)


def test_parse_json_example():
    for target in ['rust']:
        try_parse("json_default_example", target)
        try_parse("api_example", target)
        try_parse("comments_example", target)


