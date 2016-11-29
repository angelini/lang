import inspect
import os
import sys
from subprocess import Popen, PIPE


def parse_single_output(block):
    lines = block.split('\n')
    return {
        'exp': lines[0][5:],
        'typ': lines[1][5:],
        'ret': lines[2][5:],
    }


def parse_output(out):
    return [parse_single_output(block.strip())
            for block in out.split('---')
            if block.strip()]


def read_until(p, marker):
    marker = marker + '\n'
    buf = ''
    while True:
        buf += p.stdout.read(1)
        if buf.endswith(marker):
            return buf[:-len(marker)]
        if p.poll() is not None:
            print('')
            print('===> Process Died')
            print(' from:', inspect.stack()[2][3])
            print('')
            print('  stdout:')
            print(buf)
            print('  stderr:')
            print(p.stderr.read())
            sys.exit(1)


def eval_exprs(p, exprs_str):
    with_eval = exprs_str + '\n====EXEC====\n'
    p.stdin.write(with_eval)
    p.stdin.flush()

    out = read_until(p, '====END====')
    return parse_output(out)


def assert_print(expected, actual):
    print('')
    print('===> Assertion Error')
    print(' from:', inspect.stack()[2][3])
    print('')
    print(' expected:', expected)
    print('      got:', actual)
    sys.exit(1)


def assert_type(out, expected):
    actual = out['typ'].lower()
    if actual != expected:
        assert_print(expected, actual)


def assert_val(out, expected):
    actual = out['ret'].lower()
    if actual != expected:
        assert_print(expected, actual)


def test_values(p):
    out = eval_exprs(p, """
        1
        "string"
        true
        nil
        [1, 2, 3]
        {"a": 2, "bb": 34}
        fn (a: int, b: str) { print(a, b) }
    """)
    assert_type(out[0], 'int')
    assert_type(out[1], 'str')
    assert_type(out[2], 'bool')
    assert_type(out[3], 'nil')
    assert_type(out[4], 'vec(int)')
    assert_type(out[5], 'map((str, int))')
    assert_type(out[6], 'fn(([int, str], nil))')


def test_if(p):
    out = eval_exprs(p, """
        if(eq(1, 1), {
            "true"
        }, {
            "false"
        })

        if(lt(10, 5), {
            3
        }, {
            5
        })
    """)
    assert_val(out[0], 'str("true")')
    assert_val(out[1], 'int(5)')


def test_while(p):
    out = eval_exprs(p, """
        let result = 0
        let i = 0

        while(lt(i, 10), {
            i = add(i, 1)
            result = add(result, 5)
            "test"
        })
        result
    """)
    assert_type(out[2], 'str')
    assert_val(out[3], 'int(50)')


def test_closures(p):
    out = eval_exprs(p, """
        let lambda = fn () {
            let closed = 4
            fn () {
                closed
            }
        }
        let inner = lambda()
        inner()
    """)
    assert_val(out[2], 'int(4)')


def test_immutability(p):
    out = eval_exprs(p, """
        let list = [1, 2, 3]
        push(list, 4)
        list

        let map = {"a": 1}
        insert(map, "b", 2)
        map
    """)
    assert_val(out[0], 'vec([int(1), int(2), int(3)])')
    assert_val(out[1], 'vec([int(1), int(2), int(3), int(4)])')
    assert_val(out[2], 'vec([int(1), int(2), int(3)])')

    assert_val(out[3], 'map({str("a"): int(1)})')
    assert_val(out[4], 'map({str("a"): int(1), str("b"): int(2)})')
    assert_val(out[5], 'map({str("a"): int(1)})')


def test_generics(p):
    out = eval_exprs(p, """
        let get = fn (coll: map[T, R], key: T) {
            mget(coll, key)
        }
        get({"a": 1}, "a")
    """)
    assert_type(out[1], 'int')
    assert_val(out[1], 'int(1)')


def run_tests(p):
    count = 0
    for test in [test_values,
                 test_if,
                 test_while,
                 test_closures,
                 test_immutability,
                 test_generics]:
        test(p)
        count += 1
        print('.', end='\n' if count % 8 == 0 else '')
    print()


if __name__ == '__main__':
    env = dict(list(os.environ.items()) + [("RUST_BACKTRACE", "1")])
    p = Popen(['cargo', 'run', '__stream__'], universal_newlines=True,
              stdout=PIPE, stdin=PIPE, stderr=PIPE, env=env)
    read_until(p, '====START====')
    run_tests(p)
