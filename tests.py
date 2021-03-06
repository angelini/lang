import inspect
import os
import sys
from subprocess import Popen, PIPE


def parse_single_output(block):
    lines = block.split('\n')
    if lines[1][:3] == 'err':
        return {
            'exp': lines[0][5:],
            'err': lines[1][5:],
        }
    else:
        return {
            'exp': lines[0][5:],
            'typ': lines[1][5:],
            'ret': lines[2][5:],
        }


def parse_output(out):
    return [parse_single_output(block.strip())
            for block in out.split('---')
            if block.strip()]


def read_until(p, marker, d=1):
    marker = marker + '\n'
    buf = ''
    while True:
        buf += p.stdout.read(1)
        if buf.endswith(marker):
            return buf[:-len(marker)]
        if p.poll() is not None:
            print('')
            print('===> Process Died')
            print(' from:', inspect.stack()[d][3])
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

    out = read_until(p, '====END====', d=2)
    return parse_output(out)


def reset_env(p):
    p.stdin.write('====RESET====\n')
    p.stdin.flush()


def assert_print(expected, actual):
    print('')
    print('===> Assertion Error')
    print(' from:', inspect.stack()[2][3])
    print('')
    print(' expected:', expected)
    print('   actual:', actual)
    sys.exit(1)


def assert_type(out, expected):
    actual = out['typ'].lower()
    if actual != expected:
        assert_print(expected, actual)


def assert_val(out, expected):
    actual = out['ret'].lower()
    if actual != expected:
        assert_print(expected, actual)


def assert_err(out, expected):
    actual = out['err'].lower()
    actual = actual[actual.index('expected:'):]
    if actual != expected:
        assert_print(expected, actual)


def test_values(p):
    out = eval_exprs(p, """
        1
        "string"
        true
        nil
        [1, 2, 3]
        (1, "b", true)
        {"a": 2, "bb": 34}
        fn (a: int, b: str) { print((a, b)) }
    """)
    assert_type(out[0], 'int')
    assert_type(out[1], 'str')
    assert_type(out[2], 'bool')
    assert_type(out[3], 'nil')
    assert_type(out[4], 'list(int)')
    assert_type(out[5], 'tuple([int, str, bool])')
    assert_type(out[6], 'map((str, int))')
    assert_type(out[7], 'fn(([int, str], nil))')


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
    assert_val(out[0], 'list([int(1), int(2), int(3)])')
    assert_val(out[1], 'list([int(1), int(2), int(3), int(4)])')
    assert_val(out[2], 'list([int(1), int(2), int(3)])')

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


def test_tuple_getters(p):
    out = eval_exprs(p, """
        let t = (0, "a", true)
        t0(t)
        t1(t)
        t2(t)
    """)
    assert_val(out[1], 'int(0)')
    assert_val(out[2], 'str("a")')
    assert_val(out[3], 'bool(true)')


def test_type_error(p):
    out = eval_exprs(p, """
        let a = 1
        a = "a"
    """)
    assert_err(out[1], 'expected: int actual: str')

    out = eval_exprs(p, """
        let l: list[T] = {"a": 1}
    """)
    assert_err(out[0], 'expected: list(var("t")) actual: map((str, int))')


def test_argument_error(p):
    out = eval_exprs(p, """
        let func = fn(x: int) { x }
        func(1, 2)
    """)
    assert_err(out[1], 'expected: 1 actual: 2')


def test_bind_unknowns(p):
    out = eval_exprs(p, """
        if(true, [1], [])
        if(true, [], [1])
    """)
    assert_type(out[0], 'list(int)')
    assert_type(out[1], 'list(int)')


def test_recursion(p):
    out = eval_exprs(p, """
        let rec = fn (x: int) {
            if( eq(x, 1), {
                1
            }, {
                add(rec(sub(x, 1)), 1)
            })
        }
        rec(5)
    """)
    assert_type(out[0], 'fn(([int], int))')
    assert_val(out[1], 'int(5)')


def run_tests(p):
    count = 0
    tests = [v for (k, v) in globals().items() if k.startswith('test_')]
    for test in tests:
        test(p)
        count += 1
        print('.', end='\n' if count % 8 == 0 else '')
        reset_env(p)
    print()


if __name__ == '__main__':
    env = dict(list(os.environ.items()) + [("RUST_BACKTRACE", "1")])
    p = Popen(['cargo', 'run', '__stream__'], universal_newlines=True,
              stdout=PIPE, stdin=PIPE, stderr=PIPE, env=env)
    read_until(p, '====START====')
    run_tests(p)
