A = 35761448508455
B = 15492295473751

N = 119315717514047
R = 101741582076661


def dot(op1, op2):
    a, b = op1
    x, y = op2
    return (
        (a * x) % N,
        (a * y + b) % N
    )


def repeat(t, op):
    if t == 0:
        return (1, 0)
    if t == 1:
        return op
    x, y = divmod(t, 2)
    r = repeat(x, op)
    rr = dot(r, r)
    if y == 1:
        return dot(op, r)
    return rr


def inv(op):
    a, b = op
    a_ = pow(a, N - 2, N)
    b_ = -(a_ * b) % N
    return (a_, b_)


def apply(op, i):
    a, b = op
    return  (a * i + b) % N



op = repeat(R, (A, B))
print(op)

iv = inv(op)
print(inv)

res = apply(iv, 2020)
print(res)

