import itertools as it


def has_straigth(s):
    if len(s) < 3:
        return False
    if ord(s[0]) + 1 == ord(s[1]) and ord(s[1]) + 1 == ord(s[2]):
        return True
    return has_straigth(s[1:])

def has_forbidden(s):
    return any(f in s for f in 'iol')

def has_pairs(s):
    pairs = set(p for p in zip(s, s[1:]) if p[0] == p[1])
    return len(pairs) > 1

def good_password(s):
    return has_straigth(s) and not has_forbidden(s) and has_pairs(s)

def next_string(s):
    init = s[:-1]
    last = s[-1]
    if last == 'z':
        last = 'a'
        init = next_string(init)
    else:
        last = chr(ord(last) + 1)
    return init + last

def next_good_password(pw):
    pw = next_string(pw)
    while not good_password(pw):
        pw = next_string(pw)
    return pw


pw = 'vzbxkghb'
gpw = next_good_password(pw)
print(gpw)
print(next_good_password(gpw))
