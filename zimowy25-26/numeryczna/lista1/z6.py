from math import pi
def sum_pi(n):
    s = 0
    for k in range(n):
        s += 4 * (-1)**k / (2*k + 1)
        if abs(s - pi) < 10** (-6):
            print(k)
            break
    return s

n = 10**8
s = 0
for k in range(n):
    s += 4 * (-1)**k / (2*k + 1)
    if abs(s - pi) < 10** (-6):
        print(k,s,s-pi)
        break


#na kartce wyszlo 2 * 10 ^6, ale powinno to być mniej