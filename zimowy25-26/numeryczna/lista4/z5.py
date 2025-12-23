def newton_method1(x0,R, epsilon = 10**(-15)):
    n = 1
    while abs((x0 - 1/R)) > epsilon:
        x0 = x0 * (2 - x0 * R)
        n += 1

    return n,x0


tests  = [(1/(2*r), r) for r in range(2,10)] + [(r/2, (1/r)) for r in range(2,10)]

for x0,r in tests:
    print(newton_method1(x0,r), 1/r)



def newton_method2(x0,a, epsilon = 10**(-15)):
    n = 1
    while abs((x0 - 1/a**0.5)) > epsilon:
        x0 = 3/2 * x0 - a/2 * x0**3
        n += 1

    return n,x0


tests  = [(1/(2*r), r) for r in range(2,10)] + [(r/2, (1/r)) for r in range(2,10)]

for x0,a in tests:
    print(newton_method2(x0,a), a, 1/a**0.5)

#too large
tests  = [(3/(1*r), r) for r in range(2,10)] + [(r/2, (1/r)) for r in range(2,10)]

print('wrong starting points')
for x0,a in tests:
    print(newton_method2(x0,a), a, 1/a**0.5)