def get_roots(a,b,c):
    print(b**2 - 4 * a * c)
    assert a != 0 and b**2 - 4 * a * c >= 0
    delta = (b**2 - 4 * a * c)**0.5
    x1 = (-b + delta) / (2*a)
    x2 = (-b - delta) / (2*a)
    return x1,x2

def get_roots2(a,b,c):
    assert a != 0 and b**2 - 4 * a * c >= 0

    delta = (b**2 - 4 * a * c)**0.5
    if b > 0:
        x1 = -2*c / (b + delta)
        x2 = (-b - delta) / (2*a)
    else: 
        x1 = (-b + delta) / (2*a)
        x2 = 2*c / (-b + delta)

    return x1,x2

a,b,c = 10**(-30),2,-3
#ax^2 + bx + c
print(get_roots(a,b,c))
print(get_roots2(a,b,c))
#widać, że 0 nie jest rozwiazaniem bo dla 0 to jest mniej wiecej -3