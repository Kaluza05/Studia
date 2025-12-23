from math import pi, atan

def a(x):
    return x**3 + (x**6 + 2025)**0.5

def a2(x):
    if x > 0 : return a(x)
    else:
        return (-2025) / (x**3 - (x**6+2025)**0.5)
    
def b(x):
    return (atan(x) - x)/ x**3

def b2(x):
    if abs(x) <= 1/2:
        return -1/3 + x**2/5
    else:
        return b(x)
    
n = 5
for i in range(1,10**n, 10**(n-1)):
    print(-i, a(-i), a2(-i))


n = 15
#for i in range(1,tries):
#    #[1/2,3/2]
#    x = s + (e - s) *i/tries
#    print(round(x,4), round(b(x),8))
#print(list(b(s + (e - s) *i/tries) for i in range(1,tries) if s + (e - s) *i/tries != 0  ))
print(max((b(10**(-i)) for i in range(1,n))))

#print(list(b2(s + (e - s) *i/tries) for i in range(1,tries) if s + (e - s) *i/tries != 0  ))
print(max((b2(10**(-i)) for i in range(1,n))))