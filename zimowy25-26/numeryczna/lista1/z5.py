from math import log

I0 = log(2026/2025)

def In(n,I_n1):
    return 1/n - 2025 * I_n1

def consec(I0 = I0):
    print(0,I0)
    for i in range(1,21):
        I0 = In(i,I0)
        print(i, I0)

def even(I0 = I0):
    print(0,I0)
    for i in range(2,21,2):
        I0 = 1/i - 2025 / (i-1) + 2025 ** 2 * I0
        print(i, I0)

def uneven(I0 = I0):
    I1 = 1 - 2025* I0
    print(1,I1)
    for i in range(3,20,2):
        I1 = 1/i - 2025 / (i-1) + 2025 ** 2 * I1
        print(i, I1)

consec()
print('\neven\n')
even()
print('\nodd\n')
uneven()

