curr_fact = 1
curr_exp = 10**15
N = 10**4
for i in range(1,10**4,2):
    curr_fact *= i * (i+1)
    curr_exp *= 10
    print(curr_fact,curr_exp)
    if curr_fact > curr_exp:
        print(i)
        break

def my_cos(x):
    #calculates cos from [-pi,pi]
    x = abs(x)
    pow = 1
    fact = 1
    cos_sum = 1
    for i in range(1,14):
        sign = 1 if i % 2 == 0 else -1
        pow *= x**2
        i= 2*i
        fact *= (i-1) * (i)
        cos_sum += sign * pow / fact

    return cos_sum

from math import pi,cos

arg = pi
print('mine: ',my_cos(arg))
print('normal: ',cos(arg))
print('difference: ', abs(cos(arg) - my_cos(arg)))