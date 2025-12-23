from itertools import product
import matplotlib.pyplot as plt


def generate_all_nums():
    combinations = ((s,*r) for s in (1,-1) for r in (product([0,1], repeat = 5)))
    #s-sign , r- e2,e3,e4,e5,c
    all_nums = set()
    e1 = 1
    for s,e2,e3,e4,e5,c in combinations:
        eis = [e1,e2,e3,e4,e5]
        mantisa = sum([ei / 2**pow for ei, pow in zip(eis, range(1,6))])
        
        num1 = s * mantisa * 2**c
        num2 = s * mantisa * 2**(-c)

        all_nums.add(num1)

        if c != 0:
            all_nums.add(num2)

    return all_nums

nums = generate_all_nums()
print(len(nums))
print(sorted(nums))
#przedzial najmniejszy [-1.9375,1.9375]

xs = sorted(nums)
ys = [0]*len(nums)

plt.scatter(xs,ys, s = 2)
plt.show()