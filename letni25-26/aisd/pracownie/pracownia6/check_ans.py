from math import floor,sqrt,log2
from random import randint

Q = 9_999_991

def check_params(n,nums,m,ys,A,as_list):
    existing = set()
    for l in nums:
        # print(n,nums,m,ys,A,as_list)
        # print(f'typy n : {type(n)},nums : {type(nums)}, {type(nums[0])}, m : {type(m)}')
        # print(f'reszta ys : {type(ys)}, {type(ys[0])}, A : {type(A)}, as : {type(as_list)}, {type(as_list[0])}')
        g = ((A*l) % Q % m)
    
        p = (as_list[g] * l) % Q % ys[g]
        if (g,p) in existing:
            print('btroken')
        else:
            existing.add((g,p))

    print('suma sie zgadza', sum(ys) <= 3*n)

    print('A mniejsze', A <= 5*10**7)
    for a in as_list:
        if a > 5*10**7:
            print('zle a', a)

with open('input3.txt', 'r') as inp, open('ans.txt','r') as out:
    data_inp = inp.read().splitlines()
    data_out = out.read().splitlines()
    N = int(data_inp[0])
    nums = [int(i) for i in data_inp[1].split()]
    # print(N,nums)
    # mozna tez zrobić proces wybierania m dwuktornie i poprawiania A
    m = int(data_out[0])
    ys = list(map(int,data_out[1].split()))
    A = int(data_out[2])
    as_list = list(map(int,data_out[3].split()))

    print(N,nums)
    print(m,ys,A,as_list)

    check_params(N,nums,m,ys,A,as_list)

