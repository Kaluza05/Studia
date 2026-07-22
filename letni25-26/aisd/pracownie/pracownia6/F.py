from math import floor,sqrt,log2
from random import randint

Q = 9_999_991

def find_params(n,nums):
    m = floor(n/log2(n))#floor(sqrt(n))
    A = randint(1,5*10**7)
    as_list = [1] * m
    
    inv = dict()

    flag = True
    while flag:
        for l in nums:
            g = ((A*l) % Q % m)
            inv[l] = g

        buckets = [[] for _ in range(m)]

        for l in nums:
            g = inv[l]
            buckets[g].append(l)

        flag = False
        for g in range(m):
            if len(buckets[g]) > 2 * log2(n):
                flag = True
                A = randint(1,5*10**7)




    ys = [3*len(b) for b in buckets]

    final_ys = [0]*m
    #get them info buckets
    # print(buckets)
    for g,b in enumerate(buckets):
        fails = 0
        # print('bucket: ',g,b, ys[g])
        
        while True: #if less then we have repreats
            curr_bucket = set()
            # print(fails)
            flag = True
            for v in b:
                p = (as_list[g] * v) % Q % ys[g]
                if p in curr_bucket:
                    # print(len(curr_bucket),p)
                    as_list[g] = randint(10,10**4)
                    flag = False
                    fails += 1
                    
                    break
                curr_bucket.add(p)

            if flag:
                break

        final_y = [0]*ys[g]
        for v in b:
            final_y[(as_list[g] * v) % Q % ys[g]] = v
        
        final_ys[g] = final_y
        # if buckets[g] >= 2:
        #     as_list[g] = randint(10,10**4)
            #jakis reset
    # print('final ys', final_ys)


    return m,ys,A,as_list

import random

def generate_task():
    N = random.randint(10**4,2*10**4)
    
    ls = [random.randint(1,5*10**6) for _ in range(N)]
    
    ls = list(set(ls))

    with open('input_rand.txt', 'w') as out:
        out_string = str(N) + '\n' + ' '.join(map(str,ls))
        out.write(out_string)

def solve():

    with open('input_rand.txt','r') as inp:
        data=inp.read().splitlines()
        N = int(data[0])
        nums = [int(i) for i in data[1].split()]

        # print(N,nums)
        # mozna tez zrobić proces wybierania m dwuktornie i poprawiania A

        m,ys,A,as_list = find_params(N,nums)

        print('sum check',sum(ys), 3*N, sum(ys) <= 3*N)

        #final check
        existing = set()
        for l in nums:
            q = ((A*l) % Q % m)
            p = (as_list[q] * l) % Q % ys[q]
            if (q,p) in existing:
                print('blad')
                break
            else:
                existing.add((q,p))

        # print(m)
        # print(' '.join(map(str,ys)))
        # print(A)
        # print(' '.join(map(str,as_list)))

# for i in range(100):
#     print(f'proba {i}')
#     generate_task()
solve()

