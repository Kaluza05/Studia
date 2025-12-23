from random import random

def pi_montecarlo(n : int)-> float:
    in_count = 0
    all_count = 0

    for _ in range(n):
        x = random() - 1/2
        y = random() - 1/2

        if x**2 + y**2 <= 1/4:
            in_count += 1
        all_count += 1

        #print(4 * in_count / all_count)

    return 4 * in_count / all_count

print(pi_montecarlo(10000))