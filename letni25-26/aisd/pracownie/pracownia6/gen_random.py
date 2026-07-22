import random

def generate_task():
    N = random.randint(10**4,2*10**4)
    
    ls = [random.randint(1,5*10**6) for _ in range(N)]
    
    ls = list(set(ls))

    with open('input_rand.txt', 'w') as out:
        out_string = str(N) + '\n' + ' '.join(map(str,ls))
        out.write(out_string)

generate_task()