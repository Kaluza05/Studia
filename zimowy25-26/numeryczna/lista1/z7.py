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