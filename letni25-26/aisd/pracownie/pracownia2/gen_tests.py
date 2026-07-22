from random import randint

for i in range(3):
    with open(f'inputs_{i}.txt','w') as out:
        n = randint(1000,2000)
        while True:
            nums = [randint(0,200) for _ in range(n)]
            if sum(nums) <= 10**6:
                nums = ' '.join(str(i) for i in nums)
                break

        output = str(n) + '\n' + nums
        out.write(output)