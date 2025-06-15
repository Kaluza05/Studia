

def rep_2(n):
    k=2
    wyn = []
    while n>0:
        wyn.append(n%k)
        n=n//k
        k+=1
    print(wyn[::-1])

print(rep_2(100))