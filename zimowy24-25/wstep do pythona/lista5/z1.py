def colatz(n):
    if n%2==0:
        return n//2
    else:
        return 3*n+1
    
def energia(n,dl=0):
    if n==1:
        return dl
    return energia(colatz(n),dl+1)

def analiza_colatza(a,b):
    energia_liczb = [energia(i) for i in range(a,b+1)]
    print(max(energia_liczb),min(energia_liczb),sum(energia_liczb)/len(energia_liczb),median(energia_liczb))
    
def median(lst):
    n = len(lst)
    lst.sort()
    if n%2==0:
        return (lst[n//2] + lst[n//2 -1])/2
    return lst[n//2]

analiza_colatza(1,30)