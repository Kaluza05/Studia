def change_max_min(a,b):
    if a > b:
        return a,b
    else:
        return b,a

def choose_highest(t1,t2,t3):
    valid = [t for t in [t1,t2,t3] if t[0]]
    max_height = -1,None
    if valid == []:
        return False,0,0
    for t in valid:
        if t[2] > max_height[0]:
            max_height = t[2],t
    
    # print(max_height)
    return True,max_height[1][1],max_height[1][2]
    #wszystkie maja tą samą roznice wiec obojetnie 

def make_dp(n,hs):
    #len(hs) = n
    h_sum = sum(hs) // 2 + 1
    dp = [[(False,0,0) for _ in range(h_sum+1)] for _ in range(n)]

    #dp[i][j] - z pierwszych i klockow udalo się uzyskać wieże roznicy j
    #jedna optymalizacja, wieże mozna budować max do wysokości sum(hs[:i]) w i tej iteracji


    dp[0] = [(True,j,0) if j == hs[0] or j == 0 else (False,0,0) for j in range(h_sum+1)]

    for i in range(1,n):
        dp[i][hs[i]] = (True,hs[i],0)
        for j in range(h_sum+1):
            t1 = dp[i-1][j]
            b2,h21,h22 = dp[i-1][j+hs[i]] if j + hs[i] < h_sum else (False,0,0)
            b3,h31,h32 = dp[i-1][abs(j-hs[i])]

            h21,h22 = h21,h22 + hs[i]
            
            h31,h32 = (h31 + hs[i],h32) if j - hs[i] >= 0 else (h31, h32 + hs[i])
    
            h31,h32 = change_max_min(h31,h32)
            
            t2 = b2,h21,h22
            t3 = b3,h31,h32

            dp[i][j] = choose_highest(t1,t2,t3)

    for i,t in enumerate(dp[-1]):
        b,h1,h2 = t
        if b == 0 or h1 == 0 or h2 == 0:
            continue
        else:
            if i == 0:
                return 'TAK',h1
            else:
                return 'NIE',h1 - h2
            
print(make_dp(4,[1,2,3,4]))
print(make_dp(2,[1,3]))
print(make_dp(6,[1,2,2,4,5,7]))
