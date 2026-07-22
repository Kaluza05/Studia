def solve(hs):
    dp = {0: 0}

    for h in hs:
        new_dp = {}

        for d, low in dp.items():
            # 1. nie używamy
            if d not in new_dp or new_dp[d] < low:
                new_dp[d] = low

            # 2. do wyższej
            nd = d + h
            if nd not in new_dp or new_dp[nd] < low:
                new_dp[nd] = low

            # 3. do niższej
            nd = abs(d - h)
            new_low = low + min(d, h)
            if nd not in new_dp or new_dp[nd] < new_low:
                new_dp[nd] = new_low

        dp = new_dp

    if 0 in dp and dp[0] > 0:
        return "TAK", dp[0]
    else:
        best_d = min(d for d in dp if dp[d] > 0)
        return "NIE", best_d
    
print(solve([1,2,3,4]))
print(solve([1,3]))
print(solve([1,2,2,4,5,7]))
