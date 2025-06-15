def sciezka(tab,n,m):
    print(*tab,sep='\n')
    print()
    cost = [0 for _ in range(m)]
    for i in range(m):
        if i == 0: cost[0] = tab[0][0]
        else: cost[i] = cost[i-1] + tab[0][i]

    for j in range(1,n):
        print(cost)
        cost[0] += tab[j][0]
        for i in range(1,m):
            cost[i] = min(cost[i],cost[i-1]) + tab[j][i]
            
    return cost[-1]

tablica = [[10,9,31],[21,7,8],[13,14,10]]

print(sciezka(tablica,len(tablica),len(tablica[0])))