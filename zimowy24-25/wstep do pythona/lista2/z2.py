def koperta(n):  
    a= [[' ' for _ in range(n)] for _ in range(n)]
    for i in range(n):
        a[0][i] = '*' #1 wiersz
        a[n-1][i] = '*' #ostatni wiersz
        a[i][i] = '*' #przekątna w dół
        a[i][0] = '*' #lewy bok
        a[i][n-1] = '*' # prawy bok
        a[n-1-i][i] = '*'  # przekątna w górę
    koperta = '\n'.join([''.join(a[i][j] for j in range(n)) for i in range(n)]) 
    #łączymy każdy wiersz osobno i później wiersze z sobą z \n pomiędzy
    #print(*a , sep='\n')
    print(koperta)

koperta(11)