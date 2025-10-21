from itertools import product

def correct_board(b, v, w):
    n, m = len(v), len(w)
    row = all(sum(b[i*m : i*m + n]) == w[i] for i in range(m))
    col = all(sum(b[j * n + i] for j in range(m)) == v[i] for i in range(n))
    
    return row and col
    
def to_board(b,n,m):
    return [b[i*m : i*m + n] for i in range(m)]

def cienie(v, w):
    """
    bruteforce ma złożoność 2**(n**2)
    """
    #return (b for b in product([0,1], repeat = len(v) * len(w)) if correct_board(b,v,w))
    for b in product([0,1], repeat = len(v) * len(w)):
        if correct_board(b,v,w):
            yield to_board(b, len(v), len(w))



def cienie_backtrack(v,w):
    n,m = len(v), len(w)
    b = [[0] * n for _ in range(m)]
    curr_rows = [0] * m
    curr_cols = [0] * n

    correct_board_backtrack = lambda c_r, c_c : c_r == w and c_c == v
    solutions = []

    def search(row,col):
        #if we got to the last position with no errors ( filled the whole map)
        if row == m and col == n + 1:
            solutions.append(b)
            return
        
        #got to the end of a row
        if col == n+1:
            row += 1

        if b[row][col] != -1:
            search(row,col + 1)
            return 
        
        #now
        if can place :
            b[row][col] = 1
            search(row,col + 1)
            b[row][col] = 0
            

        
    print(b)

v, w = (1,3,1), (2,2,1)

cienie_backtrack(v,w)

#for b in cienie(v, w):
#    print(*b, sep = '\n')
#    print()