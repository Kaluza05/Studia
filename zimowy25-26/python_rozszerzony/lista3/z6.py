from itertools import product
from copy import deepcopy

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
    solutions = []

    def search(row,col):
        if col == n and row == m - 1:
            if all(x == y for x, y in zip(curr_cols, v)) and all(x == y for x, y in zip(curr_rows, w)):
                solutions.append(deepcopy(b))
                yield b
            return
        
        if col == m:
            yield from search(row + 1, 0)
            return
        
        if curr_cols[col] > v[col] or curr_rows[row] > w[row]:
            return
        

        yield from search(row, col + 1)

        curr_cols[col] += 1
        curr_rows[row] += 1
        b[row][col] = 1

        yield from search(row,col + 1)

        curr_cols[col] -= 1
        curr_rows[row] -= 1
        b[row][col] = 0

        
            
        
    yield from search(0,0)

v, w = (1,1,1,1,1,1,1), (1,1,1,1,1,1,1)


for b in cienie_backtrack(v,w):
    print(*b, sep = '\n')
    print()