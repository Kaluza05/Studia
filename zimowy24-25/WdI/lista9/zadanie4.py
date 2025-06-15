MOVES = [(2,1),(2,-1),(-2,1),(-2,-1),(1,2),(1,-2),(-1,2),(-1,-2)]

def valid_move(x:int,y:int,board:list[list[int]],n:int)->bool:
    return (0 <= x < n and 0 <= y < n and board[x][y] == -1)

def solve_knight(x:int,y:int,board:list[list[int]],n:int,move_count:int)->bool:
    if move_count == n*n:
        return True
    
    for move in MOVES:
        next_x,next_y = x + move[0], y + move[1]
        if valid_move(next_x,next_y,board,n):
            board[next_x][next_y] = move_count
            if move_count >=62:
                print(*board,sep='\n')
            if solve_knight(next_x,next_y,board,n,move_count+1): return True
            board[next_x][next_y] = -1
    return False

def init_knight(n:int,pos:tuple[int,int]):
    board = [[-1 for _ in range(n)] for _ in range(n)]
    x,y = pos
    board[x][y] = 0

    if solve_knight(x,y,board,n,1):
        return board
    return None
from time import time
s = time()
print(*init_knight(7,(0,0)),sep='\n')

print(time()-s)