"""
We perform a BFS on the chess positions starting from the given position until we find a checkmate.
"""


from functools import lru_cache
from collections import deque

@lru_cache(None)
def add_rook(wk,wr): #we dont need to know where the black king is cuz hes not in the way
    #figures are remembered as (x,y) , 0 <= x,y <= 7
    if wk[0] == wr[0]: #on the same rank
        file = [(i,wr[1]) for i in range(8) if i != wr[0]]
        rank = [(wr[0],i) for i in range(wk[1]+1,8) if i != wr[1]]  \
                if wk[1] < wr[1] else  \
                [(wr[0],i) for i in range(wk[1]) if i != wr[1]] # ...K...R..
    elif wk[1] == wr[1]: #on the same file
        rank = [(wr[0],i) for i in range(8) if i != wr[1]]
        file = [(i,wr[1]) for i in range(wk[1]+1,8) if i != wr[0]]  \
                if wk[0] < wr[0] else  \
                [(i,wr[1]) for i in range(wk[0]) if i != wr[0]]
        
    else:
        rank = [(wr[0],i) for i in range(8) if i != wr[1]]
        file = [(i,wr[1]) for i in range(8) if i != wr[0]]

    return file + rank

def around_king(k):
    k_x,k_y = k

    around_king = []
    for x in [-1,0,1]:
        for y in [-1,0,1]:
            if x == 0 and y == 0:
                continue
            if k_x + x >= 8 or k_x + x < 0 or k_y + y >= 8 or k_y + y < 0:
                continue
            else:
                around_king.append((k_x+x,k_y+y))
    
    return around_king

#precompute all positions so we dont have to lookup cache (only 64 positions to remember)
AROUND_KING = {(x,y) : around_king((x,y)) for x in range(8) for y in range(8)}

@lru_cache(None)
def add_king(color,wk,wr,bk):
    ar_w = AROUND_KING[wk]
    ar_b = AROUND_KING[bk]
    if color == 1:
        king_moves = []
        for (x,y) in ar_w:
            if (x,y) == wr or (x,y) in ar_b:
                continue
            else:
                king_moves.append((0,(x,y),wr,bk))
    else:
        king_moves = []
        for (x,y) in ar_b:
            if (x,y) == wr and (x,y) not in ar_w:
                king_moves.append(-2) # -2 represents that we took enemies rook (we dont want that)
                
            if (x,y) in ar_w: #too close to white king
                continue
            if x == wr[0] or y == wr[1]: #got in the file of rook
                continue
            else:
                king_moves.append((1,wk,wr,(x,y)))

    return king_moves

@lru_cache(None)
def find_moves(color,wk,wr,bk):
    #returns list of tuples color,wk,wr,bk
    if color == 1:
        king_moves = add_king(color,wk,wr,bk)
        rook_moves = [(0,wk,r,bk) for r in add_rook(wk,wr)]
        return king_moves + rook_moves

    else:
        king_moves = add_king(color,wk,wr,bk)
        if king_moves == []: #no moves
            if wr[0] == bk[0] or wr[1] == bk[1]: #check
                return []
            else:
                return [-1]
        elif king_moves == [-2]:
            return [-1]

        else:
            return [k for k in king_moves if k != -2]

    

def print_board(wk,wr,bk):
    board = '_'*17
    for i in range(8):
        board += '\n'
        for j in range(8):
            board += '|'
            if (i,j) == wk:
                board += 'K'
            elif (i,j) == wr:
                board += 'R'
            elif (i,j) == bk:
                board += 'B'
            else:
                board += ' '
        board += '|'
        
    print(board)

def is_mate(pos):
    color,wk,wr,bk = pos
    return color == 0 and find_moves(color,wk,wr,bk) == []


def moves_to_mate(color,wk,wr,bk):
    visited = set()
    to_visit = deque([((color,wk,wr,bk),0)])
    
    while to_visit: #to_visit
        pos,d = to_visit.popleft()
        
        if pos in visited:
            continue
        else:

            visited.add(pos)

            if pos == -1:
                continue
            # print(pos,d)
            if is_mate(pos): #kolor sprawdzamy bo moze byc takie ustawienie ze bialy sie zablokuje
                return d

            moves = find_moves(*pos)
            
            to_visit.extend((i,d+1) for i in moves)
            
    return 'INF' #didnt find mate so it's a draw

def moves_to_mate_path(color,wk,wr,bk):
    visited = {(color,wk,wr,bk)}
    to_visit = deque([((color,wk,wr,bk),0)])
    parents = {(color,wk,wr,bk) : None}
    while to_visit: #to_visit
        pos,d = to_visit.popleft()
        

        # print(pos,d)
        if is_mate(pos): #kolor sprawdzamy bo moze byc takie ustawienie ze bialy sie zablokuje
            return d,parents,pos
        
        moves = find_moves(*pos)
        
        for m in moves:
            if m in visited or m == -1:
                continue
            visited.add(m)
            parents[m] = pos
            to_visit.append((m,d+1))
            
    return 'INF' #didnt find mate so it's a draw

def reconstruct_path(pos,parents):
    path = []

    seen = set()
    while pos is not None:
        if pos in seen:
            raise RuntimeError("cycle detected in parents!")
        seen.add(pos)
        path.append(pos)
        pos = parents[pos]

    path.reverse()
    return path

def convert_to_pair(p):
    assert len(p) == 2

    letter,num = p

    match letter:
        case "a":
            y = 0
        case "b":
            y = 1
        case "c":
            y = 2
        case "d":
            y = 3
        case "e":
            y = 4
        case "f":
            y = 5
        case "g":
            y = 6
        case "h":
            y = 7

    x = 8 - int(num)

    return (x,y)


import argparse

def main():
    parser = argparse.ArgumentParser(description="KRK solver")
    parser.add_argument("--path", action="store_true",
                        help="Zwróć całą ścieżkę do mata zamiast samej liczby ruchów")
    
    return parser.parse_args()


with open('zad1_input.txt','r') as inp:
        c,wk,wr,bk = inp.read().split()
        print(c,wk,wr,bk)
        c = 1 if c == 'white' else 0
        wk,wr,bk = convert_to_pair(wk),convert_to_pair(wr),convert_to_pair(bk)
        
        args = main()
        if args.path:
            mvs,parents,mate = moves_to_mate_path(c,wk,wr,bk)

            for p in reconstruct_path(mate,parents):
                _,wk,wr,bk = p
                print_board(wk,wr,bk)
                print()
                print(' ' * 8 + '|' + ''*8)
                print(' ' * 8 + 'v' + ''*8)

        else:
            with open('zad1_output.txt','w') as out:
                sol = moves_to_mate(c,wk,wr,bk)
                out.write(str(sol))