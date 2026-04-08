"""
to find correct board we keep track of columns and rows which satisfy given specification,
and choosing point on the board to fix 5% random
rest by choosing point in a non valid column or row also with 5% chance of random action
and the rest by choosing point with max_score.
max_score is calculated as a sum of len(row) - opt_dist( row)  and len(col) - column
"""

from random import random, choice,randint
from functools import lru_cache

def make_list(func):
    def inside(arg1 : list,arg2 : list):
        a1,a2 = tuple(arg1),tuple(arg2)
        return func(a1,a2)
    return inside



@make_list
@lru_cache(None)
def opt_dist(bin: tuple[int | bool], ns: tuple[int]):
    # print(ns)
    prefix_nsum = []
    for i,el in enumerate(ns):
        if i == 0:
            prefix_nsum.append(el)
        else:
            prefix_nsum.append(prefix_nsum[i-1] + el)

    b = i + 1 #len(ns)

    prefix_ones = []
    total_ones = 0
    for i,el in enumerate(bin):
        if el:
            total_ones += 1
        prefix_ones.append(total_ones)

    n = i + 1 #len(bin)

    dp = [[0] * (b+1) for _ in range(n+1)] 

    for j in range(b+1):
        for i in range(n+1):
            if i == 0:
                dp[i][j] = 0
            elif j == 0:
                dp[i][j] = prefix_ones[i-1] #turn all 1' into 0's

            elif i < j-1 + prefix_nsum[j-1]: #blocks cant fit into space i 
                dp[i][j] = float('inf')
            elif i - ns[j-1] == 0: # caly poczatek to blok
                block_cost = ns[j-1] - prefix_ones[i-1]
                dp[i][j] = block_cost

            else : # i - ns[j-1] > 0
                sep_cost = bin[i-ns[j-1]-1] #if it's a 1 its cost is 1 if 0 its 0 
                block_cost = ns[j-1] - (prefix_ones[i-1] - prefix_ones[i-1-ns[j-1]])

                dp[i][j] = min([dp[i-1-ns[j-1]][j-1] + sep_cost + block_cost,           #block ends at i change them to ones from i
                            dp[i-1][j] + bin[i-1]])         #block doesnt end at i
            
    return dp[n][b]

def move_score(pos,board,spec):
    spec_x,spec_y = spec
    i,j = pos
    
    rows,columns = board
    row = rows[i]
    col = columns[j]

    row[j] = not row[j]
    col[i] = not col[i]

    val =   len(row) - opt_dist(row,spec_x[i]) + \
            len(col) - opt_dist(col,spec_y[j])
    
    row[j] = not row[j]
    col[i] = not col[i]

    return 1 + val


def is_valid(valid_rows,valid_cols):
    return (not any(valid_rows)) and (not any(valid_cols))
    

def init_board(x,y,spec, init_random = True):
    if init_random:
        spec_x,spec_y = spec
        rows = [[random() > 0.7  for _ in range(y)] for _ in range(x)]
        cols = [[rows[j][i] for j in range(x)] for i in range(y)]
    
        valid_rows = [opt_dist(r,s) for r,s in zip(rows,spec_x)]
        valid_cols = [opt_dist(c,s) for c,s in zip(cols,spec_y)]

        return rows,cols,valid_rows,valid_cols
    
    if not init_random:
        spec_x,spec_y = spec
        rows = [[False] * y for _ in range(x)] 
        cols = [[False] * x for _ in range(y)] 
        
        valid_rows = [opt_dist(rows[i],s) for i,s in enumerate(spec_x)]
        valid_cols = [opt_dist(cols[i],s) for i,s in enumerate(spec_y)]

        return rows,cols,valid_rows,valid_cols



def select_random(v_row,v_col):
    return choice([(i,1) for i,v in enumerate(v_row) if v != 0] + [(i,0) for i,v in enumerate(v_col) if v != 0])


def update_board(pos,rows,cols,valid_rows,valid_cols,spec):
    spec_x,spec_y = spec
    px,py = pos
    rows[px][py] = not rows[px][py]
    cols[py][px] = not cols[py][px]
    valid_rows[px] = opt_dist(rows[px],spec_x[px])
    valid_cols[py] = opt_dist(cols[py] ,spec_y[py])

    return rows,cols,valid_rows,valid_cols

def search_board(x,y,spec, threshold = 5*10**4):
    

    #after each move update specification
    rows,cols,valid_rows,valid_cols = init_board(x,y,spec, init_random = False)
    
    no_sol = 0
    total = 0
    
    p = 0.1

    while True:        
        if total % threshold == 0:
            print(total)
        if no_sol > threshold:
            p = 0.1
            
            no_sol = 0
            rows,cols,valid_rows,valid_cols = init_board(x,y,spec, init_random = False)
            
        if random() < p:
            point_x,point_y = randint(0,x-1), randint(0,y-1)
        else:
            i,info = select_random(valid_rows,valid_cols) 
            #selecting wrong row or column, info tells us if its a row or column (1- row, 0 -col)
            selected = rows[i] if info else cols[i]

            
            
            if random() < p:
                selected_point = choice(selected)
            else:
                max_score = 0
                max_index = 0
            
                for j in range(len(selected)):
                    
                    score = move_score((i,j),(rows,cols),spec) if info else move_score((j,i),(rows,cols),spec)
                    

                    if score > max_score:
                        max_score = score
                        max_index = j

                selected_point = max_index

            if info:
                point_x,point_y = i,selected_point
            else:
                point_x,point_y = selected_point, i

        rows,cols,valid_rows,valid_cols = update_board((point_x,point_y),rows,cols,valid_rows,valid_cols,spec)
    
        if is_valid(valid_rows,valid_cols):
            break

        no_sol += 1
        total += 1

    return [['#' if x else '.' for x in r] for r in rows], total


with open('zad_input.txt','r') as inp:
    lines = inp.read().splitlines()

    x,y = lines[0].split()
    x,y = int(x),int(y)

    spec_r = [[int(j) for j in i.split(' ')] for i in lines[1:x+1]]
    spec_c = [[int(j) for j in i.split(' ')] for i in lines[x+1:]]

    sol,n_moves = search_board(x,y,(spec_r,spec_c))
    # print(n_moves)

with open('zad_output.txt','w') as out:
    frmt_sol = '\n'.join(''.join(r) for r in sol)
    # print(frmt_sol)
    out.write(frmt_sol)