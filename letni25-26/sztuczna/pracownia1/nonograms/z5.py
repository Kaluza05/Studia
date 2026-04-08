"""
to find correct board we keep track of columns and rows which satisfy given specification,
and choosing point on the board to fix 5% random
rest by choosing point in a non valid column or row also with 5% chance of random action
and the rest by choosing point with max_score.
max_score is calculated as a sum of len(row) - opt_dist( row)  and len(col) - column
"""

from random import random, choice,randint

def opt_dist_new(bin: list[int | bool], ns: list[int]): #do zmiany

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
    # print(prefix_nsum,b,n,len(bin))
    # print(bin)
    # print(prefix_ones)

    dp = [[0] * (b+1) for _ in range(n+1)] 
    # print(*dp,sep = '\n')
    # print('bin',bin,i)
    # print('prefix_ones',prefix_ones)
    # print('prefix nsum', prefix_nsum)
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
                
                # print(i-ns[j-1],i-1,j,'fragment',bin[i-ns[j-1]:i])#, prefix_ones[i],prefix_ones[i-ns[j]])
                # print('b_cost',block_cost)
                # print('dla : ',i,j)
                # print('opcja blok', dp[i-1-ns[j-1]][j-1], sep_cost, block_cost)
                # print('opcja bez bloku', dp[i-1][j], bin[i-1])
                dp[i][j] = min([dp[i-1-ns[j-1]][j-1] + sep_cost + block_cost,           #block ends at i change them to ones from i
                            dp[i-1][j] + bin[i-1]])         #block doesnt end at i
            
    # print('after')
    # print(*dp, sep = '\n')
    return dp[n][b]

def move_score_new(pos,board,spec):
    spec_x,spec_y = spec
    i,j = pos
    
    rows,columns = board
    row = rows[i]
    col = columns[j]

    row[j] = not row[j]
    col[i] = not col[i]
    #dodać heurstykę która określi co jest lepiej zamieniać
    # print(row, spec_x[i], len(row),opt_dist(['1' if i else '0' for i in row],spec_x[i]))
    val =   len(row) - opt_dist_new(row,spec_x[i]) + \
            len(col) - opt_dist_new(col,spec_y[j])
    
    row[j] = not row[j]
    col[i] = not col[i]
    # print(val)
    return 1 + val

def opt_dist(bin: list[int], n: int):
    if n > len(bin):
        return 'impossible'
    
    all_ones = bin.count(1)
    ones_in = bin[:n].count(1)
    k = n
    max_ones = ones_in
    while k < len(bin):
        # print('max_ones', max_ones)
        # print(k,k-n+1)
        ones_in += bin[k] - bin[k-n]
        k += 1
        if ones_in > max_ones:
            max_ones = ones_in
    
    return (n - max_ones) + (all_ones - max_ones)

def valid_row(row,n):
    one_occured = False
    all_ones = 0
    for i in range(len(row)):
        if not one_occured and row[i]:
            one_occured = True
            
        if row[i]:
            all_ones += 1

        if one_occured and not row[i] and all_ones != n:
            return False

    return all_ones == n
     
def valid_row_new(row,ns):
    n_blocks = len(ns)

    in_block = False
    ones_in_block = 0
    blocks_occured = 0
    for i in range(len(row)):
        if not in_block and row[i]:
            in_block = True
            blocks_occured += 1
            ones_in_block = 1
            
        elif row[i]: #still in the same block
            ones_in_block += 1

        elif not row[i] and in_block:
            in_block = False
            if blocks_occured > n_blocks:
                return False
            if ones_in_block != ns[blocks_occured-1]:
                return False
            ones_in_block = 0

    return (in_block and ones_in_block == ns[-1]) or (not in_block) and (n_blocks == blocks_occured)

def move_score(pos,board,spec):
    spec_x,spec_y = spec
    i,j = pos
    
    rows,columns = board
    row = rows[i]
    col = columns[j]

    row[j] = not row[j]
    col[i] = not col[i]
    #dodać heurstykę która określi co jest lepiej zamieniać
    # print(row, spec_x[i], len(row),opt_dist(['1' if i else '0' for i in row],spec_x[i]))
    val =   len(row) - opt_dist(row,spec_x[i]) + \
            len(col) - opt_dist(col,spec_y[j])
    
    row[j] = not row[j]
    col[i] = not col[i]
    # print(val)
    return 1 + val



def is_valid(valid_row,valid_col):
    return all(valid_row) and all(valid_col)
    

def init_board(x,y,spec):
    spec_x,spec_y = spec
    rows = [[0] * x for _ in range(y)] 
    cols = [[0] * y for _ in range(x)] 
    valid_row = [i == 0 for i in spec_x]
    valid_col = [i == 0 for i in spec_y]

    return rows,cols,valid_row,valid_col

def select_random(v_row,v_col):
    return choice([(i,1) for i,v in enumerate(v_row) if not v] + [(i,0) for i,v in enumerate(v_col) if not v])


def update_board(pos,rows,cols,valid_rows,valid_cols,spec):
    spec_x,spec_y = spec
    px,py = pos
    rows[py][px] = not rows[py][px]
    cols[px][py] = not cols[px][py]
    valid_rows[py] = valid_row(rows[py],spec_x[py])
    valid_cols[px] = valid_row(cols[px] ,spec_y[px])

    return rows,cols,valid_rows,valid_cols

def update_board_new(pos,rows,cols,valid_rows,valid_cols,spec):
    spec_x,spec_y = spec
    px,py = pos
    rows[py][px] = not rows[py][px]
    cols[px][py] = not cols[px][py]
    valid_rows[py] = valid_row_new(rows[py],spec_x[py])
    valid_cols[px] = valid_row_new(cols[px] ,spec_y[px])

    return rows,cols,valid_rows,valid_cols

def search_board(x,y,spec):
    #spec = [spec[0],spec[1]], spec[0] - rows, spec[1] - columns

    #after each move update specification
    rows,cols,valid_row,valid_col = init_board(x,y,spec)
    #threshold = 10*9
    no_sol = 0
    p = 0.05
    new_spec = [[i] for i in spec[0]], [[i] for i in spec[1]]

    while True:
        if random() < p:
            point_x,point_y = randint(0,x-1), randint(0,y-1)
        else:
            i,info = select_random(valid_row,valid_col) #selecting wrong row or column, info tells us if its a row or column
            selected = rows[i] if info else cols[i]

            
            # print(move_scores)
            if random() < p:
                selected_point = choice(selected)
            else:
                max_score = 0
                max_index = 0
                for j in range(len(selected)):
                    score = move_score_new((i,j),(rows,cols),new_spec) if info else move_score_new((j,i),(rows,cols),new_spec)
                    
                    if (info and move_score((i,j),(rows,cols),spec) != move_score_new((i,j),(rows,cols),new_spec)) \
                        or (not info and move_score((j,i),(rows,cols),spec) != move_score_new((j,i),(rows,cols),new_spec)):
                        print('scores not equal')
                        print('score1', move_score((i,j),(rows,cols),spec))
                        print('score_new', move_score_new((i,j),(rows,cols),new_spec))
                        print((i,j),rows,spec)

                    if score > max_score:
                        max_score = score
                        max_index = j

                selected_point = max_index

            if info:
                point_x,point_y = selected_point,i
            else:
                point_x,point_y = i,selected_point

        rows,cols,valid_row,valid_col = update_board_new((point_x,point_y),rows,cols,valid_row,valid_col,new_spec)
        # rows,cols,valid_row,valid_col = update_board((point_x,point_y),rows,cols,valid_row,valid_col,spec)
    
        if is_valid(valid_row,valid_col):
            break
            #if no_sol > threshold:
            #    no_sol = 0
            #    rows,cols,valid_row,valid_col = init_board(x,y,spec)
            #print(point_x,point_y)
        
        # print(*rows, sep = '\n')
        # print(valid_row,valid_col)
        # print()
        no_sol += 1

    return [['#' if x else '.' for x in r] for r in rows], no_sol


print(*search_board(3,4,([1,2,1,0],[1,2,1])),sep = '\n')
    
# with open('zad5_input.txt','r') as inp:
#     lines = inp.read().splitlines()

#     x,y = lines[0].split()
#     x,y = int(x),int(y)

#     spec_r = [int(i) for i in lines[1:y+1]]
#     spec_c = [int(i) for i in lines[y+1:]]
#     sol,n_moves = search_board(x,y,(spec_r,spec_c))
#     print(n_moves)

# with open('zad5_output.txt','w') as out:
#     frmt_sol = '\n'.join(''.join(r) for r in sol)
#     # print(frmt_sol)
#     out.write(frmt_sol)