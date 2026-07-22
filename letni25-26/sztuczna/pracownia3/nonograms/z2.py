"""
Plan:
funkcja która przegląda planszę aż znajdzie coś gdzie może wwykonać wnioskowanie, potem odpalamy czy można wykonać wnioskowanie 
na tym wierszu znowu i na kolumnie,
jak się nie da to zaczynamy od początku całą planszę
"""

from functools import lru_cache
from collections import deque

def make_list(func):
    def inside(arg1 : list,arg2 : list):
        a1,a2 = tuple(arg1),tuple(arg2)
        return func(a1,a2)
    return inside

def update_row(row,to_change):
    for i in to_change:
        idx,val = i
        row[idx] = val
    return row

def my_copy(rows):
    return [r[:] for r in rows]

def count_unknown(rows):
    return sum(cell == -1 for row in rows for cell in row)

@make_list
@lru_cache(None)
def opt_dist(bin: tuple[int | bool], ns: tuple[int]):
    for i in bin:
        if i == -1:
            return -1
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



@make_list
@lru_cache(None)
def all_possibilities(row: tuple[int], spec: tuple[int]):
    #generate all possible positions that meet the specifications without changing the 1's and 0' only -1's
    #backtrack
    n = len(row)
    results = []
    def backtrack(pos,spec_i,current_row):
        if pos >= n:
            if spec_i == len(spec):
                results.append(current_row[:])
            return
            
        if spec_i > len(spec):
            return
        
        
        if row[pos] in (-1,0):
            current_row.append(0)
            backtrack(pos+1,spec_i,current_row)
            current_row.pop() #back

        if spec_i < len(spec):
            length = spec[spec_i]

            # does block fit?
            if pos + length <= n:
                ok = True

                # can place block?
                for i in range(length):
                    if row[pos + i] == 0:
                        ok = False
                        break

                # check separator
                if ok:
                    if pos + length < n and row[pos + length] == 1:
                        ok = False

                if ok:
                    # insert block
                    for i in range(length):
                        current_row.append(1)

                    # separator
                    if pos + length < n:
                        current_row.append(0)
                        backtrack(pos + length + 1, spec_i + 1, current_row)
                        current_row.pop()
                    else:
                        backtrack(pos + length, spec_i + 1, current_row)

                    # pop block
                    for i in range(length):
                        current_row.pop()
    
    backtrack(0,0,[])
    return results



def overlapping(row, spec):
    row,spec = list(row),list(spec)
    n = len(row)

    @lru_cache(None)
    def dp(pos, spec_i):
        if pos == n:
            return spec_i == len(spec)

        # 0
        if row[pos] in (-1, 0):
            if dp(pos + 1, spec_i):
                return True

        # blok
        if spec_i < len(spec):
            length = spec[spec_i]

            if pos + length <= n:
                ok = True
                for i in range(length):
                    if row[pos + i] == 0:
                        ok = False
                        break

                if ok and pos + length < n and row[pos + length] == 1:
                    ok = False

                if ok:
                    next_pos = pos + length + 1 if pos + length < n else pos + length
                    if dp(next_pos, spec_i + 1):
                        return True

        return False

    to_change = []

    for i in range(n):
        if row[i] != -1:
            continue

        # sprawdź czy może być 0
        row[i] = 0
        can_be_0 = dp(0, 0)

        # sprawdź czy może być 1
        row[i] = 1
        dp.cache_clear()
        can_be_1 = dp(0, 0)

        row[i] = -1
        dp.cache_clear()

        if can_be_0 and not can_be_1:
            to_change.append((i, 0))
        elif can_be_1 and not can_be_0:
            to_change.append((i, 1))

    return to_change


@make_list
@lru_cache
def infer_row(row : list,spec: list):
    #returns list of positions to change, 0 is a cross, 1 is a colored box
    if sum(row) == len(row):
        return []
        
    return overlapping(row,spec)


def solve_bfs(rows,cols,spec_x,spec_y):
    
    #infers one row, then sequentially infers columns where there was a value change
    queue = deque()

    for i in range(len(rows)):
        queue.append((i,True))
    for j in range(len(cols)):
        queue.append((j,False))

    while queue:
        row_index,is_row = queue.popleft()

        # print(row_index,'row' if is_row else 'col')
        # print(*rows,sep = '\n')
        # print()

        if is_row:
            # print('przed zamiana', rows[row_index])
            changed = infer_row(rows[row_index],spec_x[row_index])
            # print('po zamiana', rows[row_index])
            # print('zmienione row',changed)
            # print()
            # print('changes',row_index,is_row,changed)
            for idx,val in changed:
                if rows[row_index][idx] != val:
                    rows[row_index][idx] = val
                    cols[idx][row_index] = val
                    queue.append((idx,False))
        else:
            # print('przed zamiana', cols[row_index])
            changed = infer_row(cols[row_index],spec_y[row_index])
            # print('changes',row_index,is_row,changed)
            # print('po zamiana', cols[row_index])
            # print('zmienione col',changed)
            # print()

            for idx,val in changed:
                if cols[row_index][idx] != val:
                    cols[row_index][idx] = val
                    rows[idx][row_index] = val
                    queue.append((idx,True))

        
        

    
    

def row_possible(row,spec_row):
    #if theres more 1's than in spec its impossible
    return sum([1 for i in row if i == 1]) <= sum(spec_row)

def valid(rows, cols, spec_x, spec_y):
    for i in range(len(rows)):
        if not row_possible(rows[i], spec_x[i]):
            return False

    for j in range(len(cols)):
        if not row_possible(cols[j], spec_y[j]):
            return False

    return True

def solved(rows,cols,spec_x,spec_y):
    for i,r in enumerate(rows):
        if opt_dist(r,spec_x[i]) != 0:
            return False
        
    for i,c in enumerate(cols):
        if opt_dist(c,spec_y[i]) != 0:
            return False
        
    return True

def choose_uncertain(rows,cols,spec_x,spec_y):
    #w wszytkich uncertain zrob solve bfs i sprawdz ile to daje
    for i in range(len(rows)):
        for j in range(len(cols)):
            if rows[i][j] == -1:
                return (i,j)
                
# def choose_uncertain2(rows,cols,spec_x,spec_y):
#     #w wszytkich uncertain zrob solve bfs i sprawdz ile to daje
#     uncertains = dict()
#     for i in range(len(rows)):
#         for j in range(len(cols)):
#             if rows[i][j] == -1:
#                 for v in [0,1]:
#                     rows[i][j] = v
#                     cols[j][i] = v
#                     res = solve_bfs(rows,cols,spec_x,spec_y)
#                     undo(rows,cols,res)
#                     uncertains[(i,j,k)] = solve_bfs()
#                 rows[i][j] = -1
#                 cols[j][i] = -1

def all_filled(rows):
    for r in rows:
        for c in r:
            if c == -1:
                return False
    return True

def choose_row(rows,cols,spec_x,spec_y):
    row_options = [
        len(all_possibilities(rows[i], spec_x[i])) if -1 in rows[i] else float('inf')
        for i in range(len(rows))
    ]

    col_options = [
        len(all_possibilities(cols[i], spec_y[i])) if -1 in cols[i] else float('inf')
        for i in range(len(cols))
    ]

    best_row = min(range(len(rows)), key=lambda i: row_options[i])
    best_col = min(range(len(cols)), key=lambda i: col_options[i])

    if row_options[best_row] < col_options[best_col]:
        return best_row, True
    return best_col, False

def solve_backtrack(rows,cols,spec_x,spec_y):
    #zapamietaj wprowadzone zmiany w solve_bfs zeby latwo je bylo cofnac
    # print(*rows,sep = '\n')
    # print()
    solve_bfs(rows,cols,spec_x,spec_y)

    # print("unknowns:", count_unknown(rows))
    # print(*rows,sep = '\n')
    # print()

    if not valid(rows,cols,spec_x,spec_y):
        return None
    
    if solved(rows,cols,spec_x,spec_y):
        # print(rows)
        return rows
    
    if all_filled(rows):
        return None
    
    #jesli nie ma -1 zadnych to skoncz:

    
    #inaczej w ktoreś pole z -1 wstaw 0 albo 1

    #trzeba jakos na bierzaco sprawdzac czy specyfikacja nie jest zaburzona
    row_idx,is_row = choose_row(rows,cols,spec_x,spec_y)
    # wybieramy kolumne / wiersz który ma najmniejszą ilość możliwych opcji
                
    if is_row:
        for poss in all_possibilities(rows[row_idx],spec_x[row_idx]):
            new_rows,new_cols = my_copy(rows),my_copy(cols)
            for j in range(len(cols)):
                new_rows[row_idx][j] = poss[j]
                new_cols[j][row_idx] = poss[j]

            res = solve_backtrack(new_rows,new_cols,spec_x,spec_y)
            if res:
                return res

        
    else:
        for poss in all_possibilities(cols[row_idx],spec_y[row_idx]):
            new_rows,new_cols = my_copy(rows),my_copy(cols)
            for i in range(len(rows)):
                new_rows[i][row_idx] = poss[i]
                new_cols[row_idx][i] = poss[i]

            res = solve_backtrack(new_rows,new_cols,spec_x,spec_y)
            if res:
                return res


    





with open('zad_input.txt','r') as inp:
    lines = inp.read().splitlines()

    x,y = lines[0].split()
    x,y = int(x),int(y)

    spec_r = [[int(j) for j in i.split(' ')] for i in lines[1:x+1]]
    spec_c = [[int(j) for j in i.split(' ')] for i in lines[x+1:]]

    rows = [[-1]*y for _ in range(x)]
    cols = [[-1]*x for _ in range(y)]

    # rows = [[0, -1, -1, -1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0], [0, 1, 0, 0, -1, 0, -1, -1, 0, 0, -1, -1, 0, 0, 0], [0, -1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0], [0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0], [0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0], [0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0], [0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0], [0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0], [0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0], [1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0], [1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0], [1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0], [1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0], [0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1], [0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1], [1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1], [1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0], [1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0], [0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0], [0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0]]
    # cols = [list(col) for col in zip(*rows)]
    # print(*rows, sep = '\n')
    # print()
    new_board = solve_backtrack(rows,cols,spec_r,spec_c)
    sol = [['#' if x == 1 else '.' if x == 0 else '&' for x in r] for r in new_board]
    # print(*new_board, sep = '\n')
    

with open('zad_output.txt','w') as out:
    frmt_sol = '\n'.join(''.join(r) for r in sol)
    print(frmt_sol)
    # print(frmt_sol)
    out.write(frmt_sol)


