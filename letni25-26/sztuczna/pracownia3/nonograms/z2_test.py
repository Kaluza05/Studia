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




def simple_boxes(row,spec):
    c1,c2 = set(),set()
    if row[0] == 1:
        c1 = {(i,1) for i in range(1,spec[0]-1) if row[i] == -1}
    if row[-1] == 1:
        c2 = {(i,1) for i in range(len(row)-spec[-1],len(row)-1) if row[i] == -1}

    to_change = list(c1 | c2)

    return to_change
    



def simple_crosses(row,spec):
    return []


def overlapping(row,spec):
    all_poss = all_possibilities(row,spec)
    overlap = [{0,1} for _ in range(len(row))]
    for p in all_poss:
        for i,val in enumerate(p):
            overlap[i] &= {val}

    to_change = [(i,list(ov)[0]) for i,ov in enumerate(overlap) if len(ov) == 1 and row[i] == -1]

    return to_change


# print(overlapping([-1,-1,-1,-1,-1],[3]))

@make_list
@lru_cache(None)
def spreading(row,spec):
    return []
@make_list
@lru_cache(None)
def forcing(row,spec):
    return []


def infer_row(row : list,spec: list):
    #returns list of positions to change, 0 is a cross, 1 is a colored box
    if sum(row) == len(row):
        return []
    else:
        c1 = simple_boxes(row,spec)   #if theres one on the edge you can fill it with 1's to some point
        # row = update_row(row,c1)
        c2 = simple_crosses(row,spec) #if there has to be a space put that space there
        # row = update_row(row,c2)
        c3 = overlapping(row,spec)    #if there is an overlap every square in that overlap is a 1
        # row = update_row(row,c3)
        c4 = spreading(row,spec)      #if a length 4 box if on index 3 we can extend it to the right
        # row = update_row(row,c4)
        c5 = forcing(row,spec)        #if theres a cross on 3 and we have a box 3 to place 1,2 is also a cross
        # row = update_row(row,c5)
        
        return c1+c2+c3+c4+c5


def solve_bfs(rows,cols,spec_x,spec_y):
    changed_cells = []
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
                    changed_cells.append((row_index,idx))
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
                    changed_cells.append((idx,row_index))

        
    return changed_cells
    
# my_row = [[-1,-1,-1,-1],[-1,-1,-1,-1],[-1,-1,-1,-1],[0,-1,-1,-1]]
# print(solve_bfs(my_row,[[-1,-1,-1,0],[-1,-1,-1,-1],[-1,-1,-1,-1],[-1,-1,-1,-1]],[[2],[1],[1],[1]],[[1],[1],[1],[1]]))
# print(my_row)


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

def undo(rows,cols,changed):
    for i,j in changed:
        rows[i][j] = -1
        cols[j][i] = -1

# print('overlap',overlapping([0,-1,-1,-1],[2]))
def solve_backtrack(rows,cols,spec_x,spec_y):
    #zapamietaj wprowadzone zmiany w solve_bfs zeby latwo je bylo cofnac
    print('przed initial change')
    print(*rows,sep= '\n')
    
    print()
    changed = solve_bfs(rows,cols,spec_x,spec_y)

    print('po initial change',changed)
    print(*rows,sep= '\n')
    print()
    # print('po zmianie:')
    # print(*rows, sep = '\n')
    # print(changed)
    
    # print("unknowns:", count_unknown(rows))
    # print(*rows,sep = '\n')
    # print()

    if not valid(rows,cols,spec_x,spec_y):
        # print('not valid')
        # print(*rows,sep = '\n')
        #cofnij zmiany i dopiero cofaj
        undo(rows,cols,changed)
        return None
    
    if solved(rows,cols,spec_x,spec_y):
        return rows
    
    #inaczej w ktoreś pole z -1 wstaw 0 albo 1

    #trzeba jakos na bierzaco sprawdzac czy specyfikacja nie jest zaburzona
    first_uncertain = None
    for i in range(len(rows)):
        for j in range(len(cols)):
            if rows[i][j] == -1:
                first_uncertain = (i,j)
                break
                
    
    if first_uncertain is None: #no uncertains but the board is not solved
        #tez najpiew cofnij zmiany
        # print('no uncertains')
        undo(rows,cols,changed)
        return None
    
    
    else:
        i,j = first_uncertain
        print('galaz z jakims uncertain',i,j)
        # if (i,j) == (9,1):
        #     print(*rows,sep = '\n')

        rows[i][j] = 0
        cols[j][i] = 0
        result = solve_backtrack(rows,cols,spec_x,spec_y)
        if result is not None:
            return result
        
        

        # przywroc stare rows,cols
        # rows_copy,cols_copy = my_copy(rows),my_copy(cols)
        rows[i][j] = 1
        cols[j][i] = 1
        result = solve_backtrack(rows,cols,spec_x,spec_y)

        if result is not None:
            return result
        
        rows[i][j] = -1
        cols[j][i] = -1
        return None


    





with open('zad_input.txt','r') as inp:
    lines = inp.read().splitlines()

    x,y = lines[0].split()
    x,y = int(x),int(y)

    spec_r = [[int(j) for j in i.split(' ')] for i in lines[1:x+1]]
    spec_c = [[int(j) for j in i.split(' ')] for i in lines[x+1:]]

    rows = [[-1]*y for _ in range(x)]
    cols = [[-1]*x for _ in range(y)]
    # rows_before = my_copy(rows)
    # print('before')
    # print(*rows, sep = '\n')
    # print()
    # changed = solve_bfs(rows,cols,spec_r,spec_c)

    # print('after changes')
    # print(*rows, sep = '\n')
    # print()

    # undo(rows,cols,changed)

    # print('after undo')
    # print(*rows, sep = '\n')
    # print()
    # print(rows == rows_before)
    # rows = [[0, -1, -1, -1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0], [0, 1, 0, 0, -1, 0, -1, -1, 0, 0, -1, -1, 0, 0, 0], [0, -1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0], [0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0], [0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0], [0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0], [0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0], [0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0], [0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0], [1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0], [1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0], [1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0], [1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0], [0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1], [0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1], [1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1], [1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0], [1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0], [0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0], [0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0]]
    # cols = [list(col) for col in zip(*rows)]
    # print(*rows,sep = '\n')
#     #powinno cofnac rows do poczatkowego albo doprowadzic do wyniku
    new_board = solve_backtrack(rows,cols,spec_r,spec_c)
    print(*rows,sep='\n')
    
    sol = [['#' if x == 1 else '.' if x == 0 else '&' for x in r] for r in new_board]
#     # print(*new_board, sep = '\n')
    

with open('zad_output.txt','w') as out:
    frmt_sol = '\n'.join(''.join(r) for r in sol)
    print(frmt_sol)
    # print(frmt_sol)
    out.write(frmt_sol)