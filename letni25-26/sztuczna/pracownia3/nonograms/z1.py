"""
Plan:
najważniejsza heurystyka overlapping, jesli cos jest w kazdej mozliwej konfiguracji to musi tak być
"""

from random import random, choice,randint
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
        c1 = {(i,1) for i in range(1,spec[0]-1)}
    if row[-1] == 1:
        c2 = {(i,1) for i in range(len(row)-spec[-1],len(row)-1)}

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

def infer_sequentially(rows,cols,row_index,is_row,spec_x,spec_y):
    #infers one row, then sequentially infers columns where there was a value change
    if is_row:
        changed = infer_row(rows[row_index],spec_x[row_index])
        
        # print('changes',row_index,is_row,changed)
        for i in changed:
            idx,val = i
            rows[row_index][idx] = val
            cols[idx][row_index] = val

        return
        for i in changed:
            idx,_ = i
            infer_sequentially(rows,cols,idx,not is_row,spec_x,spec_y)
    else:
        changed = infer_row(cols[row_index],spec_y[row_index])
        # print('changes',row_index,is_row,changed)

        for i in changed:
            idx,val = i
            cols[row_index][idx] = val
            rows[idx][row_index] = val
        
        return
        for i in changed:
            idx,_ = i
            infer_sequentially(rows,cols,idx,not is_row,spec_x,spec_y)

def solve_bfs(rows,cols,spec):
    spec_x,spec_y = spec
    #infers one row, then sequentially infers columns where there was a value change
    queue = deque()

    for i in range(len(rows)):
        queue.append((i,True))
    for j in range(len(cols)):
        queue.append((j,False))

    

    # [(row_index,is_row)]
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

        
        

    
    return rows
        

def infer_board(rows,cols,spec):
    for i in range(len(rows)):
        infer_sequentially(rows,cols,i,True,spec[0],spec[1])

    # print('after rows')
    # print(*rows,sep='\n')
    for j in range(len(cols)):
        infer_sequentially(rows,cols,j,False,spec[0],spec[1])

    return rows





with open('zad_input.txt','r') as inp:
    lines = inp.read().splitlines()

    x,y = lines[0].split()
    x,y = int(x),int(y)

    spec_r = [[int(j) for j in i.split(' ')] for i in lines[1:x+1]]
    spec_c = [[int(j) for j in i.split(' ')] for i in lines[x+1:]]

    rows = [[-1]*y for _ in range(x)]
    cols = [[-1]*x for _ in range(y)]

    # print(*rows, sep = '\n')
    # print()
    new_board = solve_bfs(rows,cols,(spec_r,spec_c))
    sol = [['#' if x else '.' for x in r] for r in new_board]
    # print(*new_board, sep = '\n')
    

with open('zad_output.txt','w') as out:
    frmt_sol = '\n'.join(''.join(r) for r in sol)
    # print(frmt_sol)
    out.write(frmt_sol)