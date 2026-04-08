'''
lab is n x m meaning
            n
    ------------------
    |
    |
m   |
    |
    |
    |
    |

'''
DIRECTIONS = ['L','R','U','D']

from random import choice
from collections import deque
from itertools import combinations
#isc gdzie nie bylismy z aktualnym stanem punktow jeszcze w pierwszej kolejnosci, inaczej isc w strone nie sciany
#albo w strone tego skad przyszlismy wtedy
'''
stan {zbior miejsc w ktorych mozemy byc}
bfs po stanach, stan koncowy gdy zbior miejsc jest zawarty w pozycjach koncowych


'''
def try_move(n,m,lab,x,y,direction):
    try:
        if direction == 'L':
            if x-1 < 0 or lab[y][x-1] == '#':
                return (y,x)
            return (y,x-1)
        elif direction == 'R':
            if x+1 >= n or lab[y][x+1] == '#':
                return (y,x)
            return (y,x+1)
        elif direction == 'U':
            if y-1 < 0 or lab[y-1][x] == '#':
                return (y,x)
            return (y-1,x)
        elif direction == 'D':
            if y+1 >= m or lab[y+1][x] == '#':
                return (y,x)
            return (y+1,x)
        else:
            raise UserWarning(f'error wrong direction {direction}')
    except IndexError:
        print('size',n,m)
        print('pos',x,y)
        raise IndexError(f'index out of range {direction}')


def get_starting_points(lab: list[list[str]]):
    start_pts = {(i,j) for i,row in enumerate(lab) for j,el in enumerate(row) if el == 'S' or el == 'B'}
    
    return start_pts

def move_pts(n,m,lab,pts,direction):
    return {try_move(n,m,lab,i[1],i[0],direction) for i in pts}

def move_depth(n,m,lab,pts,depth):
    if depth == 0:
        return [('',len(pts),pts)]
    else:
        all_down = []
        for d in DIRECTIONS:
            new_pts = move_pts(n,m,lab,pts,d)
            all_paths = move_depth(n,m,lab,new_pts,depth-1)

            for p in all_paths:
                path_down,pts_left,pts = p
                all_down.append((d+path_down,pts_left,pts))

        return all_down

#chcemy sekwencje kroków i liczba punktow po nich i te punkty zeby na nich pogłębić search


def decrease_uncertainity(n: int,m: int,lab: list[list[str]],possible_positions : set[tuple[int,int]],depth = 4):
    a_d = move_depth(n,m,lab,possible_positions,depth)
    take_best = 16
    #bierzemy 16 najlepszych i rozwiń je glebiej
    sorted_moves = sorted(a_d, key = lambda x : x[1])
    best = sorted_moves[:take_best]
    # print('najlepsze 16',[(i[0],i[1]) for i in best])
    new_best = []
    for p in best:
        path,_,pts = p
        for k in move_depth(n,m,lab,pts,depth):
            path_down,positions2,_ = k
            whole_path = path + path_down
            new_best.append((whole_path,positions2))

    #print('najlepsze na depth * 2',sorted(new_best, key = lambda x : x[1]))
    #print(min(new_best, key = lambda x : x[1]))
    return min(new_best, key = lambda x : x[1])[0] #first element of that path

def decrease_one_by_one(n,m,lab,pts):
    start = frozenset(pts)
    bfs = deque([start])
    visited = {start}
    path = {start: None}


    while bfs:
        # print(bfs)
        pts = bfs.popleft()
        if len(pts) < len(start):
            # print('got to end state in phase 2',pts)
            
            succsesful_path = reconstruct_path(path,pts)
            return succsesful_path
        for d in DIRECTIONS:
            new_move = frozenset(move_pts(n,m,lab,pts,d))
            if not is_dominated(new_move,visited):
                visited.add(new_move)
                bfs.append(new_move)
                path[new_move] = d,pts
                
    # print('no solution found?')
    return None


def reconstruct_path(path,pts):
    p = ''
    prev = pts
    # print(path)
    while path[prev]:
        # print(prev)
        d,prev = path[prev]
        p = d + p
    
    return p

def end_state(pts,end_pts):
    for i in pts:
        if i not in end_pts:
            return False
    return True


def is_dominated(state, visited):
    state_list = list(state)
    k = len(state_list)
    
    for r in range(1, k+1):  # tylko mniejsze
        for subset in combinations(state_list, r):
            if frozenset(subset) in visited:
                return True
    return False

def solve(n,m,lab, pts):
    "look for end state starting from pts"
    "solved but state is set of points where we could be"
    end_positions = {(i,j) for i,row in enumerate(lab) for j,el in enumerate(row) if el == 'G' or el == 'B'}

    start = frozenset(pts)
    bfs = deque([start])
    visited = {start}
    path = {start: None}

    # print('end points',end_positions)

    while bfs:
        # print(bfs)
        pts = bfs.popleft()
        if end_state(pts,end_positions):
            # print('got to end state',pts)
            
            succsesful_path = reconstruct_path(path,pts)
            return succsesful_path
        for d in DIRECTIONS:
            new_move = frozenset(move_pts(n,m,lab,pts,d))
            if not is_dominated(new_move,visited):
                visited.add(new_move)
                bfs.append(new_move)
                path[new_move] = d,pts
                
    # print('no solution found?')
    return None

def move_path(n,m,lab,pts,path):
    for d in path:
        pts = {try_move(n,m,lab,i[1],i[0],d) for i in pts}
    
    #print('after the moves',pts)
    return pts

def verify_path(n,m,lab,path):
    end_positions = {(i,j) for i,row in enumerate(lab) for j,el in enumerate(row) if el == 'G' or el == 'B'}
    s_pts = get_starting_points(lab)
    for d in path:
        s_pts = {try_move(n,m,lab,i[1],i[0],d) for i in s_pts}
    
    # print('after the moves',s_pts)
    for p in s_pts:
        if p not in end_positions:
            print('failed, not end point', p)
            return False
    # print('success')
    return True


with open('zad_input.txt','r') as inp:
    lines = inp.read().splitlines()
    labytynth = [list(l) for l in lines]
    n,m = len(labytynth[0]), len(labytynth)
    # print(n,m)
    # print(*labytynth, sep = '\n')
    # from_2 = solve(n,m,labytynth,{(4, 15), (4, 20)})
    # print(spec_r)
    # print(spec_c)
    #print(labytynth)
    pts = get_starting_points(labytynth)
    # # print(pts, len(pts))
    decr_path = ''
    limit = 100
    steps = 0
    DEPTH = 3
    limit2 = 200

    #phase 1 decrease until small amount of points remain
    while len(pts) > 6 and steps < limit:
        d_path = decrease_uncertainity(n,m,labytynth,pts, depth= DEPTH) 
        for d in d_path:
            pts = move_pts(n,m,labytynth,pts,d)
        #print(d,len(pts))
        decr_path = decr_path + d_path
        steps += 2 * DEPTH

    #print(pts,len(pts))
    decr_pts = pts.copy()
    phase2_path = ''

    while len(pts) > 3 and steps < limit2:
        d_path = decrease_one_by_one(n,m,labytynth,pts)

        pts = move_path(n,m,labytynth,pts,d_path)
        # print(d_path)
        phase2_path = phase2_path + d_path
        steps += 1
    
    phase2_pts = pts.copy()
    #print('\n'.join([''.join(['O' if (i,j) in pts else ' ' if labytynth[i][j] == 'S' else p for j,p in enumerate(r)]) for i,r in enumerate(labytynth)]))
    
    # print('after_decreasing',pts, len(pts))
    # print('decreasing path check')
    # move_path(n,m,labytynth,get_starting_points(labytynth),decr_path)
    # print()

    from_1 = solve(n,m,labytynth,phase2_pts)
    final_path = decr_path + phase2_path + from_1
    # print('droga do 1', decr_path)
    # print('testing from decr to end')
    # move_path(n,m,labytynth,decr_pts,from_1)
    # print('whole path test')

    # verify_path(n,m,labytynth,final_path)


with open('zad_output.txt','w') as out:
    # print('droga do redukcji 1', len(decr_path), decr_path)
    # print('droga do redukcji 2', len(decr_path + phase2_path), decr_path + phase2_path)
    # print('ostatni krok', len(decr_path + phase2_path + from_1), decr_path + phase2_path + from_1)
    # print('droga do celu', len(final_path), final_path)
    print(len(final_path))
    out.write(final_path)

# lab_test = [['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'], ['#', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', '#', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'B', 'S', '#'], ['#', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', '#', '#', 'S', 'S', 'S', 'S', 'S', 'S', '#'], ['#', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'], ['#', 'S', 'S', 'S', 'S', 'S', 'S', '#', '#', '#', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', '#'], ['#', 'S', 'S', 'S', 'S', 'S', 'S', '#', '#', '#', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', '#'], ['#', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', '#', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', '#'], ['#', '#', 'S', '#', '#', '#', '#', '#', '#', '#', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', '#'], ['#', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', '#'], ['#', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', '#'], ['#', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', '#'], ['#', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', '#'], ['#', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', '#'], ['#', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', '#'], ['#', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', '#'], ['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]
# test_pts = {(4, 17), (4, 16), (8, 14), (6, 5), (4, 3), (8, 7), (1, 5), (8, 10), (1, 17), (8, 13), (2, 9), (8, 9), (8, 12), (1, 16), (8, 15), (3, 5), (8, 11), (2, 8)}
# test_pts2 = {(8, 15), (3, 8), (8, 14), (4, 19), (8, 17), (6, 8), (4, 6), (8, 10), (8, 16), (1, 8), (1, 20), (8, 13), (2, 12), (8, 12), (8, 18), (1, 19), (2, 11), (4, 20)}
# n,m = len(lab_test[0]), len(lab_test)
# decrease_uncertainity(n,m,lab_test,test_pts2, depth= 6)