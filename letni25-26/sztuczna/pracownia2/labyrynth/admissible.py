DIRECTIONS = ['L','R','U','D']

from collections import deque
import heapq


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

def reconstruct_path(path,pts):
    p = ''
    prev = pts
    # print(path)
    while path[prev]:
        # print(prev)
        d,prev = path[prev]
        p = d + p
    
    return p


def count_distance(n,m,end_positions):
    dist = {}
    for i in range(m):
        for j in range(n):
            min_dist = min(abs(i-pos[0])+abs(j-pos[1]) for pos in end_positions)
            dist[(i,j)] = min_dist
    
    return dist

#heuristic 2 is run solve on each point separately and take the max distance
def solve_1(n,m,lab,start_p,distances):
    start = start_p
    heap = []
    #we hold (f_score,counter,state,)
    #counter bo nie dopuszcza remisów heapq
    heapq.heappush(heap,(0,0,start))
    g_score = {start: 0}
    counter = 1
    # print('end points',end_positions)

    while heap:
        _,_,pt = heapq.heappop(heap)
        # print(bfs)

        if distances[pt] == 0:
            # print('got to end state',pts)
    
            return g_score[pt]
        
        for d in DIRECTIONS:
            new_pos = try_move(n,m,lab,pt[1],pt[0],d)
            if new_pos == pt:
                continue
            else:

                new_score = g_score[pt] + 1

                #if not visited add it, if this path to new_move is better than previous also add it
                if new_pos not in g_score or new_score < g_score[new_pos]:
                    g_score[new_pos] = new_score
                    f_score = new_score + distances[new_pos]

                
                    heapq.heappush(heap,(f_score,counter,new_pos))

                    counter += 1
                
    print('no solution found?')
    return None

def heuristic(n,m,lab, pts, distances, solve_1_precomp, take = None):
    if take == None:
        take = -1 #0, len(pts)//2, -1
    """
    speedup z 37s dla -1-> 0.15s dla 0

    """
    #for every point we want to calculate the manhattan distance to the closest of the points
    # h1 <= h2 always
    road_lengths = sorted([solve_1_precomp[p] for p in pts])
    h2 = sum(road_lengths[take:])
    #zamiast max bierzemy sumę

    return h2

def solve(n,m,lab, pts):
    "look for end state starting from pts"
    "solved but state is set of points where we could be"
    end_positions = {(i,j) for i,row in enumerate(lab) for j,el in enumerate(row) if el == 'G' or el == 'B'}

    distances = count_distance(n,m,end_positions)

    PRECOMPUTE_SOLVE1 = {(i,j) : solve_1(n,m,lab,(i,j),distances) for i in range(m) for j in range(n) if lab[i][j] != '#' }

    start = frozenset(pts)
    heap = []
    #we hold (f_score,counter,state,)
    #counter bo nie dopuszcza remisów heapq
    heapq.heappush(heap,(0,0,start))
    path = {start: None}
    g_score = {start: 0}
    counter = 1
    # print('end points',end_positions)

    while heap:
        _,_,pts = heapq.heappop(heap)
        # print(bfs)

        if heuristic(n,m,lab,pts,distances,PRECOMPUTE_SOLVE1) == 0:
            # print('got to end state',pts)
    
            succsesful_path = reconstruct_path(path,pts)
            return succsesful_path
        
        for d in DIRECTIONS:
            new_move = frozenset(move_pts(n,m,lab,pts,d))

            new_score = g_score[pts] + 1

            #if not visited add it, if this path to new_move is better than previous also add it
            if new_move not in g_score or new_score < g_score[new_move]:
                g_score[new_move] = new_score
                f_score = new_score + heuristic(n,m,lab,new_move,distances,PRECOMPUTE_SOLVE1)

                
                heapq.heappush(heap,(f_score,counter,new_move))
                path[new_move] = d,pts

                counter += 1
                
    print('no solution found?')
    return None



with open('zad_input.txt','r') as inp:
    lines = inp.read().splitlines()
    labytynth = [list(l) for l in lines]
    n,m = len(labytynth[0]), len(labytynth)

    pts = get_starting_points(labytynth)

    path = solve(n,m,labytynth,pts)

with open('zad_output.txt','w') as out:
    print(len(path),path)
    out.write(path)