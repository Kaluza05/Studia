from random import randint,choice
from copy import deepcopy
from time import sleep

def neighbours(grid,row,col):
    m = []
    for a in range(-1,2):
                    for b in range(-1,2):
                        try:
                            if (b==0 or a == 0) and (a != b) and (0<= row+a <= len(grid)) and (0<= col+b <=len(grid[0])):
                                m.append((grid[row+a][col+b],row+a,col+b))
                        except:
                            pass
    return m

def print_map(mapa):
    for i in mapa:
        for j in i:
            if isinstance(j,int):
                print(' ',end=' ')
            else:
                print(j[0],end=' ')
        print('\n')
    print('*'*50)


def game_of_life(grid,iterations):
    grid = grid[1:].split('\n')
    my_map = [['' for i in range(len(grid[0]))] for i in range(len(grid))]
    for i,row in enumerate(grid):
        for j,elem in enumerate(row):
            if elem == '.':
                my_map[i][j] = randint(1,5)
            else:
                my_map[i][j] = [grid[i][j],5]

    for i in range(iterations):
        map_copy = deepcopy(my_map)
        for j,row in enumerate(my_map):
            for k,col in enumerate(row):
                if isinstance(my_map[j][k],int): continue
                cell_now = my_map[j][k]
                neighbour = choice(neighbours(my_map,j,k))
                match neighbour:
                    case (power,a,b) if isinstance(power,int):
                        if cell_now[1] > 1:
                            map_copy[a][b] = [cell_now[0],cell_now[1]-1]

                    case ((typ,_),_,_) if typ == cell_now[0]:
                        pass
                     
                    case ((_,power),a,b):
                        if power >= cell_now[1]:
                            map_copy[a][b] = [my_map[a][b][0],min(power+1,5)]
                            new_min = cell_now[1] - 1

                            if new_min == 0:
                                map_copy[j][k] = randint(1,5)
                            else:
                                map_copy[j][k][1] = new_min
                        else:
                            map_copy[j][k] = [cell_now[0],min(cell_now[1]+1,5)]

                            if power - 1 == 0:
                                map_copy[a][b] = randint(1,5)
                            else:
                                map_copy[a][b] = [my_map[a][b][0],power - 1]
                        
        my_map = map_copy
        sleep(0.5)
        print_map(my_map)
    #print_map(my_map)
    
                
                    
map_1='''
...kkkkkkkkkkkk.......
......................
......................
......................
..nnnn................
..nnnn................
..n...................
......................
.........kkkkkkkkkkk..
......................
......................
......................
......................
.........ppppp........
......................'''

map_2 ='''
k........
.........
.pp..p...
.........'''

iters = 10
game_of_life(map_1,iters)