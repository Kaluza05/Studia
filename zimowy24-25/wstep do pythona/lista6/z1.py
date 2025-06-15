from duze_cyfry import daj_cyfre
from turtle import *
from random import randint,choice


def square(size,color=(255,255,255)):
    fillcolor(color)
    begin_fill()
    for _ in range(4):
        fd(size)
        rt(90)
    end_fill()

def move(x,y):
    penup()
    goto(x,y)
    pendown()

def liczba(number,size,color='Black',start_pos=(0,0)):
    move(*start_pos)
    want = daj_cyfre(number)
    for i in range(5):#wysokość liczby
        for j in range(5):
            #color_now = color[j//6]
            if want[4-i][j] == '#':
                square(size,color)
            #fd(size)
            move(size+pos()[0],pos()[1])
        move(start_pos[0],size+pos()[1])


def grid(numbers=10,num_colors=4,grid_size=20):
    current_grid = [[' ' for i in range(grid_size)] for i in range(grid_size)]
    numbers_added = 0
    def can_place(start_pos,num,grid):
        for i in range(5):
            for j in range(5):
                if grid[start_pos[0]+i][start_pos[1]+j] != ' ' and num[i][j] != ' ':
                    return False
        return True
    def can_place_curr_color(start_pos,num_col,grid,grid_size):
        x,y = start_pos
        colors_near = set()
        try:
            if x!=0 and y!=0:
                colors_near |= set(grid[x+i][y-1] for i in range(5))
        except:
            print(x,y,'malo x')
        try:
            if x <= (grid_size - 5) and y+5<grid_size:
                colors_near |= set(grid[x+i][y+5] for i in range(5))
        except:
            print(x,y,'duzo x')
        try:
            if y != 0 and x!=0:
                colors_near |= set(grid[x-1][y+i] for i in range(5))
        except:print(x,y,'malo y')
        try:
            if y <= (grid_size - 5) and x+5<grid_size:
                colors_near |= set(grid[x+5][y+i] for i in range(5))
        except:print(x,y,'duzo y')
        if str(num_col) in colors_near:
            return False
        return True
    
    while numbers_added < numbers:
        num = daj_cyfre(randint(0,9))
        color = randint(0,num_colors-1)
        start_num = (randint(0,grid_size-5),randint(0,grid_size-5))
        if can_place(start_num,num,current_grid) and can_place_curr_color(start_num,color,current_grid,grid_size):
            for i in range(5):
                for j in range(5):
                    if num[i][j] == '#':
                        current_grid[start_num[0]+i][start_num[1]+j] = str(color)
            numbers_added +=1
    return current_grid

#print(*grid(numbers=6,num_colors=5,grid_size=20),sep='\n')

def mozaika(numbers=10,colors=['Black','Blue'],size=30,start_pos=(0,0)):
    grid_size = window_height()//size
    move(*start_pos)
    ready_grid = grid(numbers=numbers,num_colors=len(colors),grid_size=grid_size)
    #print(*ready_grid,sep='\n')
    #print(*ready_grid,sep='\n')
    for i in range(grid_size):
        for j in range(grid_size):
            if ready_grid[i][j] != ' ':
                color = ready_grid[i][j]
                square(size,colors[int(color)])
            move(pos()[0]+size,pos()[1])
        move(start_pos[0],start_pos[1]-(i+1)*size)

def rgb_to_hex(r, g, b): #z chatu zamiania rgb na hex, czyli tak jak przyjmuje turtle
    return "#{:02x}{:02x}{:02x}".format(r, g, b)

speed(0)
tracer(0)
colors = [rgb_to_hex(*[randint(0,255) for _ in range(3)]) for _ in range(20)]

mozaika(numbers=70,colors=colors,size=10,start_pos=(-window_width()/2,window_height()/2))
update()
done()