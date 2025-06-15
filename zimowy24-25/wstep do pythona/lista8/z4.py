from turtle import *
from random import randint,choices,random

COLORS = [(0, 0.5, 0), (0.5, 1, 0), (1, 1, 0), (1, 0.64, 0), (1, 0, 0), (0.5, 0, 0)]
MAP_SIZE = 100
WIDTH = window_width()
HEIGHT = window_height()
PIXEL_SIZE = HEIGHT/MAP_SIZE
speed(0)

def square(size,color_choice=(0.5,1,0)):
    color(color_choice)
    begin_fill()
    for _ in range(4):
        fd(size)
        rt(90)
    end_fill()

def move(x,y):
    penup()
    goto(x,y)
    pendown()


def color_from_num(x:int)->tuple[int]:
    return COLORS[round(x*5)]


class HipsometricMap:
    def __init__(self,map_size:int=100,num_rand_pts:int = 5,epochs:int = 50,own_weight:int = 5)->None:
        self.map_size = map_size
        self.hipsometric = [[0 for _ in range(map_size)] for _ in range(map_size)]
        self.num_rand_pts = num_rand_pts
        self.epochs = epochs
        self.own_weight = own_weight

    def __repr__(self)->str:
        return '\n'.join([repr(i) for i in self.hipsometric])

    def __len__(self):
        return self.map_size
    
    def __getitem__(self,x):
        return self.hipsometric[x]
    
    def on_line(self)->list:
        return [pix for row in self.hipsometric for pix in row]
    
    def start_map(self)->None:
        xs = choices(range(self.map_size),k=self.num_rand_pts)
        ys = choices(range(self.map_size),k=self.num_rand_pts)
        for x,y in zip(xs,ys):
            self.hipsometric[x][y] = random()

    def get_neighbours_avg(self,point:tuple[int,int],how_close:int=3)->int:
        x,y = point
        avgs = [0 for _ in range(how_close**2)]
        weighs = [self.own_weight/how_close**2 if i == how_close**2//2 else 1/how_close**2 for i in range(how_close**2)]
        for i in range(how_close):
            for j in range(how_close):
                try:
                    avgs[3*i+j] = self.hipsometric[x-1+i][y-1+j]
                except IndexError:
                    pass
        return sum([a*w for a,w in zip(avgs,weighs)])/sum(weighs)
    
    def run_sim(self)->None:
        for _ in range(self.epochs):
            x,y = randint(0,self.map_size-1),randint(0,self.map_size-1)
            new_val = self.get_neighbours_avg((x,y))
            self.hipsometric[x][y] = new_val
        self.normalize()

        
    def normalize(self)->None:
        min_val = min(self.on_line())
        self.hipsometric = [[cell-min_val for cell in col] for col in self.hipsometric ]
        max_val = max(self.on_line())
        self.hipsometric = [[cell/max_val for cell in col] for col in self.hipsometric ]

def draw_from_map(hip_map,pixel_size:int,start_pos:tuple=(0,0))->None:
    start_x,start_y = start_pos
    move(start_x,start_y)
    for i in range(len(hip_map)):
        for j in range(len(hip_map)):
            square(pixel_size,color_from_num(hip_map[i][j]))
            fd(pixel_size)
        move(start_x,pos()[1]-pixel_size)


my_map = HipsometricMap(MAP_SIZE,800,500000,1)
my_map.start_map()
my_map.run_sim()
my_map

tracer(0)
draw_from_map(hip_map=my_map,pixel_size=PIXEL_SIZE,start_pos=(-WIDTH/2,HEIGHT/2))
update()
done()