from turtle import *
from time import sleep
import random

def square(x,y,size,color_choice=(1,1,1)):
    move(x,y)
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



def touching(squares,current_square,size):
    if current_square[0][1] <= size-WINDOW_HEIGHT//2:
        current_square[1] = True
    for i in squares:
        if i == current_square:
            continue
        if current_square[0][1]-size <= i[0][1] and i[1] == True and abs(current_square[0][0]-i[0][0]) <=size:
            current_square[1] = True
    return current_square[1]

MIN_V = 0.7
MAX_V = 3
GAMMA = 0.9
    
def new_v(v):
    if random.random() > 0.5:
        v = v / GAMMA
    else:
        v = v * GAMMA
    if v < MIN_V:
        v = MIN_V
    if v > MAX_V:
        v = MAX_V
    return v/10
    

BOK = 50
WINDOW_HEIGHT = window_height()
WINDOW_WIDTH = window_width()  
COLORS = [tuple([random.random() for _ in range(3)]) for _ in range(20)]
tracer(0)

N=50
v_squares = [1 for i in range(N)]
x_s = [0 for _ in range(N)]
squares = [[[random.randint(-WINDOW_WIDTH//2,WINDOW_WIDTH//2),random.randint(200,500)],False,random.choice(COLORS)] for _ in range(N)]

while not all([i[1] for i in squares]):
    clear()
    for i,sq in enumerate(squares):
        square(sq[0][0],sq[0][1],BOK,sq[2])
        touching(squares,sq,BOK)
        if not sq[1]:
            sq[0][1] -= x_s[i]
    update()
    sleep(0.01)
    x_s = [x_s[i]+v_squares[i] for i in range(N)]
    #print(x_s)
    v_squares = [new_v(v_squares[i]) for i in range(N)]

done()