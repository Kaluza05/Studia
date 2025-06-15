from turtle import *

YELLOW = (230, 255, 26)  
PINK = (238, 38, 156)
RED=(255,0,0)
BLUE=(0,0,255)
GREEN = (0,255,0)
LIGHT_BLUE = (0, 255, 238)

START_POS = (0,0)
SIZE=20
NUM_OF_EDGES = 18
pensize(1)
speed(0)


def rgb_to_hex(r, g, b): #z chatu zamiania rgb na hex, czyli tak jak przyjmuje turtle
    return "#{:02x}{:02x}{:02x}".format(r, g, b)

def color_now(i,steps,start_color,end_color):  #przechodzi łagodnie z start color do end zmniejszając 
    r = int(start_color[0] + (end_color[0] - start_color[0]) * i / steps)
    g = int(start_color[1] + (end_color[1] - start_color[1]) * i / steps)
    b = int(start_color[2] + (end_color[2] - start_color[2]) * i / steps)
    return rgb_to_hex(r,g,b)


def move(x, y):
    penup()
    goto(x,y)
    pendown()

def square(size):
    begin_fill()
    for _ in range(4):
        fd(size)
        rt(90)
    end_fill()


def fig(n=10,size=10,start=(0,0),starting_rotation=0,start_color=(255,255,255),end_color=(0,0,0)):#n to ile ma zrobić większych boków
    move(*start)
    setheading(starting_rotation)
    steps = 2+ ((2+n)*(n-1))//2
    current_step = 0
    for k in range(1,n+1): #robi n boków z k kwadratami
        if k ==1: #pierwszy bok ma tak na prawdę długość 2
            k=2
        for _ in range(k): #robi k kwadratów
            color('black',color_now(current_step,steps,start_color,end_color))
            square(size)
            fd(size)
            current_step +=1
        right(90)
        fd(size)
    


#fig(NUM_OF_EDGES,SIZE)
tracer(0)
#fig(80,10,start_color=LIGHT_BLUE,end_color=PINK)
#fig(NUM_OF_EDGES,SIZE,start_color=LIGHT_BLUE,end_color=PINK)
fig(30,20,starting_rotation=0,start_color=LIGHT_BLUE,end_color=PINK)
#fig(30,20,start=(SIZE,-2*SIZE),starting_rotation=180,start_color=PINK,end_color=LIGHT_BLUE)
update()
done()