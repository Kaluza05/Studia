from turtle import *
from random import choice

def move(x,y):
    penup()
    goto(x,y)
    pendown()

def square(size,fill_color):
    fillcolor(rgb_to_hex(*fill_color))
    begin_fill()
    for _ in range(4):
        fd(size)
        rt(90)
    end_fill()



def rgb_to_hex(r, g, b):
    return "#{:02x}{:02x}{:02x}".format(r, g, b)


def draw_from_data(data,size,start_pos):
    x,y = start_pos
    move(x,y)
    all_cords = [(i,j) for i in range(52) for j in range(52)]
    for i in range(52**2):
        a,b = choice(all_cords)
        all_cords.remove((a,b))
        tracer(0)
        move(x-size*a,y-size*b)
        square(size,data[(a,b)])
        update()
    


with open('dane_rgb.txt','r') as data:
    process = data.read().split()
    grid = {}
    #szerokosc 52
    for i,el in enumerate(process):
        grid[(i//52,i%52)] = eval(el)

speed(0)
#tracer(0)
draw_from_data(grid,7,(200,200))
#update()
done()