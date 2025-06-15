from turtle import *

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
    for rgb in data:
        if rgb == 'next_line':
            move(x,pos()[1]-size)
        else:
            square(size,fill_color=rgb)
            fd(size)

with open('dane_rgb.txt','r') as data:
    process = data.read().splitlines()
    grid = []
    for i in process:
        temp = i.split()
        for rgb in i.split():
            grid.append(eval(rgb))
        grid.append('next_line')


tracer(0)
draw_from_data(grid,7,(-200,200))
update()
done()