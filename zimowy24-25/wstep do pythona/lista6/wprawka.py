from turtle import *
from math import sin,sqrt,cos
speed(0)


def move(x,y):
    penup()
    goto(x,y)
    pendown()

def square(size,color='Grey'):
    fillcolor(color)
    begin_fill()
    for _ in range(4):
        fd(size)
        rt(90)
    end_fill()

def grid(sq_per_size,size,start_pos,freq =1):
    move(*start_pos)
    for i in range(sq_per_size):
        for j in range(sq_per_size):
            '''
            x,y = pos()
            na zmianę
            if (i-j)%2==0:
                color = 'White'
            else: color = 'Black'
            square(size)
            a=1/8
            move(x+a,y-a)
            square(size-2*a,color=color)
            move(x,y)
            move(x+size,y)
            '''
            x,y = pos()
            if cos(freq*(2*x+y))>0:
                color = 'White'
            else:
                color ='Black'
            fill_size = 1-abs(cos(freq*(2*x+y)))
            #fill_size = 1/8
            square(size)
            a=size*(fill_size)/2
            move(x+a,y-a)
            square(size-2*a,color=color)
            move(x,y)
            move(x+size,y)

        move(start_pos[0],start_pos[1]+(i+1)*size)

tracer(0)
grid(20,30,(-300,-280),freq=4)
update()
done()