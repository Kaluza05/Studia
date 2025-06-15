from turtle import *

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

def frac(n,s):
    if n == 1:
        fd(s)
    else:
        frac(n-1,s/3)
        lt(60)
        frac(n-1,s/3)
        rt(120)
        frac(n-1,s/3)
        lt(60)
        frac(n-1,s/3)

tracer(0)
move(-200,0)
frac(6,500)
update()
done()