from turtle import *
from math import sin,pi

speed(0)

def move(x,y):
    penup()
    goto(x,y)
    pendown()

def sin_in_circles(n=360,size=100,freq=20,sine_amount=1/5,func=sin):
    for i in range(n):
        move(0,0)
        fd(size*(1+sine_amount*func(freq*pi*i/(n))))
        rt(360/n)

def crazy_sin(x):
    return sin(x)+sin(3*x)/3 +sin(2*x)/2 + sin(4*x)/10 + sin(5*x)/3 - sin(7*x)/5


tracer(0)
sin_in_circles()
#sin_in_circles(540,200,10,1/5,crazy_sin)
update()
done()