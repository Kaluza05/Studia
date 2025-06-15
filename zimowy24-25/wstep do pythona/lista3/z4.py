from turtle import *


#400,300
pencolor('black')
pensize(2)
speed('fastest')
def move(x, y):
    penup()
    goto(x,y)
    pendown()

def kwadrat(x,y,bok):
    move(x,y)
    for _ in range(4):
        fd(bok)
        rt(90)

x=-100
y=100
start_bok = 200
decrement = 10
iters = start_bok//decrement
for i in range(iters):
    kwadrat(x,y,start_bok-decrement*i)
    x+=5
    y-=5
input()