from turtle import *

#speed(0)
def kwadrat(size):
    fillcolor('Black')
    begin_fill()
    for _ in range(4):
        fd(size)
        rt(90)
    end_fill()

def move(x,y):
    penup()
    goto(x,y)
    pendown()


def kwadrat_rek(size,n):
    if n==0:
        kwadrat(size)
    else:
        for i in range(9):
            pos_before_rek = pos()
            if i%2==0:
                kwadrat_rek(size/3,n-1)
                move(*pos_before_rek)
            if i%3==2:
                move(pos()[0]-3*size/3,pos()[1]-size/3)
            move(pos()[0]+size/3,pos()[1])


tracer(0)
size = 400
depth = 4
def geom(n):
    return 1/2 * (1-(1/3)**(n+1))
print(geom(depth))
move(-size*geom(depth),size*geom(depth))
kwadrat_rek(size,depth)
update()
done()