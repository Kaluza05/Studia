from turtle import *
import time
import random

tracer(0,0)
FRAME_RATE = 1/30
colormode(255)
pensize(3)

def kwadrat(bok, k):
    prostokat(bok, bok, k)
    
def prostokat(bok1, bok2, k):
    fillcolor(k)
    begin_fill()
    for i in range(2):
        fd(bok1)
        rt(90)
        fd(bok2)
        rt(90)
        
    end_fill()       
    
     
        
def prezent(bok, k1, k2):
    pd()
    kwadrat(bok, k1)
    b5 = bok/5
    fd(2*b5)
    prostokat(b5, bok, k2)
    bk(2*b5)
    
    rt(90)
    fd(2*b5)
    rt(180)
    prostokat(b5, bok, k2)
    rt(180)
    bk(2*b5)    
    lt(90)

    pu()        

def smooth_change(N,start_col,end_col):
    r1,g1,b1 = start_col
    r2,g2,b2 = end_col
    return (r1 + (r2-r1)//N,g1 + (g2-g1)//N,b1 + (b2-b1)//N)
def rozeta(N, kat, dlugosc, start_col,end_col,ribbon_col):
    
    for i in range(N):
        if i<N/2:
            start_col = smooth_change(N,start_col,end_col)
        else:
            end_col = smooth_change(N,start_col,end_col)
        pu()
        fd(dlugosc)
        pd()
        rt(kat)
        prezent(50, start_col, ribbon_col)
        lt(kat)
        pu()
        bk(dlugosc)
        pd()
        rt(360 / N)


"""
for i in range(10):
    prezent(40, 'green', 'red')
    pu()
    fd(55)
    pd()
    rt(random.randint(-20, 20))
"""

if __name__ == '__main__':
    D1 = 6
    D2 = 10

    kat = 0

    col1 = (255,0,0)
    col2 = (0,0,255)
    starting_color = col1
    while True:
        clear()
        rt(D1)
        rozeta(20, kat,100,col1,col2,'green')
        kat += D2
        #starting_color = smooth_change(20,col1,col2)
        #if starting_color == (255,255,255)
        update()
        time.sleep(0.03)

    input() 