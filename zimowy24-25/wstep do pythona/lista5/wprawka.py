from turtle import *
from math import sqrt
#speed(0)
def move(x,y):
    penup()
    goto(x,y)
    pendown()

def kwadrat(size,fill=False,color=(0,0,0)):
    if fill:
        fillcolor(rgb_to_hex(*color))
        begin_fill()
    for _ in range(4):
        fd(size)
        rt(90)
    if fill:
        end_fill()

RED=(255,0,0)
GREEN=(0,255,0)
BLUE=(0,0,255)
LIGHT_BLUE = (0, 255, 238)
YELLOW = (230, 255, 26)  
PINK = (238, 38, 156)

def kulka2(n):#nieparzyste najlepiej
    a = [[] for _ in range(n)]
    b = [[] for _ in range(n)]
    for i in range(n):
        for j in range(n):
            mid = (n-1)/2
            odleglosc = sqrt((i-mid)**2 + (j-mid)**2)
            b[i].append(round(odleglosc/3)%3)
            #print(odleglosc)
            if  (mid+1/2)**2 >= (i-mid)**2 + (j-mid)**2:
                a[i].append(str(round(odleglosc/3)%3))#print('*',end='')
            else:
                a[i].append(' ') #print(' ',end='')
        #print('\n',end='')
    return a

#print(*kulka2(18),sep='\n')

def rgb_to_hex(r, g, b):
    return "#{:02x}{:02x}{:02x}".format(r, g, b)


def kolko(size,layers,step=4,color=(255,255,255)):
    rozmiar = 6*layers
    bb = kulka2(6*layers)
    R= 3*layers*(size+step)
    move(-R,-R)
    print(*bb,sep='\n')
    for i in range(rozmiar):
        for j in range(rozmiar):
            if bb[rozmiar-i-1][rozmiar-j-1] != ' ':
                if bb[rozmiar-i-1][rozmiar-j-1] == '0':color=LIGHT_BLUE
                elif bb[rozmiar-i-1][rozmiar-j-1] == '1':color= YELLOW
                    
                elif bb[rozmiar-i-1][rozmiar-j-1] == '2':color = PINK
                kwadrat(size,fill=True,color=color)
            else:pass
            move(pos()[0]+step+size,pos()[1])
        move(-R,pos()[1] + size +step)


tracer(0)
kolko(10,6)
update()
done()