from turtle import *
from duze_cyfry import daj_cyfre
from random import randint

speed(0)

def move(x,y):
    penup()
    goto(x,y)
    pendown()

def rgb_to_hex(r, g, b): #z chatu zamiania rgb na hex, czyli tak jak przyjmuje turtle
    return "#{:02x}{:02x}{:02x}".format(r, g, b)


def kwadrat(size,fill=True,color='Black'):
    if fill:
        fillcolor(color)
        begin_fill()
    for _ in range(4):
        fd(size)
        rt(90)
    if fill:
        end_fill()

    
def liczba(n):
    do_wypisania = [daj_cyfre(int(i)) for i in str(n)]
    wyn = [' '.join([do_wypisania[i][num] for i in range(len(do_wypisania))]) for num in range(5)]
    return wyn

print(*liczba(456),sep='\n')

def liczba_turtle(n,size=40,start_pos=(0,0),fill=True):
    start_x,start_y = start_pos
    move(*start_pos)

    want = liczba(n)
    how_many_colors = len(str(n))
    colors = [rgb_to_hex(*[randint(0,255) for _ in range(3)]) for _ in range(how_many_colors)]
    length = len(want[0])
    for i in range(5):#wysokość liczby
        for j in range(length):
            color_now = colors[j//6]
            if want[4-i][j] == '#':
                kwadrat(size,fill=fill,color=color_now)
            #fd(size)
            move(size+pos()[0],pos()[1])
        move(start_x,size+pos()[1])

tracer(0)
liczba_turtle(314159265358979323846,size=10,start_pos=(-380,-0))
update()
done()