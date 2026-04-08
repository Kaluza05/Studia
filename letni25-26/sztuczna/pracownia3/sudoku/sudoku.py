import sys


def V(i,j):
    return f'V{i}_{j}'
    
def domains(Vs):
    return [ q + ' in 1..9' for q in Vs ]
    
def all_different(Qs):
    return 'all_distinct([' + ', '.join(Qs) + '])'
    
def get_column(j):
    return [V(i,j) for i in range(9)] 
            
def get_raw(i):
    return [V(i,j) for j in range(9)] 
                        
def horizontal():   
    return [ all_different(get_raw(i)) for i in range(9)]

def vertical():
    return [all_different(get_column(j)) for j in range(9)]

def boxes():
    boxes = []
    for i in range(3):
        for j in range(3):
            box_values = [V(x+3*i,y+3*j) for x in range(3) for y in range(3)]
            
            boxes.append(all_different(box_values))

    
    return boxes


def print_constraints(Cs, indent, d):
    position = indent
    print (indent * ' ', end='')
    for c in Cs:
        print (c + ',', end=' ')
        position += len(c)
        if position > d:
            position = indent
            print ()
            print (indent * ' ', end='')

def constraints_string(Cs,indent,d):
    s = ''
    position = indent
    s =  indent * ' ' + '\n'
    for c in Cs:
        
        s +=  c + ',' + ' '
        position += len(c)
        if position > d:
            position = indent
            
            s += "\n"
            s +=  indent * ' '

    return s

      
def sudoku(assigments):
    variables = [ V(i,j) for i in range(9) for j in range(9)]
    
    print (':- use_module(library(clpfd)).')
    print ('solve([' + ', '.join(variables) + ']) :- ')
    
    
    cs = domains(variables) + vertical() + horizontal() + boxes() #TODO: too weak contraints, add something!
    for i,j,val in assigments:
        cs.append( f'{V(i,j)} #= {val}')
    
    print_constraints(cs, 4, 70),
    print ()
    print ('    labeling([ff], [' +  ', '.join(variables) + ']).' )
    print ()
    print (':- solve(X), write(X), nl.')       

def sudoku_string(assigments):
    # print(assigments)

    variables = [ V(i,j) for i in range(9) for j in range(9)]
    
    s1 = ':- use_module(library(clpfd)).'
    s2 = 'solve([' + ', '.join(variables) + ']) :- '

    # print(s1)
    

    
    cs = domains(variables) + vertical() + horizontal() + boxes() #TODO: too weak contraints, add something!
    for i,j,val in assigments:
        cs.append( f'{V(i,j)} #= {val}')
    
    s3 = constraints_string(cs, 4, 70)
    s4 = '\n'
    s5 = '    labeling([ff], [' +  ', '.join(variables) + ']).'
    s6 = '\n'
    s7 = ':- solve(X), write(X), nl.'

    return '\n'.join([s1,s2,s3,s4,s5,s6,s7])  

with open('zad_input.txt','r') as inp:
    t = inp.read().splitlines()
    # print(*t, sep = '\n')
    triples = []
    for i,row in enumerate(t):
        for j,val in enumerate(row):
            if val != ".":
                triples.append((i,j,val))

    # sudoku(triples)

with open('zad_output.txt','w') as out:
    s = sudoku_string(triples)
    out.write(s)

# if __name__ == "__main__":
#     raw = 0
#     triples = []
    
#     for x in sys.stdin:
#         x = x.strip()
#         if len(x) == 9:
#             for i in range(9):
#                 if x[i] != '.':
#                     triples.append( (raw,i,int(x[i])) ) 
#             raw += 1          
#     sudoku(triples)
    
"""
89.356.1.
3...1.49.
....2985.
9.7.6432.
.........
.6389.1.4
.3298....
.78.4....
.5.637.48

53..7....
6..195...
.98....6.
8...6...3
4..8.3..1
7...2...6
.6....28.
...419..5
....8..79

3.......1
4..386...
.....1.4.
6.924..3.
..3......
......719
........6
2.7...3..
"""    
