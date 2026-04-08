
def B(i,j):
    return f'B_{i}_{j}'


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


def get_column(j,n_rows):
    return [B(i,j) for i in range(n_rows)] 
            
def get_row(i,n_cols):
    return [B(i,j) for j in range(n_cols)] 

def domains(bs):
    return constraints_string([f'{b} in 0..1' for b in bs],4,70)


def add_triples(triples):
    constraints = []
    for i,j,val in triples:
        constraints.append(f'{B(i,j)} #= {val}')

    return constraints_string(constraints,4,70)

def add_row_sum(n_rows,n_cols,rows,cols):
    rows_sum = []
    for i in range(n_rows):
        make_sum = ' + '.join(get_row(i,n_cols))
        
        rows_sum.append(f'{make_sum} #= {rows[i]}')

    
    cols_sum = []
    for j in range(n_cols):
        make_sum = ' + '.join(get_column(j,n_rows))
        
        cols_sum.append(f'{make_sum} #= {cols[j]}')

    return constraints_string(rows_sum + cols_sum,4,70)

def add_corners(r,c):
    c1 = rf' {B(0,0)} #= 1 #==> {B(0,1)} #= 1 #/\ {B(1,0)} #= 1 #/\ {B(1,1)} #= 1'
    c2 = rf' {B(r,0)} #= 1 #==> {B(r,1)} #= 1 #/\ {B(r-1,0)} #= 1 #/\ {B(r-1,1)} #= 1'
    c3 = rf' {B(0,c)} #= 1 #==> {B(1,c)} #= 1 #/\ {B(0,c-1)} #= 1 #/\ {B(1,c-1)} #= 1'
    c4 = rf' {B(r,c)} #= 1 #==> {B(r-1,c)} #= 1 #/\ {B(r,c-1)} #= 1 #/\ {B(r-1,c-1)} #= 1'
    return [c1,c2,c3,c4]

def add_edges(r,c):
    edge1 = []
    edge2 = []
    edge3 = []
    edge4 = []

    for i in range(1,c):
        const1 = rf' {B(0,i)} #= 1 #==> ({B(1,i)} #= 1 #/\ ( ({B(0,i-1)} #= 1 #/\ {B(1,i-1)} #= 1)  #\/ ( {B(0,i+1)} #= 1 #/\ {B(1,i+1)} #= 1 ) ) )'
        const2 = rf' {B(r,i)} #= 1 #==> {B(r-1,i)} #= 1 #/\ ( ({B(r,i-1)} #= 1 #/\ {B(r-1,i-1)} #= 1)  #\/ ( {B(r,i+1)} #= 1 #/\ {B(r-1,i+1)} #= 1 ) )'
        edge1.append(const1)
        edge2.append(const2)

    for i in range(1,r):
        const3 = rf' {B(i,0)} #= 1 #==> {B(i,1)} #= 1 #/\ ( ({B(i-1,0)} #= 1 #/\ {B(i-1,1)} #= 1)  #\/ ( {B(i+1,0)} #=1 #/\ {B(i+1,1)} #=1  ) )'
        const4 = rf' {B(i,c)} #= 1 #==> {B(i,c-1)} #= 1 #/\ ( ({B(i-1,c)}  #=1  #/\ {B(i-1,c-1)} #=1  ) #\/ ( {B(i+1,c)}  #=1  #/\ {B(i+1,c-1)}  #=1  ) )'
        edge3.append(const3)
        edge4.append(const4)

    return edge1+edge2+edge3+edge4


def add_middle(r,c):
    def h(p1,p2):
        return rf'({B(i+p1,j)} #= 1 #/\ {B(i,j+p2)} #= 1 #/\ {B(i+p1,j+p2)} #= 1 )'
    
    const = []
    for i in range(1,r):
        for j in range(1,c):
            fucked_up_formula = rf' {B(i,j)} #= 1 #==> ( ({h(1,1)}) #\/ ({h(1,-1)}) #\/ ({h(-1,1)}) #\/ ({h(-1,-1)}) )'
            const.append(fucked_up_formula)

    return const

def make_2x2(n_rows,n_cols):
    corners = add_corners(n_rows-1,n_cols-1)
    edges = add_edges(n_rows-1,n_cols-1)
    middle = add_middle(n_rows-1,n_cols-1)

    return corners + edges + middle



def add_triple_constraint(n_rows,n_cols):
    horizontal = []
    #zamienic na A <= B + C zamiast A = 1 => B = 1 \/ C = 1
    for i in range(1,n_rows-1):
        for j in range(n_cols):
            horizontal.append(f'{B(i,j)} #=< {B(i-1,j)} + {B(i+1,j)}')
            # horizontal.append(f'{B(i,j) } #= 1 #==> (({B(i-1,j)} #= 1) #\/ ({B(i+1,j)} #= 1))')

    vertial = []

    for j in range(1,n_cols-1):
        for i in range(n_rows):
            vertial.append(f'{B(i,j) } #=< {B(i,j-1)} + {B(i,j+1)}')
            # vertial.append(f'{B(i,j) } #= 1 #==> (({B(i,j-1)} #= 1) #\/ ({B(i,j+1)} #= 1))')
    

    return horizontal + vertial


def add_double_constraint(n_rows,n_cols):
    double = []
    for i in range(n_rows-1):
        for j in range(n_cols-1):
            impossible_cases = rf'{B(i,j)} + {B(i+1,j)} + {B(i,j+1)} + {B(i+1,j+1)} #\= 3'
            double.append(impossible_cases)

    return double

def add_no_touching(n_rows,n_cols):
    no_touching = []
    for i in range(n_rows-1):
        for j in range(n_cols-1):
            impossible_cases = rf'#\ ( ({B(i,j)} #= 1 #/\ {B(i+1,j+1)} #= 1 #/\  {B(i+1,j)} #= 0 #/\  {B(i,j+1)} #= 0) #\/ ({B(i,j)} #= 0 #/\ {B(i+1,j+1)} #= 0 #/\  {B(i+1,j)} #= 1 #/\  {B(i,j+1)} #= 1) )'
            no_touching.append(impossible_cases)

    return no_touching

def make_rectangle(n_rows,n_cols):
    constraints = make_2x2(n_rows,n_cols) + add_triple_constraint(n_rows,n_cols)  \
                + add_double_constraint(n_rows,n_cols) + add_no_touching(n_rows,n_cols)

    return constraints_string(constraints,4,70)


def storms(rows, cols, triples):
    prog = []
    def writeln(s):
        prog.append(s)

    writeln(':- use_module(library(clpfd)).')
    
    R = len(rows)
    C = len(cols)
    
    bs = [ B(i,j) for i in range(R) for j in range(C)]
    
    writeln('solve([' + ', '.join(bs) + ']) :- ')
    
    writeln(domains(bs))
    writeln(add_triples(triples))
    writeln(add_row_sum(R,C,rows,cols))
    writeln(make_rectangle(R,C))
    

    writeln('    labeling([ff], [' +  ', '.join(bs) + ']).' )
    writeln('')
    writeln(":- solve(X), write(X), nl.")

    return '\n'.join(prog)


with open('zad_input.txt','r') as inp, open('zad_output.txt','w') as out:
    inp = inp.readlines()
    rows = list(map(int, inp[0].split()))
    cols = list(map(int, inp[1].split()))
    triples = []

    for i in range(2, len(inp)):
        if inp[i].strip():
            triples.append(list(map(int,inp[i].split())))
            # triples.append(list(map(int, inp[i].split())))

    prog = storms(rows, cols, triples)

    # print(prog)
    out.write(prog)
       
        

