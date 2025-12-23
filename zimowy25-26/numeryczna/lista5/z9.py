from math import log

def estimate_rank(f, x0, alpha, max_steps = 1000 ):
    x1 = f(x0)
    x2 = f(x1)
    p_est = []
    while max_steps > 0 :
        max_steps -= 1
        #print('x-es', x2,x1,x0)
        
        try:
            #print(log(abs((x2 - alpha) / (x1 - alpha))), log(abs((x1 - alpha) / (x0 - alpha))))
            p = log(abs((x2 - alpha) / (x1 - alpha))) / log(abs((x1 - alpha) / (x0 - alpha)))
            p_est.append(p)
        except:
            break
        x2,x1,x0 = f(x2),x2,x1

    return p_est

def estimate_alpha(f,x0, max_steps = 1000, epsilon = 1e-6):
    steps = 0
    while steps < max_steps and abs(f(x0)) > epsilon:
        x0 = f(x0)
        steps += 1

    return x0
    
def estimate_rank_no_alpha(f, x0, max_steps = 1000 ):
    x1 = f(x0)
    x2 = f(x1)
    x3 = f(x2)
    p_est = []
    while max_steps > 0 :
        max_steps -= 1
        #print('x-es', x2,x1,x0)
        
        try:
            #print(log(abs((x2 - alpha) / (x1 - alpha))), log(abs((x1 - alpha) / (x0 - alpha))))
            p = log(abs((x3 - x2) / (x2 - x1))) / log(abs((x2 - x1) / (x1 - x0)))
            p_est.append(p)
        except:
            break
        x3,x2,x1,x0 = f(x3),x3,x2,x1

    return p_est


def olver(f,f_der,f_dder):
    return lambda x : x - (f(x) / f_der(x)) - 1/2 * (f_dder(x) / f_der(x)) * (f(x) / f_der(x))**2

f = lambda x : x - (x**2 - 4) / (2*x)

#print(estimate_rank(f,1,2))      

#different functions to test olver
t1  = lambda x: x**2 - 3.5*x + 3.0               # (x-1.5)*(x-2)
t1d = lambda x: 2*x - 3.5
t1dd= lambda x: 2

t2  = lambda x: x**3 - 1.5*x**2 + x - 1.5       # (x-1.5)*(x**2+1)
t2d = lambda x: 3*x**2 - 3*x + 1
t2dd= lambda x: 6*x - 3

t3  = lambda x: x**3 - 5*x**2 + 6.75*x - 2.25   # (x-1.5)*(x-0.5)*(x-3)
t3d = lambda x: 3*x**2 - 10*x + 6.75
t3dd= lambda x: 6*x - 10

t4  = lambda x: x**4 - 1.5*x**3 + 2*x - 3       # (x-1.5)*(x**3+2)
t4d = lambda x: 4*x**3 - 4.5*x**2 + 2
t4dd= lambda x: 12*x**2 - 9*x

t5  = lambda x: x**5 - 1.5*x**4 + x**2 - 0.5*x - 1.5
#       = (x-1.5)*(x**4 + x + 1)
t5d = lambda x: 5*x**4 - 6*x**3 + 2*x - 0.5
t5dd= lambda x: 20*x**3 - 18*x**2 + 2

# Tablica trójek (funkcja, 1. pochodna, 2. pochodna)
t_list = [
    (t1,  t1d,  t1dd),
    (t2,  t2d,  t2dd),
    (t3,  t3d,  t3dd),
    (t4,  t4d,  t4dd),
    (t5,  t5d,  t5dd)
]

t_list = [
    (t1, t1d, t1dd),
    (t2, t2d, t2dd),
    (t3, t3d, t3dd),
    (t4, t4d, t4dd),
    (t5, t5d, t5dd),
]

for (t,td,tdd) in t_list:
    olv = olver(t,td,tdd)
    x0 = 2.2
    estimates = estimate_rank_no_alpha(olv,x0)
    if len(estimates) >=5 :
        print(estimates[-5:], sum(estimates[-5:]) / 5)
    else:
        print(estimates)

    est_a = estimate_alpha(olv, x0)
    print("estimated alpha = ",est_a)
    estimates2 = estimate_rank(olver(t,td,tdd),x0, alpha= est_a)
    if len(estimates2) >=5 :
        print(estimates2[-5:], sum(estimates2[-5:]) / 5)
    else:
        print(estimates2)
