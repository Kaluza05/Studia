import matplotlib.pyplot as plt

def bin_s(xs,x):
    """
    in the sorted list xs finding index k such that xs[k-1] <= x <= xs[k]
    
    """
    a = 0
    b = len(xs) - 1
    while xs[a] < x <  xs[b]:
        m = (b + a) // 2 

        if xs[m] > x:
            b = m
        elif xs[m] < x:
            a = m +1
        else:
            return m
        
    return a


def d(xs,ys, k):
    """
    calculate f[x_k-1,x_k,x_k+1]
    calculate the equation manually
    f[1,2,3]-- f[2,3] - f[1,2] / 3 - 1
    f[1,2] - f[2] - f[1] / 2 - 1
    f[2,3] - f[3] - f[2] / 3 - 2]
    """
    
    assert 1 <= k <= len(xs) - 1
    x1,x2,x3 = xs[k-1], xs[k], xs[k+1]
    y1,y2,y3 = ys[k-1], ys[k], ys[k+1]

    f_12 =  (y2 - y1) / (x2 - x1)
    f_23 =  (y3 - y2) / (x3 - x2)
    f_123 = (f_23 - f_12) / (x3 - x1)
    return f_123


def get_moments(ts : list[float],yss : list[list[float]]):
    """
    finding moments where we have matrix:
    | 2 (1-l(1))                       | d1    |
    | l(2) 2 (1- l(2))                 | d2    |
    |      l(3) 2 (1- l(3))            | d3    |
    |            ...                   | ...   |
    |                   ... (1-l(n-2)) | d_n-2 |
    |                   l(n-1)    2    | d_n-1 |

    dis represent changed d_i to calculate M-n-1
    
    """
    n = len(ts) - 1
    h = lambda k : ts[k] - ts[k - 1]         #x_k - x_k-1
    l = lambda k : h(k) / (h(k) + h(k + 1))  

    dis = [[0] * len(yss) for _ in range(n)]
    dis[0] = [d(ts,ys,1) for ys in yss]

    w = 2

    for k in range(1,n-1):
        #dis[i] is really d_i+1
        t = l(k+1) / w
        w = 2 - (1 - l(k)) * t
        dis[k] = [d(ts,ys,k+1) - t * dis[k][i] for i,ys in enumerate(yss)]

    print(dis)
    mss = [[0] * len(yss) for _ in range(n+1)]
    # mss[0], mss[n] is set to zero as it should be
    mss[1] = [dis[n-1][i] / w for i in range(len(yss))]    #list of M_(n-1) moments for each of the ys in yss

    for j in range(2,n-1,-1):   
        for k in range(len(yss)):
            mss[j+1][k] = (d(ts,yss[k],j) - (1- l(j)) * mss[j-1][k] - 2 * mss[j-1][k]) / l(j)

    transpose_mss = [[mss[row][col] for row in range(len(mss))] for col in range(len(mss[0]))]
    
    return transpose_mss
    
    
def nifs_from_moments(ts,ys, ms):
    """
    ts  - list of knots, 
    yss - list of lists such that for each ys in yss f(t_k) = y_k, 
    ms  - moments m_k = s''(t_k)
    return NIFS3
    """
    def f(x):
        for i,y in enumerate(ts):
            if x == y:
                return ys[i]
            
        #k such that t_(k-1) < x < t_k   x cant be equal to any of t_k because we looped over them
        k = bin_s(ts,x)  

        x_k1 = ts[k - 1]
        x_k = ts[k]

        h = x_k - x_k1

        m_k1 = ms[k - 1]
        m_k  = ms[k]

        f_k1 = ys[k - 1]
        f_k = ys[k]

        s = (1/h) * (
        (1/6) * m_k1 * (x_k - x)**3
        + (1/6) * m_k * (x - x_k1)**3
        + f_k1 * (x_k - x)
        + f_k   * (x - x_k1)
        - (1/6) * m_k1 * h**2 * (x_k - x)
        - (1/6) * m_k   * h**2 * (x - x_k1)
        )

        return s
    return f

def get_nifs(ts : list[float], yss: list[list[float]]):
    """
    return list of NIFS3 for all lists in yss where values of f(t_k) = y_k
    """
    mss = get_moments(ts,yss)
    nifs = [nifs_from_moments(ts,ys,ms) for ys, ms in zip(yss,mss)]
    return nifs


def plot_nifs(s_x,s_y,m : int):
    points_x = [s_x(i/m) for i in range(m+1)]
    points_y = [s_y(i/m) for i in range(m+1)]

    plt.plot(points_x, points_y)
    plt.show()



ts = [i/95 for i in range(96)]

xs = [
    5.5, 8.5, 10.5, 13.0, 17.0, 20.5, 24.5, 28.0, 32.5, 
    37.5, 40.5, 42.5, 45.0, 47.0, 49.5, 50.5, 51.0, 51.5,
    52.5, 53.0, 52.8, 52.0, 51.5, 53.0, 54.0, 55.0, 56.0,
    55.5, 54.5, 54.0, 55.0, 57.0, 58.5, 59.0, 61.5, 62.5,
    63.5, 63.0, 61.5, 59.0, 55.0, 53.5, 52.5, 50.5, 49.5,
    50.0, 51.0, 50.5, 49.0, 47.5, 46.0, 45.5, 45.5, 45.5,
    46.0, 47.5, 47.5, 46.0, 43.0, 41.0, 41.5, 41.5, 41.0,
    39.5, 37.5, 34.5, 31.5, 28.0, 24.0, 21.0, 18.5, 17.5,
    16.5, 15.0, 13.0, 10.0, 8.0, 6.0, 6.0, 6.0, 5.5, 3.5,
    1.0, 0.0, 0.0, 0.5, 1.5, 3.5, 5.0, 5.0, 4.5, 4.5, 5.5,
    6.5, 6.5, 5.5]

ys = [
    41.0, 40.5, 40.0, 40.5, 41.5, 41.5, 42.0, 42.5, 43.5, 
    45.0, 47.0, 49.5, 53.0, 57.0, 59.0, 59.5, 61.5, 63.0,
    64.0, 64.5, 63.0, 61.5, 60.5, 61.0, 62.0, 63.0, 62.5,
    61.5, 60.5, 60.0, 59.5, 59.0, 58.5, 57.5, 55.5, 54.0,
    53.0, 51.5, 50.0, 50.0, 50.5, 51.0, 50.5, 47.5, 44.0,
    40.5, 36.0, 30.5, 28.0, 25.5, 21.5, 18.0, 14.5, 10.5,
    7.5, 4.0, 2.5, 1.5, 2.0, 3.5, 7.0, 12.5, 17.5, 22.5, 
    25.0, 25.0, 25.0, 25.5, 26.5, 27.5, 27.5, 26.5, 23.5, 
    21.0, 19.0, 17.0, 14.5, 11.5, 8.0, 4.0, 1.0, 0.0, 0.5, 
    3.0, 6.5, 10.0, 13.0, 16.5, 20.5, 25.5, 29.0, 33.0, 35.0, 
    36.5, 39.0, 41.0]



plot_nifs(*get_nifs(ts,[xs,ys]), 10000)
"""
plan:
przerobić tak jak mówił :
najlepiej jak nifs na kazdym odcinku ma równą długość,

zebrać punkty ze zdjęcia
dobrać węzły tak, żeby długość funkcji pomiędzy nimi była taka sama

"""