
from random import randint,sample
def random_graph(n,neighbours_amnt):
    edges = {i:[] for i in range(n)}
    for i in range(n):
        neighbours = randint(*neighbours_amnt)
        aval = list(range(0,n))
        aval.remove(i)
        edges[i] = sample(aval,k=neighbours)
    return edges

def visualize_graph(graph):
    pass
def find_path(graph:dict,a:int,b:int):
    to_verify = [(a,[a])]
    checked = [0 for _ in range(len(graph))]
    while to_verify:
        i = to_verify[0]
        checked[i[0]] = 1
        to_verify = to_verify[1:]
        for j in graph[i[0]]:
            if checked[j] == 1:
                continue
            j = (j,i[1]+[j])
            if j[0] == b:
                return j[1]
            to_verify.append(j)
    return -1

a = random_graph(100,(2,3))
#print(a)
print(find_path(a,0,10))