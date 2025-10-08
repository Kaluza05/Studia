def flatten(xss : list[list])-> list:
    return [x for xs in xss for x in xs]

def tabliczka(x1,x2,y1,y2,d):
    num_x = int((x2 - x1) // d)
    num_y = int((y2 - y1) // d)

    xs = [x1 + i * d for i in range(num_x + 1)]
    ys = [y1 + i * d for i in range(num_y + 1)]
    
    table = {y : [str(x * y) for x in xs] for y in ys}

    y_width = max(map(lambda x: len(str(x)), ys))
    x_width = max(map(len,flatten(table.values())))


    print(' '* y_width + ' ' + ' '.join(map(lambda x : str(x).rjust(x_width) ,xs)))

    for y in ys:
        nums = ' '.join([str(x * y).rjust(x_width) for x in xs])
        y_pad = str(y).rjust(y_width)
        print(y_pad + ' ' + nums)



tabliczka(3,7.5,2,6,1.5)