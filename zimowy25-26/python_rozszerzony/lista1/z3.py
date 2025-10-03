def tabliczka(x1,x2,y1,y2,d):
    num_x = (x2 - x1) // d
    num_y = (y2 - y1) // d

    xs = [x1 + i * d for i in range(num_x + 1)]
    ys = [y1 + i * d for i in range(num_y + 1)]

    y_width = len(str(y2))
    x_width = len(str(x2 * y2))



    print(' '* y_width + ' ' + ' '.join(map(lambda x : str(x).ljust(x_width) ,xs)))

    for y in ys:
        nums = ' '.join([str(x * y).ljust(x_width) for x in xs])
        y_pad = str(y).ljust(y_width)
        print(y_pad + ' ' + nums)


tabliczka(3,8,2,6,2)