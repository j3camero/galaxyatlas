import random

def SampleOnce(s, w):
    x, y, z = random.gauss(0, w), random.gauss(0, w), random.gauss(0, w)
    squared_distance = (x - s)**2 + y**2 + z**2
    if squared_distance <= 0:
        return 999999999
    return float(1) / squared_distance

def SampleMany(s, w, trials):
    total = 0
    for i in xrange(trials):
        total += SampleOnce(s, w)
    return total / trials

def arange(lo, hi, increment):
    r = []
    while lo < hi:
        r.append(lo)
        lo += increment
    return r

def GridSample(max_separation, max_width, trials, increment, progress=False):
    with open('sample.csv', 'w') as output_file:
        output_file.write('s V w ->')
        for w in arange(0, max_width, increment):
            output_file.write(',' + str(w))
        output_file.write('\n')
        for s in arange(0, max_separation, increment):
            output_file.write(str(s))
            for w in arange(0, max_width, increment):
                if progress:
                    print s, w
                output_file.write(',' + str(SampleMany(s, w, trials)))
            output_file.write('\n')

GridSample(2.5, 2.5, 1000 * 1000, 0.1, True)
