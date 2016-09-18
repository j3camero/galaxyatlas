import math

import numpy as np
from scipy.optimize import minimize

points = []
with open('1d.csv') as f:
    for line in f:
        tokens = line.strip().split(',')
        if len(tokens) == 2:
            points.append([float(t) for t in tokens])

points = [p for p in points if abs(p[0]) > 1.5]

def Rmse(params):
    total_squared_error = 0
    largest_error_index = -1
    largest_error = -1
    for i, (x, y) in enumerate(points):
        sigmoid = float(1) / (1 + math.exp(params[0] * x))
        bb = float(1) / (1 + math.exp(params[1] * x + params[2]))
        bump = params[3] * bb * (1.0 - bb)
        model = sigmoid + bump
        err = (model - y) ** 2
        total_squared_error += err
        if err > largest_error:
            largest_error = err
            largest_error_index = i
    return (math.sqrt(total_squared_error / len(points)),
            largest_error_index,
            math.sqrt(largest_error))

def LossFunction(params):
    return Rmse(params)[0]

def RemoveWorstDataPoint(params):
    rmse, worst_index, worst_error = Rmse(params)
    line = [len(points), worst_error, params[0], params[1], params[2],
            params[3]]
    print ','.join(str(s) for s in line)
    del points[worst_index]

incremental_params = np.array([-2.0, -3.0, 2.0, 2.0])
#while len(points) > 300:
res = minimize(LossFunction, incremental_params, method='nelder-mead',
               options={'xtol': 1e-8, 'disp': True})
print res.x
    #incremental_params = res.x
    #RemoveWorstDataPoint(incremental_params)
