# Performs a live load test. Sends lots of real traffic.
#
# Do not use this script unless you have written permission from the owners
# of galaxyatlas.net. This script runs a denial of service attack. We use
# it to simulate a traffic spike to the website. Using it to send huge
# amounts of traffic to galaxyatlas.net without our permission is a serious
# crime.

import math
import os
import random
import time
import urllib2

target_host = 'http://localhost:8080'
qps = 3

def Magnitude(v):
    return math.sqrt(sum(x * x for x in v))

def VectorMultiply(v, mult):
    return [mult * x for x in v]

def Normalize(v):
    mag = Magnitude(v)
    if mag < 0.000000001:
        return v
    return VectorMultiply(v, float(1) / mag)

def RandomUnitVector():
    while True:
        v = random.uniform(-1, 1), random.uniform(-1, 1), random.uniform(-1, 1)
        mag = Magnitude(v)
        if mag < 1 and mag > 0.0001:
            return Normalize(v)

def MakeCallAtRandomLocation():
    sigma = 100
    x = random.gauss(0, sigma)
    y = random.gauss(0, sigma)
    z = random.gauss(0, sigma)
    args = ('visibleStars?pointX=' + str(x) + '&pointY=' + str(y) +
            '&pointZ=' + str(z) + '&minLum=0.001')
    url = os.path.join(target_host, args)
    response = urllib2.urlopen(url).read()
    print response.count('},'), len(response)

next_request_times = []

for i in range(qps):    
    next_request_times.append(time.time() + random.random())

while True:
    for i, t in enumerate(next_request_times):
        if t > time.time():
            continue
        next_request_times[i] = time.time() + 1 + random.uniform(-0.001, 0.001)
        MakeCallAtRandomLocation()
    time.sleep(0)
