import random

for i in xrange(5):
    d = 620
    x, y, z = random.gauss(0, d), random.gauss(0, d), random.gauss(0, d)
    brightness = 0
    color = [1, 0, random.random()]
    random.shuffle(color)
    r, g, b = color
    print '%.2f,%.2f,%.2f,%.3f,%.1f,%.1f,%.1f' % (x, y, z, brightness, r, g, b)
