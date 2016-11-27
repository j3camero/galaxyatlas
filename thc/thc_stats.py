import csv

minx, maxx = 0, 0
miny, maxy = 0, 0
minz, maxz = 0, 0
with open('thc.csv') as input_file:
    reader = csv.DictReader(input_file)
    for row in reader:
        x, y, z = float(row['x']), float(row['y']), float(row['z'])
        minx = min(x, minx)
        maxx = max(x, maxx)
        miny = min(y, miny)
        maxy = max(y, maxy)
        minz = min(z, minz)
        maxz = max(z, maxz)
print 'x:', minx, '-', maxx
print 'y:', miny, '-', maxy
print 'z:', minz, '-', maxz
