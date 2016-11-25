import csv
import math

def ParseStar(row):
    tokens = row.split("|")
    designation = 'HIP ' + tokens[1].strip()
    right_ascension = tokens[3]
    declination = tokens[4]
    try:
        apparent_magnitude = float(tokens[5])
    except:
        apparent_magnitude = 30
    try:
        parallax = float(tokens[11])
    except:
        parallax = -1
    hour, minute, second = right_ascension.split(" ")
    dec_deg, dec_min, dec_sec = declination.split(" ")
    right_ascension = (float(hour) * 15 +
                       float(minute) / 4 +
                       float(second) / 240) * math.pi / 180
    declination = (float(dec_deg) +
                   float(dec_min) / 60 +
                   float(dec_sec) / 3600) * math.pi / 180
    if parallax > 0:
        parsecs = 1000.0 / parallax
        absolute_magnitude = apparent_magnitude - 5 * (math.log10(parsecs) - 1)
    else:
        parsecs = 1000
        absolute_magnitude = 0
    luminosity = math.pow(2.512, -absolute_magnitude)
    #luminosity = max(0, 10 - absolute_magnitude)
    x = parsecs * math.sin(declination) * math.cos(right_ascension)
    y = parsecs * math.sin(declination) * math.sin(right_ascension)
    z = parsecs * math.cos(declination)
    return designation, x, y, z, luminosity, 255, 255, 255

with open('hipparcos.csv', 'w') as output_file:
    writer = csv.writer(output_file)
    writer.writerow(['designation', 'x', 'y', 'z',
                     'luminosity', 'red', 'green', 'blue'])
    with open('hip_main.dat') as input_file:
        for row in input_file:
            row = row.strip()
            designation, x, y, z, luminosity, r, g, b = ParseStar(row)
            if not designation:
                continue
            writer.writerow([designation,
                             '%.2f' % x, '%.2f' % y, '%.2f' % z,
                             '%.2f' % luminosity, r, g, b])
