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
        return None, None, None, None, None, None, None, None
    hour, minute, second = right_ascension.split(" ")
    dec_deg, dec_min, dec_sec = declination.split(" ")
    right_ascension = (float(hour) * 15 +
                       float(minute) / 4 +
                       float(second) / 240) * math.pi / 180
    declination = (float(dec_deg) +
                   float(dec_min) / 60 +
                   float(dec_sec) / 3600) * math.pi / 180
    if parallax < 0.000001:
        return None, None, None, None, None, None, None, None
    parsecs = 1000.0 / parallax
    absolute_magnitude = apparent_magnitude - 5 * (math.log10(parsecs) - 1)
    x = parsecs * math.sin(declination) * math.cos(right_ascension)
    y = parsecs * math.sin(declination) * math.sin(right_ascension)
    z = parsecs * math.cos(declination)
    return designation, x, y, z, absolute_magnitude, 255, 255, 255

with open('hipparcos.csv', 'w') as output_file:
    writer = csv.writer(output_file)
    writer.writerow(['designation', 'x', 'y', 'z',
                     'absmag', 'red', 'green', 'blue'])
    with open('hip_main.dat') as input_file:
        for row in input_file:
            row = row.strip()
            designation, x, y, z, absmag, r, g, b = ParseStar(row)
            if not designation:
                continue
            writer.writerow([designation,
                             '%.8f' % x, '%.8f' % y, '%.8f' % z,
                             '%.2f' % absmag, r, g, b])
