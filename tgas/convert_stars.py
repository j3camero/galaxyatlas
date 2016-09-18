import csv
import math
import sys

def ConvertStar(row):
    hip = row['hip']
    tyc = row['tycho2_id']
    designation = ''
    if hip:
        designation = 'HIP ' + hip
    elif tyc:
        designation = 'TYC ' + tyc
    right_ascension = float(row['ra']) * math.pi / 180
    declination = float(row['dec']) * math.pi / 180
    try:
        apparent_magnitude = float(row['phot_g_mean_mag'])
    except:
        apparent_magnitude = float(30)
    parallax = float(row['parallax'])
    if parallax <= 0:
        return None, None, None, None, None, None
    parsecs = 1000.0 / parallax
    absolute_magnitude = apparent_magnitude - 5 * (math.log10(parsecs) - 1)
    luminosity = math.pow(2.512, -absolute_magnitude)
    #luminosity = max(0, 10 - absolute_magnitude)
    #declination += math.pi / 2
    x = parsecs * math.sin(declination) * math.cos(right_ascension)
    y = parsecs * math.sin(declination) * math.sin(right_ascension)
    z = parsecs * math.cos(declination)
    return designation, x, y, z, luminosity, 'G2'

#filename = 'sample.csv'
star_count = 0
with open('tgas.csv', 'w') as output_file:
    writer = csv.writer(output_file)
    writer.writerow(['mjid', 'designation', 'x', 'y', 'z',
                     'luminosity', 'spectral'])
    for i in range(16):
        input_shard_filename = 'TgasSource_000-000-' + str(i).zfill(3) + '.csv'
        print 'Crunching', input_shard_filename
        with open(input_shard_filename) as input_file:
            reader = csv.DictReader(input_file)
            for row in reader:
                star_count += 1
                designation, x, y, z, luminosity, spectral = ConvertStar(row)
                if not designation:
                    continue
                writer.writerow([star_count, designation, x, y, z,
                                 luminosity, spectral])