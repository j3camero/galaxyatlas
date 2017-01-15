import csv
import math
import sys

# Load stars from the HYG catalog.
#hyg = {}
#with open('../abbreviator/data/hygdata_v3.csv') as hyg_file:
#    reader = csv.DictReader(hyg_file)
#    for row in reader:
#        hip = row['hip']
#        mag = float(row['mag'])
#        hyg[hip] = mag

def ConvertStar(row):
    hip = row['hip']
    tyc = row['tycho2_id']
    designation = ''
    if hip:
        designation = 'HIP ' + hip
        #if hip in hyg:
        #    del hyg[hip]
    elif tyc:
        designation = 'TYC ' + tyc
    right_ascension = float(row['ra']) * math.pi / 180
    declination = float(row['dec']) * math.pi / 180
    try:
        apparent_magnitude = float(row['phot_g_mean_mag'])
    except:
        apparent_magnitude = float(30)
    parallax = float(row['parallax'])
    if parallax < 1e-7:
        return None, None, None, None, None, None, None, None
    parsecs = 1000.0 / parallax
    absolute_magnitude = apparent_magnitude - 5 * (math.log10(parsecs) - 1)
    x = parsecs * math.cos(declination) * math.cos(right_ascension)
    y = parsecs * math.cos(declination) * math.sin(right_ascension)
    z = parsecs * math.sin(declination)
    assert abs(math.sqrt(x*x + y*y + z*z) - parsecs) < 0.0001
    return designation, x, y, z, absolute_magnitude, 255, 255, 255

with open('tgas.csv', 'w') as output_file:
    writer = csv.writer(output_file)
    writer.writerow(['designation', 'x', 'y', 'z',
                     'absmag', 'red', 'green', 'blue'])
    for i in range(16):
        input_shard_filename = 'TgasSource_000-000-' + str(i).zfill(3) + '.csv'
        print 'Crunching', input_shard_filename
        with open(input_shard_filename) as input_file:
            reader = csv.DictReader(input_file)
            for row in reader:
                designation, x, y, z, absmag, r, g, b = ConvertStar(row)
                if not designation:
                    continue
                writer.writerow([designation,
                                 '%.8f' % x, '%.8f' % y, '%.8f' % z,
                                 '%.2f' % absmag, r, g, b])
