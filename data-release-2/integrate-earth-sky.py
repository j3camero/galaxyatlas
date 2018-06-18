import csv
import gzip
import math
import os

import numpy as np

input_dir = '/Users/jecameron/gaia/gdr2/gaia_source_parallax/csv/'

w = 1920
h = 1080
g_sums = np.zeros((w, h))
bp_sums = np.zeros((w, h))
rp_sums = np.zeros((w, h))

def ParseFloat(s):
    try:
        return float(s)
    except:
        return 0

# How many degrees from the center of the LArge Magellanic Cloud.
def DegreesFromLMC(ra, dec):
    ra_diff = 80.8917 - ra
    dec_diff = -69.7561 - dec
    return math.sqrt(ra_diff**2 + dec_diff**2)

lmc_star_counter = 0

def ProcessShard(filename, csv_writer):
    global g_sums
    global bp_sums
    global rp_sums
    global lmc_star_counter
    first_row = True
    input_filename = os.path.join(input_dir, filename)
    with gzip.open(input_filename, 'rb') as input_file:
        reader = csv.DictReader(input_file)
        for row in reader:
            g_mean_flux = ParseFloat(row['phot_g_mean_flux'])
            bp_mean_flux = ParseFloat(row['phot_bp_mean_flux'])
            rp_mean_flux = ParseFloat(row['phot_rp_mean_flux'])
            parallax = ParseFloat(row['parallax'])
            ra = ParseFloat(row['ra'])
            dec = ParseFloat(row['dec'])
            if first_row:
                first_row = False
                if DegreesFromLMC(ra, dec) > 13:
                    return
            # Take only stars between 100k and 200k lightyears away.
            if parallax > 0.03261 or parallax < 0.0163078:
                continue
            if DegreesFromLMC(ra, dec) > 12:
                continue
            lmc_star_counter += 1
            output_row = [
                row['designation'],
                row['ra'],
                row['dec'],
                row['parallax'],
                row['phot_g_mean_flux'],
                row['phot_bp_mean_flux'],
                row['phot_rp_mean_flux'],
            ]
            csv_writer.writerow(output_row)
            x = int(w * ra / 360)
            y = int(h * (dec + 90) / 180)
            g_sums[x,y] += g_mean_flux
            bp_sums[x,y] += bp_mean_flux
            rp_sums[x,y] += rp_mean_flux

count = 0
with open('lmc-stars.csv', 'wb') as lmc_file:
    writer = csv.writer(lmc_file)
    for filename in sorted(os.listdir(input_dir)):
        if filename.endswith('.csv.gz'):
            print 'Processing shard', count
            ProcessShard(filename, writer)
            count += 1
np.save('g_sums', g_sums)
np.save('bp_sums', bp_sums)
np.save('rp_sums', rp_sums)
print 'lmc_star_counter:', lmc_star_counter
