# Quick script that prints the percentage of red light for all LMC stars.
# This is to quickly ball-park a mapping between the red-blue ratio vs actual
# RGB colors.

import csv

def ParseFloat(s):
    try:
        return float(s)
    except:
        return 0

with open('lmc-stars.csv', 'rb') as input_file:
    reader = csv.DictReader(input_file)
    for row in reader:
        blue = ParseFloat(row['phot_bp_mean_flux'])
        red = ParseFloat(row['phot_rp_mean_flux'])
        total = red + blue
        if total > 0:
            red_percentage = red / total
            print red_percentage
