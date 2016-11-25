import csv
import sys

output_csv_filename = 'thc.csv'

def WriteOneStar(csv_row_as_list):
    with open(output_csv_filename, 'a') as output_file:
        writer = csv.writer(output_file)
        writer.writerow(csv_row_as_list)

# Load the Hipparcos stars, keyed by their HIP numbers.
hipparcos = {}
with open('hipparcos.csv') as input_file:
    input_file.readline()  # Skip csv headers.
    reader = csv.reader(input_file)
    for row in reader:
        designation = row[0]
        hipparcos[designation] = row

# Clear the output file.
with open(output_csv_filename, 'w') as f:
    pass

# Write the TGAS stars, removing any stars that overlap from Hipparcos.
WriteOneStar(['designation', 'x', 'y', 'z', 'absmag', 'red', 'green', 'blue'])
with open('tgas.csv') as input_file:
    input_file.readline()  # Skip csv headers.
    reader = csv.reader(input_file)
    for row in reader:
        WriteOneStar(row)
        if reader.line_num % 100000 == 0:
            sys.stdout.write('.')
            sys.stdout.flush()
        designation = row[0]
        hipparcos.pop(designation, None)

# Write the remaining Hipparcos stars, that weren't eliminated for
# overlapping with TGAS.
for hip in hipparcos:
    row = hipparcos[hip]
    WriteOneStar(row)

print ''
