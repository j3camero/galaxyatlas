#!/usr/bin/python

import csv
import re

romanNumerals = ["I","II","III","IV","V"]
romanNumerals.reverse()
colorMap = {}

with open('colormap.csv', 'rb') as colorfile:
    reader = csv.reader(colorfile)
    # Read field names
    header = reader.next()
    for row in reader:
        colorMap[row[0]] = [row[1],row[2],row[3],row[4],row[5],row[6]]

stars = []
with open('hygdata_min.csv', 'rb') as starfile:
    reader = csv.reader(starfile, delimiter=',')
    # Read field names
    header = reader.next()
    print header
    for row in reader:
        stars.append(row)


strangeSpects = []
normalSpects = []
missingSpects = []

isStrange = re.compile(r"[^/\- \\:\.]*[/\- \\:\.][^/\- \\:\.]*")
for star in stars:
    if len(star[5]) > 0:
        match = isStrange.match(star[5])
        if match is not None:
            strangeSpects.append(star)
            #print star[5]
        else:
            normalSpects.append(star)
    else:
        missingSpects.append(star)

print "After scanning for unrecognized characters we have:"
print "Total stars: " + str(len(normalSpects) + len(strangeSpects) +
                            len(missingSpects))
print "Seems normal: " + str(len(normalSpects))
print "Odd characters: " + str(len(strangeSpects))
print "No characters: " + str(len(missingSpects))

foundStars = []
modFoundStars = 0
notFoundStars = []
for star in normalSpects:
    if star[5] in colorMap.keys():
        foundStars.append(star)
    else:
        found = False
        for num in romanNumerals:
            newName = star[5] + num
            if newName in colorMap.keys():
                found = True
                star[5] = newName
                foundStars.append(star)
                modFoundStars += 1
                break
        if not found:
            notFoundStars.append(star)

print "After first pass:"
print "Total recognizable stars: " + str(len(foundStars))
print "Unrecognizable stars: " + str(len(notFoundStars))
print "Stars modified: " + str(modFoundStars)

matchSpectLetter = re.compile(r"^([OBAFGKMNobafgkmn]).*")
goodSpectLetters = []
badSpectLetters = []
for star in (notFoundStars + strangeSpects):
    match = matchSpectLetter.match(star[5])
    if match is not None:
        star[5] = star[5][0].upper() + star[5][1:]
        goodSpectLetters.append(star)
    else:
        badSpectLetters.append(star)

print "Good vs Bad Spectral Letter"
print len(goodSpectLetters)
print len(badSpectLetters)

notNeutron = []
for star in goodSpectLetters:
    if star[5][0].upper() == 'N':
        star[5] = "N"
        foundStars.append(star)
    else:
        notNeutron.append(star)

print "New found stars count"
print len(foundStars)
print "Neutron vs Not Neutron"
print len(goodSpectLetters) - len(notNeutron)
print len(notNeutron)

matchesLetterNumberRoman = re.compile(
    r"([OBAFGKMN])([0-9])(IV|III|II|I|V).*")
lookingLessLikely = []
for star in notNeutron:
    match = matchesLetterNumberRoman.match(star[5])
    if match is not None:
        newName = match.group(1) + match.group(2) + match.group(3)
        if newName in colorMap.keys():
            star[5] = newName
#            print newName
            foundStars.append(star)
            continue

        found = False
        for i in range(10):
            newASName = match.group(1) + str(i) + match.group(3)
            if newASName in colorMap.keys():
                star[5] = newASName
                foundStars.append(star)
                found = True
                break
        if found:
            continue

        found = False
        for num in romanNumerals:
            newMSName = match.group(1) + match.group(2) + num
            if newMSName in colorMap.keys():
                star[5] = newMSName
#                print newMSName
                foundStars.append(star)
                found = True
                break
        if not found:
            lookingLessLikely.append(star)
    else:
        lookingLessLikely.append(star)

print "New found stars count"
print len(foundStars)
print len(lookingLessLikely)

print (len(foundStars) + len(lookingLessLikely) + len(missingSpects) +
       len(badSpectLetters))

for star in lookingLessLikely:
    star[5] = "G2V"
    foundStars.append(star)

for star in missingSpects:
    star[5] = "G2V"
    foundStars.append(star)

for star in badSpectLetters:
    star[5] = "G2V"
    foundStars.append(star)

print len(foundStars)

with open("hygdata_min2.csv", 'w') as outfile:
    for star in foundStars:
        for field in star[:-1]:
            outfile.write(field)
            outfile.write(",")
        outfile.write(star[-1])
        outfile.write('\n')

with open("hygdata_min3.csv", 'w') as outfile:
    for star in foundStars:
        for field in star[:-1]:
            outfile.write(field)
            outfile.write(",")

        r = colorMap[star[5]][2]
        g = colorMap[star[5]][3]
        b = colorMap[star[5]][4]
        outfile.write(str(r) + "," + str(g) + "," + str(b))
        outfile.write('\n')
