import csv
import math

import numpy as np
from PIL import Image

width = 854
height = 480
fov_multiplier = 1.73  # For 60 degrees, set to 1.73. For 90 degrees, set to 1.
minwh2 = 0.5 * min(width, height)

class Star:
    def __init__(self, ra, dec, parallax, g_flux, bp_flux, rp_flux):
        self.ra = ra
        self.dec = dec
        self.parallax = parallax
        self.g_flux = g_flux
        self.bp_flux = bp_flux
        self.rp_flux = rp_flux
        distance_parsecs = 1000 / parallax
        distance_ly = distance_parsecs * 3.26156
        ra_rad = ra * math.pi / 180
        dec_rad = (dec + 90) * math.pi / 180
        self.x = distance_ly * math.sin(dec_rad) * math.cos(ra_rad)
        self.y = distance_ly * math.sin(dec_rad) * math.sin(ra_rad)
        self.z = distance_ly * math.cos(dec_rad)
        self.absolute_luminosity = g_flux * distance_ly**2

def ParseFloat(s):
    try:
        return float(s)
    except:
        return 0

stars = []
with open('lmc-stars.csv', 'rb') as input_file:
    reader = csv.DictReader(input_file)
    for row in reader:
        stars.append(Star(
            ParseFloat(row['ra']),
            ParseFloat(row['dec']),
            ParseFloat(row['parallax']),
            ParseFloat(row['phot_g_mean_flux']),
            ParseFloat(row['phot_bp_mean_flux']),
            ParseFloat(row['phot_rp_mean_flux'])
            ))

def ProjectPointOntoVector(p, v):
    return np.dot(p, v) / dot(v, v)

def IntegrateFromPointOfView(position, direction, up):
    g_flux = np.zeros((width, height))
    red_flux = np.zeros((width, height))
    blue_flux = np.zeros((width, height))
    right = -np.cross(direction, up)
    for s in stars:
        transformed = [s.x - position[0], s.y - position[1], s.z - position[2]]
        x = np.dot(transformed, right)
        y = np.dot(transformed, up)
        z = np.dot(transformed, direction)
        if z < 1:
            continue
        sx = int(width / 2 + fov_multiplier * minwh2 * x / z)
        sy = int(height / 2 - fov_multiplier * minwh2 * y / z)
        if sx < 0 or sx >= width or sy < 0 or sy >= height:
            continue
        d2 = x**2 + y**2 + z**2
        apparent_luminosity = s.absolute_luminosity / d2
        g_flux[sx,sy] += apparent_luminosity
        redness = 0.5
        if s.rp_flux + s.bp_flux > 0:
            redness = s.rp_flux / (s.rp_flux + s.bp_flux)
        red_flux[sx,sy] += apparent_luminosity * redness
        blue_flux[sx,sy] += apparent_luminosity * (1 - redness)
    return g_flux, red_flux, blue_flux

# Mix the two colors in the proportion specified by the ratio.
def MixColors(color1, color2, ratio):
    r = ratio * color2[0] + (1 - ratio) * color1[0]
    g = ratio * color2[1] + (1 - ratio) * color1[1]
    b = ratio * color2[2] + (1 - ratio) * color1[2]
    return r, g, b

# Converts a color's components to integer values.
def IntColor(c):
    return (int(c[0]), int(c[1]), int(c[2]))

# What fraction of the way between lo and hi is the value? If outside the
# range of (lo,hi), it's capped to 0 and 1 respectively.
def CappedRange(lo, hi, value):
    if value < lo:
        return float(0)
    elif value > hi:
        return float(1)
    else:
        return float(value - lo) / (hi - lo)

# redness is a number between 0 and 1. It's the ratio of red to blue light.
def RednessRatioToColor(redness):
    red = (255, 0, 0)
    blue = (0, 0, 255)
    return MixColors(red, blue, CappedRange(0.3, 0.9, redness))

# g_normalized: a number between 0 and 1 representing the percentile
#               brightness of a pixel.
# red_flux: how much total red flux in a pixel. No need to normalize.
# blue_flux: how much total blue flux in a pixel. No need to normalize.
def FluxToColor(g_normalized, red_flux, blue_flux):
    redness = 0.6
    if red_flux + blue_flux > 0:
        redness = red_flux / (red_flux + blue_flux)
    base_color = RednessRatioToColor(redness)
    black = (0, 0, 0)
    white = (255, 255, 255)
    if g_normalized < 0.5:
        return MixColors(black, base_color, CappedRange(0, 0.5, g_normalized))
    else:
        return MixColors(base_color, white, CappedRange(0.5, 1, g_normalized))

# Normalizes a raw flux value into the range [0,1].
def FluxPercentile(flux, sorted_sample):
    lo = 0
    hi = len(sorted_sample)
    while hi - lo > 1:
        mid = int((lo + hi) / 2)
        if flux >= sorted_sample[mid]:
            lo = mid
        else:
            hi = mid
    return 1.0 * lo / len(sorted_sample)

frame_number = 1

def RenderImageFromFlux(g_flux, red_flux, blue_flux):
    global frame_number
    sorted_flux = []
    for i in range(width):
        for j in range(height):
            flux = g_flux[i,j]
            if flux > 0.000000001:
                sorted_flux.append(flux)
    sorted_flux.sort()
    image = Image.new('RGB', (width, height))
    for i in range(width):
        for j in range(height):
            p = FluxPercentile(g_flux[i,j], sorted_flux)
            color = FluxToColor(p, red_flux[i,j], blue_flux[i,j])
            image.putpixel((i, j), IntColor(color))
    image.save('frames/lmc%05d.png' % frame_number)
    frame_number += 1

def RenderFrameFromPointOfView(position, direction, up):
    g_flux, red_flux, blue_flux = IntegrateFromPointOfView(position, direction, up)
    RenderImageFromFlux(g_flux, red_flux, blue_flux)

num_frames = 10 * 30
up = np.array([0, 1, 0])
lmc = np.array([8950, 59000, 152880])
orbit_radius = 100 * 1000
for i in range(num_frames):
    print 'Frame', (i + 1), 'of', num_frames
    angle = 2 * math.pi * i / num_frames
    direction = np.array([math.sin(angle), 0, -math.cos(angle)])
    position = lmc - orbit_radius * direction
    RenderFrameFromPointOfView(position, direction, up)
