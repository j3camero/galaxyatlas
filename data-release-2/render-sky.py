from PIL import Image

import numpy as np

print 'Loading'
g_sums = np.load('g_sums.npy')
bp_sums = np.load('bp_sums.npy')
rp_sums = np.load('rp_sums.npy')

print 'Flattening'
g_sums_flat = []
w, h = g_sums.shape
for i in range(w):
    for j in range(h):
        flux = g_sums[i,j]
        if flux > 0.000000001:
            g_sums_flat.append(flux)

print 'Sorting'
g_sums_flat.sort()
print 'Min value:', g_sums_flat[0]
print 'Max value:', g_sums_flat[-1]
print 'Normalize'
precision = 256
cutoffs = []
for i in range(precision):
    cutoffs.append(g_sums_flat[int(i * len(g_sums_flat) / precision)])

# Normalizes a raw flux value into the range [0,1].
def FluxPercentile(flux):
    lo = 0
    hi = len(cutoffs)
    while hi - lo > 1:
        mid = int((lo + hi) / 2)
        if flux >= cutoffs[mid]:
            lo = mid
        else:
            hi = mid
    return 1.0 * lo / precision

print 'Rendering'
image = Image.new('RGB', (w, h))
for i in range(w):
    if i % 100 == 0:
        print '    Rendering', i, 'of', w
    for j in range(h):
        p = FluxPercentile(g_sums[i,j])
        divisor = bp_sums[i,j] + rp_sums[i,j]
        if divisor > 0.000000001:
            red_blue_mix = rp_sums[i,j] / divisor
        else:
            red_blue_mix = 0.5
        # Rescale for higher contrast.
        #red_blue_mix = max(0, min(1, 4/3 * red_blue_mix - 1/3))
        if red_blue_mix < 0.5:
            red_channel = 2 * red_blue_mix
            blue_channel = 1
        else:
            red_channel = 1
            blue_channel = 2 * (1 - red_blue_mix)
        color = (int(p * red_channel * 256), 0, int(p * blue_channel * 256))
        image.putpixel((i, j), color)
image.save('earth-sky.png')
print 'Done'
