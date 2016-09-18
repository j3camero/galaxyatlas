import math
import random
import sys

import pygame

from octree import Octree
from star import Star

def load_stars(filename):
    stars = []
    data_file = open("hip_main.dat")
    count = 0
    sys.stdout.write("Loading stars")
    for line in data_file:
        count += 1
        if count % 2000 == 0:
            sys.stdout.write(".")
            sys.stdout.flush()
        star = Star.parse(line)
        if star is not None and star.distance(0, 0, 0) < 100:
            stars.append(star)
    print ""
    data_file.close()
    sun = Star("Sol", 0, 0, 0, 255, 0, 0, 0)
    stars.append(sun)
    return stars

def make_octree(stars):
    octree = Octree(0, 0, 0, 2000, 4)
    for star in stars:
        octree.insert(star)
    return octree

def trunc(s, length):
    if len(s) > length:
        return s[0:length]
    else:
        return s

stars = load_stars("hip_main.dat")
octree = make_octree(stars)
for child in octree.children:
    num_stars = child.total_num_stars()
    luminosity = child.max_luminosity
    print "    *", num_stars, luminosity
sys.exit(0)

camera_bearing = 0.0
camera_inclination = 0.0
camera_x = 0.0
camera_y = 0.0
camera_z = 0.0
camera_speed = 10.0
width = 800
height = 600

pygame.init()
screen = pygame.display.set_mode((width, height),
                                 pygame.FULLSCREEN |
                                 pygame.DOUBLEBUF |
                                 pygame.HWSURFACE)

clock = pygame.time.Clock()
running = True

pygame.mouse.set_visible(False)
pygame.event.set_grab(True)
pygame.event.clear()

while running:
    fps = clock.get_fps()
    if fps < 1: fps = 30
    screen.fill((0, 0, 0))
    rh = camera_bearing
    rv = -camera_inclination
    crh = math.cos(rh)
    crv = math.cos(rv)
    srh = math.sin(rh)
    srv = math.sin(rv)
    num_visible = octree.render((camera_x, camera_y, camera_z),
                                crh, srh, crv, srv,
                                width, height, screen,
                                0.000005)
    print num_visible
    #sys.exit(0)
    keys = pygame.key.get_pressed()
    number_key_codes = [None, None, pygame.K_2, pygame.K_3, pygame.K_4,
                        pygame.K_5, pygame.K_6, pygame.K_7, pygame.K_8,
                        pygame.K_9, pygame.K_0]
    for i, key in enumerate(number_key_codes):
        if key is not None and keys[key]:
            camera_speed = 2 * i
    x, y, z = camera_x, camera_y, camera_z
    rh = 0.5 * math.pi - camera_bearing
    rv = camera_inclination + 0.5 * math.pi
    distance = camera_speed / fps
    dx = distance * math.sin(rv) * math.cos(rh)
    dy = distance * math.cos(rv)
    dz = distance * math.sin(rv) * math.sin(rh)
    if keys[pygame.K_w] or keys[pygame.K_UP]:
        x += dx
        y += dy
        z += dz
    if keys[pygame.K_s] or keys[pygame.K_DOWN]:
        x -= dx
        y -= dy
        z -= dz
    if keys[pygame.K_a] or keys[pygame.K_LEFT]:
        x -= dz
        y += dy
        z += dx
    if keys[pygame.K_d] or keys[pygame.K_RIGHT]:
        x += dz
        y += dy
        z -= dx
    if keys[pygame.K_1]:
        camera_speed = 0.01
    camera_x, camera_y, camera_z = x, y, z
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False
        elif event.type == pygame.KEYDOWN:
            if event.key == pygame.K_ESCAPE:
                running = False
        elif event.type == pygame.MOUSEMOTION:
            dh = event.rel[0]
            dv = event.rel[1]
            mouse_sensitivity = 0.005
            camera_bearing += mouse_sensitivity * dh
            camera_inclination += mouse_sensitivity * dv
            if camera_bearing > math.pi * 2:
                camera_bearing -= math.pi * 2
            if camera_bearing < 0:
                camera_bearing += math.pi * 2
            if camera_inclination > 0.45 * math.pi:
                camera_inclination = 0.45 * math.pi
            if camera_inclination < -0.45 * math.pi:
                camera_inclination = -0.45 * math.pi
    font = pygame.font.Font(None, 24)
    color = 50, 255, 0
    ra = trunc(str(camera_bearing * 3.819718656), 5)
    text = font.render("ra: " + ra, 1, color)
    location = text.get_rect(left=10, top=10)
    screen.blit(text, location)
    dec = trunc(str(camera_inclination * 57.295779847), 5)
    text = font.render("dec: " + dec, 1, color)
    location = text.get_rect(left=10, top=40)
    screen.blit(text, location)
    pygame.display.flip()
    clock.tick(60)
