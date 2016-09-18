import math

import pygame
import pygame.font

from star_names import star_names

class Star:
    def __init__(self, hipparcos_id, x, y, z, r, b, g, luminosity):
        self.hipparcos_id = hipparcos_id
        self.x = x
        self.y = y
        self.z = z
        self.r = r
        self.g = g
        self.b = b
        self.luminosity = luminosity
        output = [hipparcos_id,int(x), int(y), int(z), "%.2f" % luminosity]
        print "[" + ",".join([str(x) for x in output]) + "],"

    @staticmethod
    def parse(s):
        tokens = s.split("|")
        hip = int(tokens[1])
        right_ascension = tokens[3]
        declination = tokens[4]
        try:
            apparent_magnitude = float(tokens[5])
        except:
            apparent_magnitude = 30
        try:
            parallax = abs(float(tokens[11]))
        except:
            return None
        hour, minute, second = right_ascension.split(" ")
        dec_deg, dec_min, dec_sec = declination.split(" ")
        math.pi = 3.14159265
        right_ascension = (float(hour) * 15 +
                           float(minute) / 4 +
                           float(second) / 240) * math.pi / 180
        declination = (float(dec_deg) +
                       float(dec_min) / 60 +
                       float(dec_sec) / 3600) * math.pi / 180
        try:
            parsecs = 1000.0 / parallax
            distance = 3.26163626 * parsecs
        except:
            return None
        absolute_magnitude = apparent_magnitude - 5 * (math.log10(parsecs) - 1)
        luminosity = math.pow(2.512, -absolute_magnitude)
        #luminosity = max(0, 10 - absolute_magnitude)
        declination += math.pi / 2
        x = distance * math.sin(declination) * math.cos(right_ascension)
        y = distance * math.sin(declination) * math.sin(right_ascension)
        z = distance * math.cos(declination)
        star = Star(hip, x, y, z, 255, 255, 255, luminosity)
        star.apparent_magnitude_from_earth = apparent_magnitude
        return star

    def distance(self, x, y, z):
        dx = self.x - x
        dy = self.y - y
        dz = self.z - z
        return math.sqrt(dx**2 + dy**2 + dz**2)

    def render(self, camera, crh, srh, crv, srv, width, height, screen):
        camera_x, camera_y, camera_z = camera
        dx = self.x - camera_x
        dy = self.y - camera_y
        dz = self.z - camera_z
        squared_distance = dx**2 + dy**2 + dz**2
        dx, dz = (dx * crh - dz * srh, dx * srh + dz * crh)
        dy, dz = (dy * crv - dz * srv, dy * srv + dz * crv)
        if (squared_distance < 0.00000001 or
            dz < 0.00000001 or dz < dy or dz < dx):
            return
        screen_x = int((dx * width / dz + width) / 2)
        screen_y = int(height - (dy * height / dz + height) / 2)
        if (screen_x < 0 or screen_x > width or
            screen_y < 0 or screen_y > height):
            return
        brightness = 18 * 1000.0 * self.luminosity / squared_distance
        brightness = min(brightness, 5)
        #if self.apparent_magnitude_from_earth < 7:
        #    print self.apparent_magnitude_from_earth, brightness
        self.draw_star_shape(screen, screen_x, screen_y, brightness)
        if self.hipparcos_id in star_names:
            font = pygame.font.Font(None, 18)
            b = min(brightness * 100, 255)
            color = b, b, b
            text = font.render(star_names[self.hipparcos_id], 1, color)
            location = text.get_rect(centerx=screen_x, top=screen_y)
            screen.blit(text, location)

    def draw_star_shape(self, screen, x, y, brightness):
        brightness *= 255
        inner = min(brightness, 255)
        outer = max(brightness / 4 - 64, 0)
        inner = inner, inner, inner
        outer = outer, outer, outer
        screen.set_at((x, y), inner)
        screen.set_at((x+1, y), outer)
        screen.set_at((x-1, y), outer)
        screen.set_at((x, y+1), outer)
        screen.set_at((x, y-1), outer)
