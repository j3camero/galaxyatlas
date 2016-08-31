

class Octree:
    def __init__(self, x, y, z, radius, max_leaf_node_size):
        self.x = x
        self.y = y
        self.z = z
        self.radius = radius
        self.max_leaf_node_size = max_leaf_node_size
        self.stars = []
        self.children = None
        self.max_luminosity = 0

    def which_child(self, star):
        child_number = 0
        if star.x > self.x:
            child_number += 4
        if star.y > self.y:
            child_number += 2
        if star.z > self.z:
            child_number += 1
        return child_number

    @staticmethod
    def get_direction(child_number):
        if child_number == 0:
            return -1, -1, -1
        if child_number == 1:
            return -1, -1, +1
        if child_number == 2:
            return -1, +1, -1
        if child_number == 3:
            return -1, +1, +1
        if child_number == 4:
            return +1, -1, -1
        if child_number == 5:
            return +1, -1, +1
        if child_number == 6:
            return +1, +1, -1
        if child_number == 7:
            return +1, +1, +1
        return None

    def insert(self, star):
        if star.luminosity > self.max_luminosity:
            self.max_luminosity = star.luminosity
        if self.children is None:
            self.stars.append(star)
            if len(self.stars) > self.max_leaf_node_size:
                self.split_leaf()
        else:
            child_number = self.which_child(star)
            child = self.children[child_number]
            child.insert(star)

    def total_num_stars(self):
        if self.children is None:
            return len(self.stars)
        total = 0
        for child in self.children:
            total += child.total_num_stars()
        return total

    def num_nodes(self):
        if self.children is None:
            return 1
        total = 0
        for child in self.children:
            total += child.num_nodes()
        return total + 1

    def split_leaf(self):
        self.children = []
        r = 1.0 * self.radius / 2
        for i in range(8):
            dx, dy, dz = self.get_direction(i)
            x = self.x + dx * r
            y = self.y + dy * r
            z = self.z + dz * r
            child = Octree(x, y, z, r, self.max_leaf_node_size)
            self.children.append(child)
        for star in self.stars:
            self.insert(star)
        self.stars = None

    def is_visible(self, camera, brightness_threshold):
        camera_x, camera_y, camera_z = camera
        dx, dy, dz = self.x - camera_x, self.y - camera_y, self.z - camera_z
        squared_distance = dx**2 + dy**2 + dz**2 - 3 * self.radius**2
        if squared_distance <= 0:
            return True
        else:
            brightness = self.max_luminosity / squared_distance
            return brightness > brightness_threshold

    def render(self, camera, crh, srh, crv, srv,
               width, height, screen, brightness_threshold):
        if not self.is_visible(camera, brightness_threshold):
            return 0
        if self.children is not None:
            num_stars = 0
            for child in self.children:
                num_stars += child.render(camera, crh, srh, crv, srv,
                                          width, height, screen,
                                          brightness_threshold)
            return num_stars
        if self.stars is not None:
            for star in self.stars:
                star.render(camera, crh, srh, crv, srv, width, height, screen)
            return len(self.stars)
