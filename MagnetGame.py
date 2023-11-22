import numpy as np

class Player:
    def __init__(self, polarity):
        self.score = 0

class Magnet:
    def __init__(self, polarity, position, strength=1):
        self.polarity = polarity
        self.position = position
        self.strength = strength

class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

class Grid:
    def __init__(self, n):
        self.n = n
        self.grid = [[None for _ in range(n)] for _ in range(n)]
        self.points = [Point(x, y) for x in range(n) for y in range(n)]
        self.magnets = {}

    def get(self, point):
        return self.grid[point.x][point.y]

    def set(self, point, value):
        self.grid[point.x][point.y] = value
        if isinstance(value, Magnet):
            self.magnets[point] = value
        else:
            self.magnets.pop(point, None)
        
    def place_magnet(grid, magnet, point):
        grid.set(point, magnet)

    def calculate_force(self, magnet1, magnet2):
        distance = np.linalg.norm(np.array(magnet1.position) - np.array(magnet2.position))
        force = (magnet1.strength + magnet2.strength) / distance**2
        if magnet1.polarity == magnet2.polarity:
            force = -force
        print("Force", force)
        return force

    def move_magnet(self, magnet, force):
        magnet.position = [magnet.position[0] + force[0], magnet.position[1] + force[1]]

    def calculate_net_force(self, magnets):
        print("MAGNETS", magnets)
        net_forces = {}
        for magnet1 in magnets:
            net_forces[magnet1] = [0, 0]
            for magnet2 in magnets:
                if magnet1 == magnet2:
                    continue
                force = self.calculate_force(magnet1, magnet2)
                direction = (np.array(magnet2.position) - np.array(magnet1.position)).astype(float)
                direction /= np.linalg.norm(direction)
                net_forces[magnet1] += force * direction
        return net_forces

    def handle_collision(self, magnet1, magnet2):
        force = self.calculate_force(magnet1, magnet2)
        if force < 0:
            self.move_magnet(magnet1, -force)
            self.move_magnet(magnet2, force)

    def move_magnet(self, magnet, direction):
        magnet.position = [magnet.position[0] + direction[0], magnet.position[1] + direction[1]]

    def move_magnets(self):
        magnets = list(self.magnets.values())
        while True:
            net_forces = self.calculate_net_force(magnets)
            equilibrium = True
            for magnet in magnets:
                force = net_forces[magnet]
                if np.linalg.norm(force) >= 1:
                    equilibrium = False
                    direction = force / np.linalg.norm(force)
                    self.move_magnet(magnet, direction)
                    #point = self.points[magnet.position[0] * self.n + magnet.position[1]]
                    #other_magnet = self.get(point)
                    #if other_magnet is not None:
                    #    self.handle_collision(magnet, other_magnet)
                    #self.set(point, magnet)
            if equilibrium:
                break

    def game_loop(self, players, board, magnets):
        for player in players:
            # player places a magnet
            # calculate forces and move magnets
            # handle collisions
            pass

# initialize game
n = 5
board = np.zeros((n, n))
players = [Player(1), Player(-1)]
magnets = []
grid = Grid(n)
grid.place_magnet(Magnet(1, [1, 1], 10), Point(1, 1))
grid.place_magnet(Magnet(0, [1, 1], 10), Point(0, 2))
grid.place_magnet(Magnet(0, [1, 1], 10), Point(0, 4))
grid.place_magnet(Magnet(1, [3, 3], 10), Point(3, 3))
grid.move_magnets()
print([magnet.position for magnet in grid.magnets.values()])
# game loop
#grid.game_loop(players, board, magnets)
