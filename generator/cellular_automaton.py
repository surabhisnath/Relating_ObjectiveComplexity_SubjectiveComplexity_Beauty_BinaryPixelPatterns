"""
Author: Surabhi S Nath
"""
import numpy as np
import matplotlib.pyplot as plt
import matplotlib
import re
import imageio
import os

def matrix_to_pattern(p):
    """
    This function converts the pattern array to an image sized array
    """
    ratio = 40
    new_data = np.zeros(np.array(p.shape) * ratio)
    for j in range(p.shape[0]):
        for k in range(p.shape[1]):
            if p[j, k] == 0:
                new_data[j * ratio: (j+1) * ratio, k * ratio: (k+1) * ratio] = 1
            else:
                new_data[j * ratio: (j+1) * ratio, k * ratio: (k+1) * ratio] = 0
    new_data = np.uint8(np.round(new_data*255))
    images.append(new_data)
    return new_data

def neighbourhood_sum(i, j):
    """
    This function evaluates the neighbourhood sum depending on the neighbourhood defined using tot/Otot and 9-neigh/5-neigh
    """
    sum = 0
    if nine_neighbourhood:
        if totalistic:
            tuples = [(i, j), (i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1), (i + 1, j + 1), (i + 1, j - 1), (i - 1, j + 1), (i - 1, j - 1)]
        elif outer_totalistic:
            tuples = [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1), (i + 1, j + 1), (i + 1, j - 1), (i - 1, j + 1), (i - 1, j - 1)]
    elif five_neighbourhood:
        if totalistic:
            tuples = [(i, j), (i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]
        elif outer_totalistic:
            tuples = [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]
    
    for t in tuples:
        if t[0] >= 0 and t[0] < grid_size and t[1] >= 0 and t[1] < grid_size:
            sum += grid[t[0], t[1]] > 0
    return sum

def get_pixel(i, j):
    """
    This function returns the new state of the pixel based on the rule
    """
    neighsum = neighbourhood_sum(i, j)
    if totalistic:
        if neighsum in powers:
            return 1
    elif outer_totalistic:
        if 2 * neighsum + grid[i, j] in powers: 
            return 1
    return 0


palette = np.array([[255,   255,    255],   # white
                    [0,     0,      0]      # black
                    ])
grid_size = 15
evolution = 40
states = [0, 1] # 2 states

# Rules of the form [rule code, tot/Otot, 9-niegh/5-neigh, IC]
rules = [
    [451, False, False, 1],
    [452, False, False, 1],
    [453, False, False, 1],
    [454, False, False, 1],
    [457, False, False, 1],
    [459, False, False, 1],
    [461, False, False, 1],
    [462, False, False, 1],
    [465, False, False, 1],
    [467, False, False, 1],
    [468, False, False, 1],
    [469, False, False, 1],
    [470, False, False, 1],
    [475, False, False, 1],
    [478, False, False, 1],
    [481, False, False, 1],
    [483, False, False, 1],
    [485, False, False, 1],
    [489, False, False, 1],
    [491, False, False, 1],
    [493, False, False, 1],
    [497, False, False, 1],
    [1022, False, False, 1],
    [510, False, False, 1],
    [374, False, False, 1],
    [174, False, False, 1],
    [494, False, False, 1],
    [750, False, False, 1],
    [15822, False, True, 1],
    [699054, False, True, 1],
    [191044, False, True, 1],
    [93737, False, True, 1],
    [85507, False, True, 1],

    [451, False, False, 2],
    [452, False, False, 2],
    [453, False, False, 2],
    [454, False, False, 2],
    [457, False, False, 2],
    [459, False, False, 2],
    [461, False, False, 2],
    [462, False, False, 2],
    [465, False, False, 2],
    [467, False, False, 2],
    [468, False, False, 2],
    [469, False, False, 2],
    [470, False, False, 2],
    [475, False, False, 2],
    [478, False, False, 2],
    [481, False, False, 2],
    [483, False, False, 2],
    [485, False, False, 2],
    [489, False, False, 2],
    [491, False, False, 2],
    [493, False, False, 2],
    [497, False, False, 2],
    [750, False, False, 2],
    [15822, False, True, 2],
    [699054, False, True, 2],
    [191044, False, True, 2],
    [93737, False, True, 2],
    [85507, False, True, 2],
    [256746, False, True, 2],
    [736, False, True, 2],
    [291552, False, True, 2],
    [184, False, True, 2],
    [46, False, True, 2],

    [24, True, False, 3],
    [510, True, False, 3],
    [52, True, False, 3],
    [736, False, True, 3],
    [196623, False, True, 3],
    [152822, False, True, 3],
    [143954, False, True, 3],
    [3276, False, True, 3],
    [224, False, True, 3],
    [4, True, False, 3],
    [6, True, False, 3],
    [30, True, False, 3],
    [38, True, False, 3],
    [52, True, False, 3],
    [54, True, False, 3],
    [56, True, False, 3],
    [58, True, False, 3]
]

# This code creates and saves all stimuli and evolution gifs into 2 folders - stimuli and gifs in the generator folder.

for p in rules:
    code = p[0]
    totalistic = p[1]
    outer_totalistic = not(totalistic)
    nine_neighbourhood = p[2]
    five_neighbourhood = not(nine_neighbourhood)
    IC = p[3]

    if IC == 1:
        initial = np.zeros((grid_size, grid_size), int)
        initial[int(grid_size/2), int(grid_size/2)] = 1
    elif IC == 2:
        initial = np.zeros((grid_size, grid_size), int)
        initial[int(grid_size/2) - 1, int(grid_size/2)] = 1
        initial[int(grid_size/2) + 1, int(grid_size/2)] = 1
        initial[int(grid_size/2), int(grid_size/2) + 1] = 1
        initial[int(grid_size/2) + 1, int(grid_size/2) + 1] = 1
        initial[int(grid_size/2) + 1, int(grid_size/2) - 1] = 1
    if IC == 3:
        initial = np.random.randint(2, size=(grid_size, grid_size))

    if nine_neighbourhood:
        s1 = "9neigh"
    elif five_neighbourhood:
        s1 = "5neigh"
    if totalistic:
        s2 = "tot"
    elif outer_totalistic:
        s2 = "outertot"

    # Derive rule from code
    binary = "{0:b}".format(code)
    l = len(binary)
    powers = [l -  1 - m.start() for m in re.finditer('1', binary)]
    
    grid = initial
    images = []

    print(grid)
    print(os.getcwd())

    # Evolve the grid and save every 8th pattern
    for time in range(evolution):
        previous = grid.copy()
        ret = matrix_to_pattern(grid)
        if (time + 1) % int(evolution/8) == 0:
            print(os.path)
            if not os.path.exists('./stimuli'):
                print("hi")
                os.makedirs('./stimuli')
            plt.imsave("./stimuli/" + s1 + "_" + s2 + "_code" + str(code) + "_IC" + str(IC) + "_iter" +  format(time + 1, '02d') + ".png", ret, cmap='gray', vmin=0, vmax=255)
        gridcopy = np.copy(grid)
        for i in range(grid_size):
            for j in range(grid_size):
                gridcopy[i, j] = get_pixel(i, j)
        grid = gridcopy.copy()
        if (previous == grid).all():
            if not os.path.exists('./stimuli'):
                os.makedirs('./stimuli')
            plt.imsave("./stimuli/" + s1 + "_" + s2 + "_code" + str(code) + "_IC" + str(IC)  + "_iter" + format(time + 1, '02d') + ".png", ret, cmap='gray', vmin=0, vmax=255)
            break
    
    # Save the whole evolution as a gif
    if not os.path.exists('./gifs'):
        os.makedirs('./gifs')
    imageio.mimsave("./gifs/" + s1 + "_" + s2 + "_code" + str(code) + "_IC" + str(IC) + ".gif", images, duration = 0.2)