author = 'Surabhi S Nath'
import numpy as np
import math
import sys
sys.path.insert(1, '../entropy/')
from calculate_entropy import *

def calculate_mean_entropy(gridorflatgrid, grid_size):
    """
    This function calculates the mean entropy of a pattern across all scales

    Args:
        gridorflatgrid (array): pattern
        grid_size (integer): grid size of the pattern

    Returns:
        float: mean entropy of the pattern
    """
    grid = gridorflatgrid.reshape((grid_size, grid_size))

    mean_entropy = 0
    for sliding_window_size in range(1, min(grid.shape) + 1):       # iterate over all window sizes
        sliding_window_entropy_sum = 0
        num_windows = (15 - sliding_window_size + 1) ** 2   # number of windows at a particular scale is (15 - w + 1)^2
        
        for i in range(15 - sliding_window_size + 1):
            for j in range(15 - sliding_window_size + 1):
                window = grid[i:i+sliding_window_size, j:j+sliding_window_size]     # get window
                sliding_window_entropy_sum += calculate_entropy(window, sliding_window_size)    # calculate entropy of the window
        mean_entropy += sliding_window_entropy_sum/num_windows
    return mean_entropy/grid_size

if __name__ == '__main__':
    grid_size = 15

    # calculate density for some example patterns:
    #    1) Full black pattern
    #    2) White grid with one central black CellType
    #    3) Checkarboard
    #    4) Random pattern
    
    all_black = np.ones(grid_size * grid_size, dtype=int)
    print(calculate_mean_entropy(all_black, grid_size))

    grid_with_one_centre = np.zeros(grid_size * grid_size, dtype=int)
    grid_with_one_centre[(grid_size * grid_size) //2] = 1
    print(calculate_mean_entropy(grid_with_one_centre, grid_size))

    checkarboard = np.zeros(grid_size * grid_size, dtype=int)
    checkarboard[1::2] = 1
    print(calculate_mean_entropy(checkarboard, grid_size))

    random = np.random.choice([0, 1], size=(grid_size, grid_size))
    print(calculate_mean_entropy(random, grid_size))