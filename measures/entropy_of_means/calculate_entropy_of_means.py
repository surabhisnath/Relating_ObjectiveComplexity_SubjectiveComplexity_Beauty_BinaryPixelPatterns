"""
Python implementation of the matrix information measurement examples from the
StackExchange answer written by WilliamAHuber for
"Measuring entropy/ information/ patterns of a 2d binary matrix"
http://stats.stackexchange.com/a/17556/43909

Copyright 2014 Cosmo Harrigan
This program is free software, distributed under the terms of the GNU LGPL v3.0
"""

author = 'Cosmo Harrigan'

from matplotlib import pyplot
from neighborhood_functions import avg_components
from moving_window_filter import moving_window_filter
from calculate_profile import profile
import matplotlib.pyplot as plt
import matplotlib
import numpy as np

# Function to apply
F = avg_components

# Define the matrices as input_matrices
from data import *

def calculate_entropy_of_means(flatgrid, grid_size):
    # Produce the filtered matrices at varying scales and the associated entropy "profiles"
    grid = flatgrid.reshape((grid_size, grid_size))
    matrices = []
    for n in range(1, min(grid.shape)):
        output_matrix = moving_window_filter(matrix=grid,
                                             f=F,
                                             neighborhood_size=n)
        matrices.append(output_matrix)

    prof = profile(matrices)
    print(prof)
    return np.mean(prof)

if __name__ == "__main__":
    grid_size = 15
    
    # calculate density for some example patterns:
    #    1) Full black pattern
    #    2) White grid with one central black CellType
    #    3) Checkarboard
    #    4) Random pattern

    all_black = np.ones(grid_size * grid_size, dtype=int)
    print(calculate_entropy_of_means(all_black, grid_size))

    grid_with_one_centre = np.zeros(grid_size * grid_size, dtype=int)
    grid_with_one_centre[(grid_size * grid_size) //2] = 1
    print(calculate_entropy_of_means(grid_with_one_centre, grid_size))

    checkarboard = np.zeros(grid_size * grid_size, dtype=int)
    checkarboard[1::2] = 1
    print(calculate_entropy_of_means(checkarboard, grid_size))

    random = np.random.choice([0, 1], size=(grid_size, grid_size))
    print(calculate_entropy_of_means(random, grid_size))