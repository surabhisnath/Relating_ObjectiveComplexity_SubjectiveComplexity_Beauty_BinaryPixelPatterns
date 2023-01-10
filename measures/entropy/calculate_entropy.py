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

# Function to apply
F = avg_components

# Define the matrices as input_matrices
from data import *

def calculate_entropy(flatgrid, grid_size):
    # Produce the filtered matrices at varying scales and the associated entropy "profiles"
    flatgrid = flatgrid.reshape((grid_size, grid_size))
    matrices = []
    for n in range(1, min(flatgrid.shape)):
        output_matrix = moving_window_filter(matrix=flatgrid,
                                             f=F,
                                             neighborhood_size=n)
        matrices.append(output_matrix)

    return np.mean(profile(matrices))