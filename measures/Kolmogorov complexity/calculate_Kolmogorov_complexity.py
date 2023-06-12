author = 'Surabhi S Nath'
import numpy as np
import sys
from pybdm import BDM

def calculate_Kolmogorov_complexity(pattern, grid_size):
    """
    This function calculates the approximate Kolmogorov Complexity of a pattern using Block Decomposition Method.
    
    Returns:
        integer: The approximate of Kolmogorov complexity for the pattern
    """
    pattern = pattern.reshape(grid_size, grid_size)
    grid = pattern.copy()

    bdm = BDM(ndim = 2)       # Initialize BDM object
    K = bdm.bdm(grid)       # Compute BDM
    
    return K

if __name__ == "__main__":
    grid_size = 15
    
    # calculate density for some example patterns:
    #    1) Full black pattern
    #    2) White grid with one central black CellType
    #    3) Checkarboard
    #    4) Random pattern

    all_black = np.ones(grid_size * grid_size, dtype=int)
    print(calculate_Kolmogorov_complexity(all_black, grid_size))

    grid_with_one_centre = np.zeros(grid_size * grid_size, dtype=int)
    grid_with_one_centre[(grid_size * grid_size) //2] = 1
    print(calculate_Kolmogorov_complexity(grid_with_one_centre, grid_size))

    checkarboard = np.zeros(grid_size * grid_size, dtype=int)
    checkarboard[1::2] = 1
    print(calculate_Kolmogorov_complexity(checkarboard, grid_size))

    random = np.random.choice([0, 1], size=(grid_size, grid_size))
    print(calculate_Kolmogorov_complexity(random, grid_size))