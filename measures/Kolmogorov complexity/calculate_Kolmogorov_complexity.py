author = 'Surabhi S Nath'
import numpy as np
from pybdm import BDM

"""
This module implements the Kolmogorov complexity measure. Refer to Section 2.2 (4) for details.
"""

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
    #    3) checkerboard
    #    4) Random pattern

    all_black = np.ones(grid_size * grid_size, dtype=int)
    assert calculate_Kolmogorov_complexity(all_black, grid_size) == 25.176631293734488
    print(calculate_Kolmogorov_complexity(all_black, grid_size))

    grid_with_one_centre = np.zeros(grid_size * grid_size, dtype=int)
    grid_with_one_centre[(grid_size * grid_size) //2] = 1
    assert calculate_Kolmogorov_complexity(grid_with_one_centre, grid_size) == 48.354642249885316
    print(calculate_Kolmogorov_complexity(grid_with_one_centre, grid_size))

    checkerboard = np.zeros(grid_size * grid_size, dtype=int)
    checkerboard[1::2] = 1
    assert calculate_Kolmogorov_complexity(checkerboard, grid_size) == 33.43569202047461
    print(calculate_Kolmogorov_complexity(checkerboard, grid_size))

    random = np.random.choice([0, 1], size=(grid_size, grid_size))
    print(calculate_Kolmogorov_complexity(random, grid_size))