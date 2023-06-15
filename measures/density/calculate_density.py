author = 'Surabhi S Nath'
import numpy as np

"""
This module implements the density measure. Refer to Section 2.2 (1) and AII for details.
"""

def calculate_density(grid, grid_size):
    """
    This function caclulates the density of black pixels in a pattern
    
    Returns:
        float: Density of the pattern
    """
    blacks = np.where(grid == 1)
    num_blacks = len(blacks[0])
    density = num_blacks/(grid_size * grid_size)
    return density

if __name__ == "__main__":
    grid_size = 15
    
    # calculate density for some example patterns:
    #    1) Full black pattern
    #    2) White grid with one central black CellType
    #    3) checkerboard
    #    4) Random pattern
    
    all_black = np.ones(grid_size * grid_size)
    assert calculate_density(all_black, grid_size) == 1.0
    print(calculate_density(all_black, grid_size))

    grid_with_one_centre = np.zeros(grid_size * grid_size)
    grid_with_one_centre[(grid_size * grid_size) //2] = 1
    assert calculate_density(grid_with_one_centre, grid_size) == 0.0044444444444444444
    print(calculate_density(grid_with_one_centre, grid_size))

    checkerboard = np.zeros(grid_size * grid_size)
    checkerboard[1::2] = 1
    assert calculate_density(checkerboard, grid_size) == 0.49777777777777776
    print(calculate_density(checkerboard, grid_size))

    random = np.random.choice([0, 1], size=(grid_size, grid_size))
    print(calculate_density(random, grid_size))