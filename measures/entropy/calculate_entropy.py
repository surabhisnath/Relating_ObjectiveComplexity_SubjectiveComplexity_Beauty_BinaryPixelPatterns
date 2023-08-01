author = 'Surabhi S Nath'
import numpy as np
import math

"""
This module implements the entropy measure. Refer to Section 2.2 (2i) and AII for details.
"""

def calculate_entropy(gridorflatgrid, grid_size):
    """
    This function calculates the singlescale entropy of the pattern

    Args:
        gridorflatgrid (array): pattern
        grid_size (integer): grid size of the pattern

    Returns:
        float: entropy of the pattern
    """

    flatgrid = gridorflatgrid.reshape((1, grid_size * grid_size))

    uniq, cnts = np.unique(flatgrid, return_counts=True)
    assert len(uniq) == len(cnts) < 3
    if len(uniq) == 1:
        return 0
    
    p1 = cnts[1]/(cnts[0] + cnts[1])        # fraction of black pixels
    p2 = cnts[0]/(cnts[0] + cnts[1])        # fraction of white pixels

    entropy = -1 * (p1 * math.log(p1,2) + p2 * math.log(p2,2))      # entropy equation (Eq. 1)
    return entropy
    
if __name__ == '__main__':
    grid_size = 15

    # calculate density for some example patterns:
    #    1) Full black pattern
    #    2) White grid with one central black CellType
    #    3) checkerboard
    #    4) Random pattern

    all_black = np.ones(grid_size * grid_size, dtype=int)
    assert calculate_entropy(all_black, grid_size) == 0
    print(calculate_entropy(all_black, grid_size))

    grid_with_one_centre = np.zeros(grid_size * grid_size, dtype=int)
    grid_with_one_centre[(grid_size * grid_size) //2] = 1
    assert calculate_entropy(grid_with_one_centre, grid_size) == 0.041125624368577904
    print(calculate_entropy(grid_with_one_centre, grid_size))

    checkerboard = np.zeros(grid_size * grid_size, dtype=int)
    checkerboard[1::2] = 1
    assert calculate_entropy(checkerboard, grid_size) == 0.99998575111318
    print(calculate_entropy(checkerboard, grid_size))

    random = np.random.choice([0, 1], size=(grid_size, grid_size))
    print(calculate_entropy(random, grid_size))