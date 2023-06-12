author = 'Surabhi S Nath'
import numpy as np
import math

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
    
    p1 = cnts[0]/(cnts[0] + cnts[1])        # fraction of white pixels
    p2 = cnts[1]/(cnts[0] + cnts[1])        # fraction of black pixels

    entropy = -1 * (p1 * math.log(p1,2) + p2 * math.log(p2,2))      # entropy equation (Eq. 1)
    return entropy
    
if __name__ == '__main__':
    grid_size = 15

    # calculate density for some example patterns:
    #    1) Full black pattern
    #    2) White grid with one central black CellType
    #    3) Checkarboard
    #    4) Random pattern

    all_black = np.ones(grid_size * grid_size, dtype=int)
    print(calculate_entropy(all_black, grid_size))

    grid_with_one_centre = np.zeros(grid_size * grid_size, dtype=int)
    grid_with_one_centre[(grid_size * grid_size) //2] = 1
    print(calculate_entropy(grid_with_one_centre, grid_size))

    checkarboard = np.zeros(grid_size * grid_size, dtype=int)
    checkarboard[1::2] = 1
    print(calculate_entropy(checkarboard, grid_size))

    random = np.random.choice([0, 1], size=(grid_size, grid_size))
    print(calculate_entropy(random, grid_size))