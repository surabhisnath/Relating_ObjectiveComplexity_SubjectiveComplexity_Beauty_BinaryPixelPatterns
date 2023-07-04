author = 'Surabhi S Nath'
import numpy as np

"""
This module implements the quadtree measure. Refer to AII for details.
"""

def all_same(grid):
    """
    Checks if all pixels of the subgrid are the same state

    Args:
        grid (array): split subgrid from recursion

    Returns:
        boolean: True if all pixels are the same state else False
    """
    if len(np.unique(grid)) == 1:
        return True
    return False


def find_quadtree(grid, grid_size_rows, grid_size_cols):
    """
    This function recursively calls itself on split subgrids.
    """
    # Base case when subpattern has only 2 pixels
    if  grid_size_rows * grid_size_cols == 2:
        if all_same(grid) == True:
            return 0
        return 1

    # Base cases when all pixels are the same colour
    if all_same(grid) == True:
        return 0
    
    else:
        # Divide the pattern into 4 sub-patterns
        quad1 = grid[0:grid_size_rows//2, 0:grid_size_cols//2]
        quad2 = grid[0:grid_size_rows//2, grid_size_cols//2:]
        quad3 = grid[grid_size_rows//2:, 0:grid_size_cols//2]
        quad4 = grid[grid_size_rows//2:, grid_size_cols//2:]

        # Every splitting counts as one towards the quadtree measure. Call the function again on the 4 split subgrids
        return 1 + find_quadtree(quad1, grid_size_rows//2, grid_size_cols//2) \
                + find_quadtree(quad2, grid_size_rows//2, grid_size_cols - (grid_size_cols//2)) \
                + find_quadtree(quad3, grid_size_rows - (grid_size_rows//2), grid_size_cols//2) \
                + find_quadtree(quad4, grid_size_rows - (grid_size_rows//2), grid_size_cols - (grid_size_cols//2))


def calculate_quadtree(grid, grid_size):
    """
    This function calculate the quadtree measure for a pattern.

    Returns:
        integer: quadtree for the pattern
    """
    grid = grid.reshape(grid_size, grid_size)
    return find_quadtree(grid, grid_size, grid_size)

if __name__ == "__main__":
    grid_size = 15
    
    # calculate density for some example patterns:
    #    1) Full black pattern
    #    2) White grid with one central black CellType
    #    3) checkerboard
    #    4) Random pattern

    all_black = np.ones(grid_size * grid_size, dtype=int)
    assert calculate_quadtree(all_black, grid_size) == 0 
    print(calculate_quadtree(all_black, grid_size))

    grid_with_one_centre = np.zeros(grid_size * grid_size, dtype=int)
    grid_with_one_centre[(grid_size * grid_size) //2] = 1
    assert calculate_quadtree(grid_with_one_centre, grid_size) == 4 
    print(calculate_quadtree(grid_with_one_centre, grid_size))

    checkerboard = np.zeros(grid_size * grid_size, dtype=int)
    checkerboard[1::2] = 1
    assert calculate_quadtree(checkerboard, grid_size) == 84
    print(calculate_quadtree(checkerboard, grid_size))

    random = np.random.choice([0, 1], size=(grid_size, grid_size))
    print(calculate_quadtree(random, grid_size))