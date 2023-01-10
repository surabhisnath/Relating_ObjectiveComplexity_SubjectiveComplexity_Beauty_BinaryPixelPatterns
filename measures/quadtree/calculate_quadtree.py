"""
Author: Surabhi S Nath
"""
import numpy as np

def all_same(grid):
    if len(np.unique(grid)) == 1:
        return True
    return False


def find_quadtree(grid, grid_size_rows, grid_size_cols):
    """
    This function recursively calls itself on split subgraphs.
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

        # Every splitting counts as one towards the quadtree measure
        return 1 + find_quadtree(quad1, len(np.arange(0,grid_size_rows//2)), len(np.arange(0,grid_size_cols//2))) \
                + find_quadtree(quad2, len(np.arange(0,grid_size_rows//2)), len(np.arange(grid_size_cols//2,grid_size_cols))) \
                + find_quadtree(quad3, len(np.arange(grid_size_rows//2,grid_size_rows)), len(np.arange(0,grid_size_cols//2))) \
                + find_quadtree(quad4, len(np.arange(grid_size_rows//2,grid_size_rows)), len(np.arange(grid_size_cols//2,grid_size_cols)))


def calculate_quadtree(grid, grid_size):
    """
    This function calculate the quadtree measure for a pattern.
    """
    return find_quadtree(grid, grid_size, grid_size)
