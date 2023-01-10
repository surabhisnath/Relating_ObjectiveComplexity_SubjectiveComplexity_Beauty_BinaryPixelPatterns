import numpy as np

def calculate_density(grid, grid_size):
    """
    This function caclulates the density of black pixels in a pattern
    """
    blacks = np.where(grid == 1)
    num_blacks = len(blacks[0])
    density = num_blacks/(grid_size * grid_size)
    return density