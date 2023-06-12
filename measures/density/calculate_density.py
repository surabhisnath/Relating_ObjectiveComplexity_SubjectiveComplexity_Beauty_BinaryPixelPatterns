author = 'Surabhi S Nath'
import numpy as np

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
    #    3) Checkarboard
    #    4) Random pattern
    
    all_black = np.ones(grid_size * grid_size)
    print(calculate_density(all_black, grid_size))

    grid_with_one_centre = np.zeros(grid_size * grid_size)
    grid_with_one_centre[(grid_size * grid_size) //2] = 1
    print(calculate_density(grid_with_one_centre, grid_size))

    checkarboard = np.zeros(grid_size * grid_size)
    checkarboard[1::2] = 1
    print(calculate_density(checkarboard, grid_size))

    random = np.random.choice([0, 1], size=(grid_size, grid_size))
    print(calculate_density(random, grid_size))