author = 'Surabhi S Nath'
import numpy as np

'''
This module implememts the asymmetry measure. Refer to Section 2.2 (6) and AII for details.
'''

def get_coords(grid):
	"""
	This function returns the indices and count of black pixels in the pattern

	Args:
		grid (array): pattern

	Returns:
		tuple: indices, count of black pixels
	"""
	blackpos = np.where(grid == 1)
	num_black = np.sum(grid)
	coords = list(zip(blackpos[0], blackpos[1]))
	return coords, num_black

def horizontal_asymm(grid, grid_size, coords, num_black):
	"""
	This function calculates horizontal asymmetry by by counting the horizintally mismatched black pixels.

	Returns:
		float: horizontal asymmetry
	"""
	mismatches = 0
	for coord in coords:
		if (coord[1] >= grid_size/2 and grid[coord[0], grid_size - 1 - coord[1]] != 1) or (coord[1] < grid_size/2 and grid[coord[0], grid_size - 1 - coord[1]] != 1):		# for horizontal asymmetry, the black pixels on the left half of the pattern are matched with their corresponding right half to find number of mismatched pixels
			mismatches = mismatches + 1
	percent = mismatches/num_black * 100
	return percent

def vertical_asymm(grid, grid_size, coords, num_black):
	"""
	This function calculates vertical asymmetry by by counting the vertically mismatched black pixels.

	Returns:
		float: vertical asymmetry
	"""
	mismatches = 0
	for coord in coords:
		if (coord[0] >= grid_size/2 and grid[grid_size - 1 - coord[0], coord[1]] != 1) or (coord[0] < grid_size/2 and grid[grid_size - 1 - coord[0], coord[1]] != 1):		# for vertical asymmetry, the black pixels on the top half of the pattern are matched with their corresponding botton half to find number of mismatched pixels
			mismatches = mismatches + 1
	percent = mismatches/num_black * 100
	return percent

def calculate_asymmetry(grid, grid_size):
	"""
	This function calculates the horizontal and vertical asymmetry of a pattern.

	Args:
		grid (array): pattern whose asymmetry is being calculated
		grid_size (integer): grid size of the pattern

	Returns:
		tuple: horizintal and vertical asymmetry
	"""
	grid = grid.reshape((grid_size, grid_size))
	coords, num_black = get_coords(grid)
	p1 = horizontal_asymm(grid, grid_size, coords, num_black)
	p2 = vertical_asymm(grid, grid_size, coords, num_black)
	return (p1, p2)

if __name__ == "__main__":
    grid_size = 15

    # calculate density for some example patterns:
    #    1) Full black pattern
    #    2) White grid with one central black CellType
    #    3) checkerboard
    #    4) Random pattern
   
    all_black = np.ones(grid_size * grid_size)
    assert calculate_asymmetry(all_black, grid_size) == (0.0, 0.0)
    print(calculate_asymmetry(all_black, grid_size))

    grid_with_one_centre = np.zeros(grid_size * grid_size)
    grid_with_one_centre[(grid_size * grid_size) //2] = 1
    assert calculate_asymmetry(grid_with_one_centre, grid_size) == (0.0, 0.0)
    print(calculate_asymmetry(grid_with_one_centre, grid_size))

    checkerboard = np.zeros(grid_size * grid_size)
    checkerboard[1::2] = 1
    assert calculate_asymmetry(checkerboard, grid_size) == (0.0, 0.0)
    print(calculate_asymmetry(checkerboard, grid_size))

    random = np.random.choice([0, 1], size=(grid_size, grid_size))
    print(calculate_asymmetry(random, grid_size))