"""
Author: Surabhi S Nath
"""
import numpy as np

def get_coords(grid):
	blackpos = np.where(grid == 1)
	num_black = np.sum(grid)
	coords = list(zip(blackpos[0], blackpos[1]))
	return coords, num_black

def horizontal_symm(grid, grid_size, coords, num_black):
	"""
	This function calculates horizontal asymmetry by by counting the horizintally mismatched black pixels.
	"""
	mismatch = 0
	for coord in coords:
		if (coord[1] >= grid_size/2 and grid[coord[0], grid_size - 1 - coord[1]] != 1) or (coord[1] < grid_size/2 and grid[coord[0], grid_size - 1 - coord[1]] != 1):
			mismatch = mismatch + 1
	percent = mismatch/num_black * 100
	return percent

def vertical_symm(grid, grid_size, coords, num_black):
	"""
	This function calculates vertical asymmetry by by counting the vertically mismatched black pixels.
	"""
	mismatch = 0
	for coord in coords:
		if (coord[0] >= grid_size/2 and grid[grid_size - 1 - coord[0], coord[1]] != 1) or (coord[0] < grid_size/2 and grid[grid_size - 1 - coord[0], coord[1]] != 1):
			mismatch = mismatch + 1
	percent = mismatch/num_black * 100
	return percent

def calculate_symmetry(grid, grid_size):
	"""
	This function calculates the horizontal and vertical asymmetry of a pattern.
	"""
	coords, num_black = get_coords(grid)
	p1 = horizontal_symm(grid, grid_size, coords, num_black)
	p2 = vertical_symm(grid, grid_size, coords, num_black)
	return (p1, p2)