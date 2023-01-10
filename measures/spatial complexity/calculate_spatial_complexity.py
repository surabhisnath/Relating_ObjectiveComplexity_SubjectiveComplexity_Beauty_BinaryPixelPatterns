"""
Author: Surabhi S Nath
"""
import numpy as np

def get_tuples(col1, col2, dirn, grid, grid_size):
	count_tuples = 0
	count_neigh = 0
	for i in range(grid_size):
		for j in range(grid_size):
			if dirn == 1: # Up
				if j + 1 < grid_size:
					if grid[i, j + 1] == col2:
						count_neigh += 1
					if grid[i, j] == col1 and grid[i, j + 1] == col2:
						count_tuples += 1
			elif dirn == 2: # Down
				if j - 1 >= 0:
					if grid[i, j - 1] == col2:
						count_neigh += 1
					if grid[i, j] == col1 and grid[i, j - 1] == col2:
						count_tuples += 1
			elif dirn == 3: # Right
				if i + 1 < grid_size:
					if grid[i + 1, j] == col2:
						count_neigh += 1
					if grid[i, j] == col1 and grid[i + 1, j] == col2:
						count_tuples += 1
			elif dirn == 4: # Left
				if i - 1 >= 0:
					if grid[i - 1, j] == col2:
						count_neigh += 1
					if grid[i, j] == col1 and grid[i - 1, j] == col2:
						count_tuples += 1
			elif dirn == 5:
				if i + 1 < grid_size and j + 1 < grid_size:
					if grid[i + 1, j + 1] == col2:
						count_neigh += 1
					if grid[i, j] == col1 and grid[i + 1, j + 1] == col2:
						count_tuples += 1
			elif dirn == 6:
				if i + 1 < grid_size and j - 1 >= 0:
					if grid[i + 1, j - 1] == col2:
						count_neigh += 1
					if grid[i, j] == col1 and grid[i + 1, j - 1] == col2:
						count_tuples += 1
			elif dirn == 7:
				if i - 1 >= 0 and j + 1 < grid_size:
					if grid[i - 1, j + 1] == col2:
						count_neigh += 1
					if grid[i, j] == col1 and grid[i - 1, j + 1] == col2:
						count_tuples += 1
			elif dirn == 8:
				if i - 1 >= 0 and j - 1 >= 0:
					if grid[i - 1, j - 1] == col2:
						count_neigh += 1
					if grid[i, j] == col1 and grid[i - 1, j - 1] == col2:
						count_tuples += 1

	return count_neigh, count_tuples

def get_joint_conditional(col1, col2, dirn, grid, grid_size):
	"""
	This function evaluates and returns the condinational and joint probabilities given the 2 colours as stated in Javid, 2019.
	"""
	den1 = grid_size * (grid_size - 1)
	den2, num = get_tuples(col1, col2, dirn, grid, grid_size)
	if den2 == 0:
		return 0, 0
	conditional = num/den2
	joint = num/den1
	return conditional, joint

def calculate_spatial_complexity(pattern, grid_size):
	"""
	This function provides an implementation for the calculation of Spatial Complexity and Local Asymmetry as described in Javid, 2019.
	"""
	grid = pattern.copy()
	states = np.array([0, 1])	# 2 states as binary grid
	dirns = [1, 2, 3, 4, 5, 6, 7, 8]	# set of 8 directions
	
	# Calculate spatial complexity
	Gs = []
	muG = 0
	for dirn in dirns:
		G = 0
		for state1 in states:
			for state2 in states:
				conditional, joint = get_joint_conditional(state1, state2, dirn, grid, grid_size)
				if conditional != 0:
					G += -1 * joint * np.log2(conditional)
		muG += G
		Gs.append(G)

	spatial_complexity = muG/len(dirns)

	# Calculate local asymmetry
	horizontal_asymm = max(Gs[0], Gs[1]) - min(Gs[0], Gs[1])
	vertical_asymm = max(Gs[2], Gs[3]) - min(Gs[2], Gs[3])
	secdiag_asymm = max(Gs[5], Gs[6]) - min(Gs[5], Gs[6])
	primdiag_asymm = max(Gs[4], Gs[7]) - min(Gs[4], Gs[7])

	local_asymm = (horizontal_asymm + vertical_asymm + secdiag_asymm + primdiag_asymm)/4

	return spatial_complexity, local_asymm


# Ref:
# Javaheri Javid, M. A. (2019). Aesthetic Automata: Synthesis and Simulation of Aesthetic Behaviour in Cellular Automata (Doctoral dissertation, Goldsmiths, University of London).