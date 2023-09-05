author = 'Surabhi S Nath'
import numpy as np
from collections import Counter
from math import log

'''
This module implememts the H measure from Snodgrass, 1971. Refer to AII for details.
'''

def get_sliding_windows(grid, grid_size, window_size):
    """
	This function returns all sliding windows of size window_size x window_size

	Args:
		grid (array): pattern
        grid_size (int): pattern size = 15
        window_size (int): size of window

	Returns:
		list: list of sliding windows each as a 0/1 string of length window_size x window_size
	"""
    assert window_size <= len(grid)
    windows = []
    for i in range(grid_size - window_size + 1):
        for j in range(grid_size - window_size + 1):
            window = [grid[i + x][j + y] for x in range(window_size) for y in range(window_size)]
            windows.append(''.join(map(str, window)))
    return windows

def get_sliding_window_probabilities(windows):
    """
	This function calculates probabilities of each (nonzero) configuration of sliding windows

	Args:
		windows (list): list of sliding windows

	Returns:
		list: list of probabilities of each (nonzero) configuration of sliding windows
	"""
    frequencies_dict =  Counter(windows)
    counts = frequencies_dict.values()
    total_count = sum(counts)
    assert total_count == len(windows)
    probabilities = [float(x) / total_count for x in counts]
    return probabilities

def entropy(probs):
    """
	This function calculates the entropy of a list as -[(over p in list) sum plogp]

	Args:
		probs (list): list of probabilities to calculate entropy

	Returns:
		float: entropy value
	"""
    entropy = 0
    for p in probs:
        entropy += p * log(p, 2)

    if entropy == 0:
        return entropy
    return -1 * entropy

def calculate_H(flatgrid, grid_size):
    """
	This function calculates the H measure as defined in Snodgrass, 1971 -
    Sliding windows are extracted at varying scales and the associated H values are evaluated

	Args:
		flatgrid (array): flattened pattern
        grid_size (int): pattern size = 15

	Returns:
		list: H at 3 scales - 
	"""

    grid = flatgrid.reshape((grid_size, grid_size))

    sliding_window_sizes = np.arange(1, grid_size + 1)
    Hs = []
    for sws in sliding_window_sizes:
        windows = get_sliding_windows(grid, grid_size, sws)
        probabilities = get_sliding_window_probabilities(windows)
        Hs.append(entropy(probabilities))

    return Hs[1], np.mean(Hs)

if __name__ == "__main__":
    grid_size = 15
    
    # calculate density for some example patterns:
    #    1) Full black pattern
    #    2) White grid with one central black CellType
    #    3) checkerboard
    #    4) Random pattern

    all_black = np.ones(grid_size * grid_size, dtype=int)
    assert calculate_H(all_black, grid_size) == (0.0, 0.0)
    print(calculate_H(all_black, grid_size))

    grid_with_one_centre = np.zeros(grid_size * grid_size, dtype=int)
    grid_with_one_centre[(grid_size * grid_size) //2] = 1
    assert calculate_H(grid_with_one_centre, grid_size) == (0.18454249646999404, 2.74023274483865)
    print(calculate_H(grid_with_one_centre, grid_size))

    checkerboard = np.zeros(grid_size * grid_size, dtype=int)
    checkerboard[1::2] = 1
    assert calculate_H(checkerboard, grid_size) == (1.0, 0.932628161077425)
    print(calculate_H(checkerboard, grid_size))

    random = np.random.choice([0, 1], size=(grid_size, grid_size))
    print(calculate_H(random, grid_size))