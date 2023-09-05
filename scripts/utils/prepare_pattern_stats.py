# Author: Surabhi S Nath

'''
This script prepares pattern_stats.csv which is used to make stat_analysis.csv which is used in 
DescriptiveAnalysis.ipynb and MixedEffectsModelling.R
'''

# Imports
import numpy as np
import pandas as pd
import os
import pickle
import matplotlib.pyplot as plt
from PIL import Image
import sys
sys.path.insert(1, '../../measures/density/')
from calculate_density import *
sys.path.insert(1, '../../measures/asymmetry/')
from calculate_asymmetry import *
sys.path.insert(1, '../../measures/intricacy/')
from calculate_intricacy import *
sys.path.insert(1, '../../measures/quadtree/')
from calculate_quadtree import *
sys.path.insert(1, '../../measures/local spatial complexity/')
from calculate_local_spatial_complexity import *
sys.path.insert(1, '../../measures/Kolmogorov complexity/')
from calculate_Kolmogorov_complexity import *
sys.path.insert(1, '../../measures/entropy/')
from calculate_entropy import *
sys.path.insert(1, '../../measures/mean_entropy/')
from calculate_mean_entropy import *
sys.path.insert(1, '../../measures/H/')
from calculate_H import *

# Pattern paths
pattern_path = "../../patterns/15by15/experiment/set{}/"
paths = [pattern_path.format(i) for i in range(1, 5)]

csv = open("pattern_stats.csv", 'w')	# create pattern_stats.csv

# Write columns
csv.write('patternname,patternnum,set,density,entropy,mean_entropy,H_2,H_mean,LSC,KC,intricacy_4,intricacy_8,local_asymm,Hasymm,Vasymm,quadtree,neighbourhood_size,tot_outertot,IC,num_active_rules\n')

# load pattern_to_num
pattern_to_num = pickle.load(open("pattern_to_num.pk", 'rb'))

grid_size = 15

def pattern_to_matrix(filepath):
	'''
	This function reads an image and returns it as a grid_size x grid_size matrix
	'''
	img = Image.open(filepath).convert('L')
	big = np.array(img)
	small = np.array([big[i, 0::40] for i in range(0,600,40)])
	small[small == 0] = 1
	small[small == 255] = 0
	return small

for i, path in enumerate(paths):
	print(path)
	relevant_files = [f for f in os.listdir(path) if f.endswith(".png")]
	sorted_files = np.sort(relevant_files)

	for patternname in sorted_files:
		condition = str(i+1)
		pattern_full = path[6:] + patternname
		patternnum = str(pattern_to_num[pattern_full])
		segments = patternname.split('_')

		neigh = segments[3][0]
		tot = segments[4]
		rule = "{0:b}".format(int(segments[5][4:]))
		num_active_rules = str(rule.count('1'))
		IC = segments[6][-1]

		mat = pattern_to_matrix(os.path.join(path, patternname))
		metrics = [
			calculate_density(mat, grid_size),
			calculate_entropy(mat, grid_size),
			calculate_mean_entropy(mat, grid_size),
			*calculate_H(mat, grid_size),
			calculate_local_spatial_complexity(mat, grid_size)[0], # LSC
			calculate_Kolmogorov_complexity(mat, grid_size),
			*calculate_intricacy(mat, grid_size),
			calculate_local_spatial_complexity(mat, grid_size)[1], # local_asymm
			*calculate_asymmetry(mat, grid_size),
			calculate_quadtree(mat, grid_size)
		]

		csv.write(f"{pattern_full},{patternnum},{condition},{','.join(map(str, metrics))},{neigh},{tot},{IC},{num_active_rules}\n")