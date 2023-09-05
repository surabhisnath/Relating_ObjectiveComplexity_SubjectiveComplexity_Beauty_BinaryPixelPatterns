# Author: Surabhi S Nath

'''
This script prepares stat_analysis.csv which is used in 
DescriptiveAnalysis.ipynb and MixedEffectsModelling.R
'''

# Imports
import numpy as np
import pandas as pd
import os
import pickle
import matplotlib.pyplot as plt
from PIL import Image

path = "../../data/"	# datapath contained processed data files
csv = open("stat_analysis.csv", 'w')	# create stat_analysis.csv

# Write columns
csv.write('subject,set,pattern,complexity_rating,beauty_rating,rtime,trial,previous_complexity_rating,previous_beauty_rating,density,entropy,mean_entropy,H_2,H_mean,LSC,KC,intricacy_4,intricacy_8,local_asymm,Hasymm,Vasymm,quadtree,neighbourhood_size,tot_outertot,IC,num_active_rules\n')

# load pattern_to_num and pattern_stats
pattern_to_num = pickle.load(open("pattern_to_num.pk", 'rb'))
num_to_pattern = {v: k for k, v in pattern_to_num.items()}
pattern_stats = pd.read_csv("pattern_stats.csv")
pattern_stats.set_index('patternnum', inplace=True)
stats_to_num_to_statval = pattern_stats.to_dict()	# invert pattern_stats to get mapping from measure to patternnum

measures = \
	[
		"density", "entropy", "mean_entropy", "H_2", "H_mean", "LSC", "KC", 
		"intricacy_4", "intricacy_8", "local_asymm", "Hasymm", "Vasymm", "quadtree", 
		"neighbourhood_size", "tot_outertot", "IC", "num_active_rules"
	]

relevant_files = [f for f in os.listdir(path) if f.startswith("data")]
sorted_files = np.sort(relevant_files)

for i, file in enumerate(sorted_files):

	oldcomplexity = "50"	# oldcomplexity and oldbeauty are the complexity and beauty in the previous trial - initially set to mean value = 50
	oldbeauty = "50"

	name = file.split('_')
	sub = open(path + file, 'r')
	
	for line in sub.readlines()[1:]:
		arr = line.split(',')
		patternnum = int(arr[0])

		# get all measures using inverted pattern_stats
		values = [stats_to_num_to_statval[measure][patternnum] for measure in measures]
		density, entropy, mean_entropy, H_2, H_mean, LSC, KC, intricacy_4, intricacy_8, local_asymm, Hasymm, Vasymm, quadtree, neigh, tot, IC, num_active_rules = values

		# write row to stat_analysis.csv
		towrite = \
		[
			name[1], name[2][3], patternnum, arr[1], arr[2], arr[3], arr[5][:-1], 
			oldcomplexity, oldbeauty, density, entropy, mean_entropy, H_2, H_mean,
			LSC, KC, intricacy_4, intricacy_8, local_asymm, Hasymm, Vasymm, quadtree,
			neigh, tot, IC, num_active_rules
		]
		csv.write(','.join(str(val) for val in towrite) + '\n')
	
		oldcomplexity = arr[1] # update oldcomplexity and oldbeauty
		oldbeauty = arr[2]