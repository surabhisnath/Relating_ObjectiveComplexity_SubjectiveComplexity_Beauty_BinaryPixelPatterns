# Relating Objective Complexity Subjective Complexity and Beauty in Binary Pixel Patterns

This repository contains the data and scripts for the paper "Relating Objective Complexity, Subjective Complexity and Beauty" submitted to PACA.

## Abstract

The complexity of images critically influences our assessment of their beauty. However, studies relating assessments of complexity and beauty to potential objective measures are hampered by the use of hand-crafted stimuli which are hard to reproduce and manipulate. To tackle this, we developed a systematic method for generating 2D black-and-white patterns using cellular automata, and collected ratings of complexity and beauty from 80 participants. We developed various computational measures of pattern quantification such as density, entropies, local spatial complexity, Kolmogorov complexity, and asymmetries. We also introduced an “intricacy” measure quantifying the number of components in a pattern using a graph-based approach. We related these objective measures with participant judgements of complexity and beauty to find that a weighted combination of local spatial complexity and intricacy was an effective predictor (R2test = 0.47) of complexity. This implies that people’s complexity ratings depend on the number of distinct elements in the pattern along with the elements’ local spatial distribution. Therefore, global and local image features are integrated to determine complexity judgements. Furthermore, we found a positive linear influence of complexity ratings on beauty, with a negative linear influence of disorder (asymmetry and entropy of means), and a negative interaction between the two (R2test = 0.64). This implies that there is beauty in complexity as long as there is sufficient order. Lastly, a moderated mediation analysis showed that complexity mediates the influence of objective complexity on beauty, implying that complexity supplies useful information over and above objective complexity.

## Setup

The repository contains the following folders:
- **data**: contains the processed .csv data files from 80 participants. Each file has 6 columns indicating the pattern number, complexity reponse (0-100), beauty response (0-100), reaction time (ms), is_repeated flag which indicates if the pattern is repeated (6 patterns were repeated for each participant) and trial number. The dictionary mapping pattern names to pattern numbers is provided in `scrips/utils/pattern_to_num.pk`. The data folder also has info.csv which stores participant details such as dmeographics, open-ended reponses, number of attention checks presented/failed and total time taken.
- **figures**: contains all the figures in the main paper as PDFs.
- **generator**: 
