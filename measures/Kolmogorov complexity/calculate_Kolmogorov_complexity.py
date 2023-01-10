"""
Author: Surabhi S Nath
"""
import numpy as np
import sys
from pybdm import BDM

def calculate_Kolmogorov_complexity(pattern):
    """
    This function calculates the approximate Kolmogorov Complexity of a pattern using Block Decomposition Method.
    """
    grid = pattern.copy()
    
    # Initialize BDM object
    bdm = BDM(ndim=2)
    
    # Compute BDM
    K = bdm.bdm(grid)
    
    return K