"""
Author: Surabhi S Nath
"""

import numpy as np
import sys

#Source: GeeksforGeeks: https://www.geeksforgeeks.org/program-to-count-number-of-connected-components-in-an-undirected-graph/

class Graph:
    def __init__(self, V):
 
        # No. of vertices
        self.V = V
 
        # Pointer to an array containing adjacency lists
        self.adj = [[] for i in range(self.V)]
 
    # Function to return the number of connected components in an undirected graph
    def NumberOfconnectedComponents(self):
         
        # Mark all the vertices as not visited
        visited = [False for i in range(self.V)]
        
        # To store the number of connected components
        count = 0
         
        for v in range(self.V):
            if (visited[v] == False):
                self.DFSUtil(v, visited)
                count += 1
                 
        return count
         
    def DFSUtil(self, v, visited):
 
        # Mark the current node as visited
        visited[v] = True
 
        # Recur for all the vertices adjacent to this vertex
        for i in self.adj[v]:
            if (not visited[i]):
                self.DFSUtil(i, visited)
                 
    # Add an undirected edge
    def addEdge(self, v, w):
        self.adj[v].append(w)
        self.adj[w].append(v)

def get_neigh(grid, rnum, cnum, dirn, grid_size): # Origin: top left of matrix
    if dirn == 0: # Up
        if rnum > 0:
            return grid[rnum-1, cnum], rnum-1, cnum
    if dirn == 1: # Right
        if cnum < grid_size - 1:
            return grid[rnum, cnum+1], rnum, cnum+1
    if dirn == 2: # Down
        if rnum < grid_size - 1:
            return grid[rnum+1, cnum], rnum+1, cnum
    if dirn == 3: # Left
        if cnum > 0:
            return grid[rnum, cnum-1], rnum, cnum-1
    if dirn == 4: # Up, Right
        if rnum > 0 and cnum < grid_size - 1:
            return grid[rnum-1, cnum+1], rnum-1, cnum+1
    if dirn == 5: # Right, Down
        if rnum < grid_size - 1 and cnum < grid_size - 1:
            return grid[rnum+1, cnum+1], rnum+1, cnum+1
    if dirn == 6: # Down Left
        if rnum < grid_size - 1 and cnum > 0:
            return grid[rnum+1, cnum-1], rnum+1, cnum-1
    if dirn == 7: # Left, Up
        if rnum > 0 and cnum > 0:
            return grid[rnum-1, cnum-1], rnum-1, cnum-1
    return -1, -1, -1

def calculate_intricacy(grid, grid_size):
    """
    This function calculates the 4-neighbourhood and 8-neighbourhood intricacy of a pattern.
    """
    g1 = Graph(grid_size * grid_size) # 4-neighbourhood
    g2 = Graph(grid_size * grid_size) # 8-neighbourhood

    for rnum in range(grid_size):
        for cnum in range(grid_size):
            for dirn in range(4): # 4-neighbourhood intricacy
                neigh = get_neigh(grid, rnum, cnum, dirn, grid_size)
                if neigh[0] == grid[rnum, cnum]:
                    g1.addEdge(neigh[1] * grid_size + neigh[2], rnum * grid_size + cnum)
            
            for dirn in range(8): # 8-neighbourhood intricacy
                neigh = get_neigh(grid, rnum, cnum, dirn, grid_size)
                if neigh[0] == grid[rnum, cnum]:
                    g2.addEdge(neigh[1] * grid_size + neigh[2], rnum * grid_size + cnum)

    return g1.NumberOfconnectedComponents(), g2.NumberOfconnectedComponents()