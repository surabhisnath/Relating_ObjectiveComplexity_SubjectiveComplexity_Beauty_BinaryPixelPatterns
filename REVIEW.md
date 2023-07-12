# Review

## General

- It is all really good generally
- There are no show-stoppers
- But, here are a bunch of suggestions anyway
  - You are welcome to ignore any/all of them!

## Comments

- Now installs and runs on windows/linux(/mac)
	- Both the python and the R code seem to run successfully everywhere I have tested

### `README.md`

- Mostly great:
	- Good explanation of what it is/is for
	- Usage instructions work with copy and paste
- Still missing:
	- A description of what to do after you run the code
	- *i.e.*, Where are the resulting diagrams:
		- In the jupyter notebook?
		- In a subdir? (Why both?)
	- *i.e.*, What does the output in the R term mean?
		- Is it important?
		- What is output to file, and why?
  - Move `Setup` section above `Repository Description`?

### `DescriptiveAnalysis.ipynb`

- It all runs!
- Is there a reason why those two figures are saved, but not the others?
- Why does the generated figure 4 include numbers that are missing from the plot in the paper?
    - *E.g.*, lambda by LSC
- Obviously, not all the figures in the paper are produced by this code. Is it worth explaining why?
- Cell 3: Where is `pearsonr_ci` being used?
    - I only see it used once, and it is commented out
    - Also, it is a few cells below
 - Cell 4: It would be good to have slightly more detail for each of the `variables`
 - Cell 5: Is `subjects` used anywhere?
 - Cell 6:
     - `[[] for i in range(num_patterns)]` can be written `[[]] * num_patterns`
     - I would probably `file.split("_")`, and use that rather than indexing into the string
     - `csv` is a strange name for a dataframe
     - Use `path.join` to join paths (rather than `+`)
     - Sometimes you use `.to_numpy()`, sometimes you use `.value`. Aren't those doing the same thing?
- Cell 7: 
    - I am unsure about the commented out code.
        - If it is important, should it be uncommented?
        - If not, can it be deleted?
    - Is there a reason you do the correlation with some columns, and then `drop` them?
 - Cell 8: 
     - Is this table in the paper? If so, where?
     - I don't think any of the `tolist()` calls are doing anything...
 - Cell 11:
     - Unused `import gmr`
     - It would be good to comment the bit that splits testing and training data
     - Can you delete `# print(RMSE_test)`?
         - If so, do you need to calculate it?
     - I think you are missing a `plt.show()` at the end of the cell
- Cell 14: "onkective" == "objective"?
- Cell 15: Although it is clear in usage below, maybe comment to say what `to_display` is for
- Cell 18: 
    - Is it worth mentioning in the comments that the title is the mean rating?
        - Same for following cells
- Cell 23:
    - Maybe a comment to explain what you mean by `confusing` vs. `consistent`?

### `calculate_entropy.py`

- Line 31: I am sure is doesn't matter, but you have `P(b)` and `P(w)` reversed with respect to eqn. 1

### `calculate_intracacy.py`

- This is all fine, but I think you can do it more efficiently (by, *e.g.*, precomputing neighbours)

### `calculate_Kolmogorov_complexity.py`

- `sys` is imported but unused

### `calculate_mean_entropy.py`

- `math` imported but not used

### `entropy_of_means`

- As you make clear in your comments, this code is taken from someone else's git repo
- I think Kevin mentioned the possibility of you rewriting it yourself, to simplify things
- My suggestion would be to:
    - Keep using the code (since it seems a bit like busy-work to reimplement something that should give you the exact same result)
    - Restructure the directory, so that it contains:
        - the code you have added (`calculate_entropy_of_means.py`) at the top level
        - A complete copy of the repo you are using in a subdirectory
    - That should make it clearer what code is yours, and what you ... borrowed
    - The comments on your code should also mention:
        - The url of the repo you have copied
        - The date you copied it
    - `stats` imported but not used
- Does that make sense?