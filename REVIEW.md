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

### `MixedEffectsModelling.R`

- I notice you have a couple of `conflicted copy` files in the `plots` folder
    - You can delete those, I assume

- A few general stylistic things (for writing idiomatic R):
    - Lines should be less than 80 characters
    - It is not so common to use `{}` to define blocks
    - Use `snake_case`, not `camelCase` for names
    - Operators (*e.g.*, `+`, `=`, *etc.*) should always have spaces before and after
    - Try to always use `<-` (rather than `=`) for assignment
- (I have fixed some of these in the pull-request I made)
- Some of these are too much work to change, but I think that comments can easily be made less than 80 characters

- Line 1: 
    - I think you should expand the top comment here to include more information about what it does
    - You should also state that it is using `process.R`, and give some attributes
    - Maybe also mention that the code for plotting *etc.*, is in `utils/Utils.R`
- Line 6 (and others after):
    - Now that the functions are gone, I would remove all of the top-level `{}` blocks, and outdent the code
    - R doesn't like them (in terms of style)
    - I think you can get the same effect using more complete comments at the start of each block
- Line 11: Insert `source("process.R")`, so that it happens automatically?
    - And, remove that line from the instructions in the `README.md` file
- Lines 22, 23: `all_attemntion_failed` => `all_attention_failed`?
- Line 29: Why view data? Why now?
- Block starting at line 160:
    - You have a `for` loop which is generating (and printing) a bunch of output
    - I realise you don't have much control over how the results are printed
    - You can, however, control the overall structure
    - I think you should be printing information, and line breaks, as you loop, so that the output is at least a bit human-readable 
- Line 194: In the comments you say that this is the plot for AIII.5, but I don't think it is
- Line 220: You are using `f` here, which was defined on line 189. Can you do this earlier?
- Line 223: You could mention here where you use the mediation analysis
- Lines 242, 243: Printing some extra information here would be helpful
- Block starting at 249: Maybe explain what `disorder`, `disorder_1`, `disorder_2` are for
- Line 335+: Again, printing information while looping would be helpful
- Line 368: `Figure AIII.5`?
- Block beginning 381: I am not getting this plot generated. The resulting pdf is empty...
- Line 418, 420, 430, 432: Printing more context would be helpful
- Line 438+:
    - Your actual results get entirely lost in the `process` output
    - Is it worth waiting until the end of the last block in the file, and then outputting the values that you want to show, with a description of why they are important?

### `utils/Utils.R`

-  A few general things:
    - Use `FALSE` rather than `F` if you want to set something to false
    - My R linter doesn't like your variable names...
        - It wants `meanAIC` to be `mean_aic`, *etc.*
    - I am not really checking `ggCaterpillar`, since you have just taken the code from elsewhere

- Line 57: it is a bit dangerous to use function names (*e.g.*, `print` and `plot`) as variable names as well
- Line 77: `{` on the same line as the `for`
- Line 78-81: Should this be commented? Can it be removed?
    - Actually, is `VIFs` used for anything?
- Lines 155, 168, 183, 196: Use `FALSE` rather than `F`
- Line 212, 213: It seems like `plot_model` only works by accident...
    - You are using two variables (`data_train_fold1` and `data_test_fold1`) which are not actually defined in the function
    - I guess they must already exist in the workspace, which is why it works?
    - Better to pass them in as arguments
Lines 231, 232:  I am sure you can delete these ones!
