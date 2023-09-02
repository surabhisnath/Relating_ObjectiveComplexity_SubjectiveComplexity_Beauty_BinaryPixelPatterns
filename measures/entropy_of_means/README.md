Python implementation of the matrix information measurement examples from the
Stats StackExchange answer for
**Measuring entropy/ information/ patterns of a 2d binary matrix** which was originally written by
William Huber using Mathematica at http://stats.stackexchange.com/a/17556/43909

##### Running the program:
```python main.py```

##### Output:

![Output](https://raw.githubusercontent.com/cosmoharrigan/matrix-entropy/master/output.png)

```
---------
Matrix #0
---------

Neighborhood size = 1
[[ 0.  1.  1.  1.  0.]
 [ 1.  1.  0.  1.  1.]
 [ 1.  0.  1.  0.  0.]
 [ 0.  1.  1.  1.  1.]
 [ 1.  0.  1.  0.  0.]]

Neighborhood size = 2
[[ 0.75  0.75  0.75  0.75]
 [ 0.75  0.5   0.5   0.5 ]
 [ 0.5   0.75  0.75  0.5 ]
 [ 0.5   0.75  0.75  0.5 ]]

Neighborhood size = 3
[[ 0.66666667  0.66666667  0.55555556]
 [ 0.66666667  0.66666667  0.66666667]
 [ 0.66666667  0.55555556  0.55555556]]

Neighborhood size = 4
[[ 0.6875  0.6875]
 [ 0.625   0.5625]]

Profile:
[0.9709505944546686, 0.9886994082884974, 0.9182958340544896, 1.5]

---------
Matrix #1
---------

Neighborhood size = 1
[[ 0.  0.  1.  1.  0.]
 [ 0.  0.  1.  1.  0.]
 [ 1.  1.  1.  1.  1.]
 [ 0.  0.  1.  1.  0.]
 [ 1.  0.  1.  0.  0.]]

Neighborhood size = 2
[[ 0.    0.5   1.    0.5 ]
 [ 0.5   0.75  1.    0.75]
 [ 0.5   0.75  1.    0.75]
 [ 0.25  0.5   0.75  0.25]]

Neighborhood size = 3
[[ 0.55555556  0.77777778  0.77777778]
 [ 0.55555556  0.77777778  0.77777778]
 [ 0.66666667  0.66666667  0.66666667]]

Neighborhood size = 4
[[ 0.625   0.625 ]
 [ 0.625   0.5625]]

Profile:
[0.9988455359952018, 2.126614471810182, 1.5304930567574824, 0.8112781244591328]

---------
Matrix #2
---------

Neighborhood size = 1
[[ 1.  1.  1.  1.  1.]
 [ 1.  0.  0.  0.  1.]
 [ 1.  0.  0.  0.  1.]
 [ 1.  0.  0.  0.  1.]
 [ 1.  1.  1.  1.  1.]]

Neighborhood size = 2
[[ 0.75  0.5   0.5   0.75]
 [ 0.5   0.    0.    0.5 ]
 [ 0.5   0.    0.    0.5 ]
 [ 0.75  0.5   0.5   0.75]]

Neighborhood size = 3
[[ 0.55555556  0.33333333  0.55555556]
 [ 0.33333333  0.          0.33333333]
 [ 0.55555556  0.33333333  0.55555556]]

Neighborhood size = 4
[[ 0.4375  0.4375]
 [ 0.4375  0.4375]]

Profile:
[0.9426831892554922, 1.5, 1.3921472236645345, 0.0]

---------
Matrix #3
---------

Neighborhood size = 1
[[ 0.  1.  0.  1.  0.]
 [ 1.  0.  1.  0.  1.]
 [ 0.  1.  0.  1.  0.]
 [ 1.  0.  1.  0.  1.]
 [ 0.  1.  0.  1.  0.]]

Neighborhood size = 2
[[ 0.5  0.5  0.5  0.5]
 [ 0.5  0.5  0.5  0.5]
 [ 0.5  0.5  0.5  0.5]
 [ 0.5  0.5  0.5  0.5]]

Neighborhood size = 3
[[ 0.44444444  0.55555556  0.44444444]
 [ 0.55555556  0.44444444  0.55555556]
 [ 0.44444444  0.55555556  0.44444444]]

Neighborhood size = 4
[[ 0.5  0.5]
 [ 0.5  0.5]]

Profile:
[0.9988455359952018, 0.0, 0.9910760598382222, 0.0]

---------
Matrix #4
---------

Neighborhood size = 1
[[ 0.  0.  0.  0.  0.]
 [ 1.  1.  1.  1.  1.]
 [ 1.  1.  1.  1.  1.]
 [ 1.  1.  1.  1.  1.]
 [ 0.  0.  0.  0.  0.]]

Neighborhood size = 2
[[ 0.5  0.5  0.5  0.5]
 [ 1.   1.   1.   1. ]
 [ 1.   1.   1.   1. ]
 [ 0.5  0.5  0.5  0.5]]

Neighborhood size = 3
[[ 0.66666667  0.66666667  0.66666667]
 [ 1.          1.          1.        ]
 [ 0.66666667  0.66666667  0.66666667]]

Neighborhood size = 4
[[ 0.75  0.75]
 [ 0.75  0.75]]

Profile:
[0.9709505944546686, 1.0, 0.9182958340544896, 0.0]
```
