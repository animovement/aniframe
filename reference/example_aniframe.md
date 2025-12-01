# Create example aniframe data

Generates a synthetic aniframe object with random coordinates for
testing and demonstration purposes. The function creates a complete
design with all combinations of time points, individuals, keypoints,
trials, and sessions.

## Usage

``` r
example_aniframe(
  n_obs = 50,
  n_individuals = 3,
  n_keypoints = 11,
  n_trials = 1,
  n_sessions = 1,
  n_dims = 2
)
```

## Arguments

- n_obs:

  Integer. Number of time observations per combination. Default is 50.

- n_individuals:

  Integer. Number of individuals to simulate. Default is 3.

- n_keypoints:

  Integer. Number of keypoints per individual (max 11). Default is 11.
  When set to 1, only "centroid" is used. Otherwise, anatomical
  keypoints are used (head, neck, shoulders, etc.).

- n_trials:

  Integer. Number of trials per session. Default is 1.

- n_sessions:

  Integer. Number of sessions. Default is 1.

- n_dims:

  Integer. Number of spatial dimensions (1, 2, or 3). Default is 2. If
  1, only x coordinates are generated. If 2, x and y coordinates are
  generated. If 3, x, y, and z coordinates are generated.

## Value

An aniframe object containing randomly generated tracking data with
columns for individual, keypoint, time, trial, session, and spatial
coordinates (x, y, and/or z depending on `n_dims`). The coordinates are
drawn from a standard normal distribution.

## Examples

``` r
# Create a basic example with default parameters (2D)
example_aniframe()
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right, shoulder_left, abdomen, hip_right,
#> #   hip_left, knee_right, knee_left, foot_right, foot_left
#> # Sessions:    1
#> # Trials:      1
#>    session trial individual keypoint  time      x      y confidence
#>      <int> <int> <fct>      <fct>    <int>  <dbl>  <dbl>      <dbl>
#>  1       1     1 1          head         1 -1.06   0.551      0.414
#>  2       1     1 1          head         2 -0.796  0.330      0.847
#>  3       1     1 1          head         3 -1.76   0.173      0.795
#>  4       1     1 1          head         4 -0.691  0.427      0.761
#>  5       1     1 1          head         5 -0.559 -0.814      0.635
#>  6       1     1 1          head         6 -0.537  0.651      0.721
#>  7       1     1 1          head         7  0.227  0.920      0.651
#>  8       1     1 1          head         8  0.978  0.390      0.757
#>  9       1     1 1          head         9 -0.209  0.957      0.675
#> 10       1     1 1          head        10 -1.40  -0.170      0.795
#> # ℹ 1,640 more rows

# Create a 1D example
example_aniframe(n_dims = 1)
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right, shoulder_left, abdomen, hip_right,
#> #   hip_left, knee_right, knee_left, foot_right, foot_left
#> # Sessions:    1
#> # Trials:      1
#>    session trial individual keypoint  time        x confidence
#>      <int> <int> <fct>      <fct>    <int>    <dbl>      <dbl>
#>  1       1     1 1          head         1 -1.38         0.842
#>  2       1     1 1          head         2  0.0854       0.932
#>  3       1     1 1          head         3  0.00483      0.714
#>  4       1     1 1          head         4  0.875        0.735
#>  5       1     1 1          head         5  2.53         0.890
#>  6       1     1 1          head         6  0.672        0.921
#>  7       1     1 1          head         7 -0.378        0.843
#>  8       1     1 1          head         8  0.820        0.689
#>  9       1     1 1          head         9 -1.23         0.706
#> 10       1     1 1          head        10  0.853        0.696
#> # ℹ 1,640 more rows

# Create a 3D example
example_aniframe(n_dims = 3)
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right, shoulder_left, abdomen, hip_right,
#> #   hip_left, knee_right, knee_left, foot_right, foot_left
#> # Sessions:    1
#> # Trials:      1
#>    session trial individual keypoint  time      x       y      z confidence
#>      <int> <int> <fct>      <fct>    <int>  <dbl>   <dbl>  <dbl>      <dbl>
#>  1       1     1 1          head         1  1.95  -1.18   -0.879      0.960
#>  2       1     1 1          head         2 -1.80   3.05    1.42       0.497
#>  3       1     1 1          head         3 -0.102 -0.208   1.11       0.455
#>  4       1     1 1          head         4  0.904  0.442   0.410      0.739
#>  5       1     1 1          head         5  1.05  -0.186   0.816      0.706
#>  6       1     1 1          head         6  0.441  0.541  -1.94       0.700
#>  7       1     1 1          head         7  2.94  -1.65   -1.23       0.892
#>  8       1     1 1          head         8 -1.66   0.622  -1.35       0.576
#>  9       1     1 1          head         9  0.130 -0.323  -1.60       0.588
#> 10       1     1 1          head        10 -0.483  0.0668 -1.03       0.919
#> # ℹ 1,640 more rows

# Create a smaller example with 2 individuals and 5 keypoints
example_aniframe(n_individuals = 2, n_keypoints = 5)
#> # Individuals: 1, 2
#> # Keypoints:   head, neck, shoulder_right, shoulder_left, abdomen
#> # Sessions:    1
#> # Trials:      1
#>    session trial individual keypoint  time      x       y confidence
#>      <int> <int> <fct>      <fct>    <int>  <dbl>   <dbl>      <dbl>
#>  1       1     1 1          head         1  0.962 -1.49        0.578
#>  2       1     1 1          head         2  0.337 -1.05        0.610
#>  3       1     1 1          head         3 -1.18  -0.0417      0.442
#>  4       1     1 1          head         4 -0.734  0.0916      0.789
#>  5       1     1 1          head         5 -0.363 -1.16        0.701
#>  6       1     1 1          head         6 -0.462  0.231       0.794
#>  7       1     1 1          head         7 -1.40   1.08        0.919
#>  8       1     1 1          head         8  1.18  -0.288       0.739
#>  9       1     1 1          head         9  0.699  0.739       0.802
#> 10       1     1 1          head        10  0.104  0.515       0.673
#> # ℹ 490 more rows

# Create example with multiple trials and sessions
example_aniframe(n_obs = 100, n_trials = 3, n_sessions = 2)
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right, shoulder_left, abdomen, hip_right,
#> #   hip_left, knee_right, knee_left, foot_right, foot_left
#> # Sessions:    2
#> # Trials:      1, 2, 3
#>    session trial individual keypoint  time      x      y confidence
#>      <int> <int> <fct>      <fct>    <int>  <dbl>  <dbl>      <dbl>
#>  1       1     1 1          head         1 -2.56  -1.11       0.671
#>  2       1     1 1          head         2  0.291  0.136      0.961
#>  3       1     1 1          head         3 -0.123 -2.10       0.699
#>  4       1     1 1          head         4  0.127  0.123      0.697
#>  5       1     1 1          head         5  1.91  -0.585      0.810
#>  6       1     1 1          head         6  0.856  2.41       0.602
#>  7       1     1 1          head         7 -0.777  0.421      0.890
#>  8       1     1 1          head         8 -0.725 -0.408      0.803
#>  9       1     1 1          head         9 -0.584 -0.921      0.509
#> 10       1     1 1          head        10  0.176  2.63       0.656
#> # ℹ 19,790 more rows

# Create minimal example with just centroid in 3D
example_aniframe(n_keypoints = 1, n_dims = 3)
#> # Individuals: 1, 2, 3
#> # Keypoints:   centroid
#> # Sessions:    1
#> # Trials:      1
#>    session trial individual keypoint  time       x      y      z confidence
#>      <int> <int> <fct>      <fct>    <int>   <dbl>  <dbl>  <dbl>      <dbl>
#>  1       1     1 1          centroid     1 -0.0259 -1.12  -0.159      0.751
#>  2       1     1 1          centroid     2  1.16   -1.94   0.246      0.874
#>  3       1     1 1          centroid     3  0.823  -0.476 -1.20       0.900
#>  4       1     1 1          centroid     4  0.681  -0.293 -0.799      0.775
#>  5       1     1 1          centroid     5  0.346   1.09   0.722      0.897
#>  6       1     1 1          centroid     6 -0.0847 -0.699  0.936      0.468
#>  7       1     1 1          centroid     7  0.557  -1.51  -1.67       0.572
#>  8       1     1 1          centroid     8  0.0176 -0.236  0.260      0.710
#>  9       1     1 1          centroid     9 -0.281  -1.97  -0.274      0.541
#> 10       1     1 1          centroid    10  0.320   1.36   1.27       0.679
#> # ℹ 140 more rows
```
