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
#>    individual keypoint session trial  time       x       y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>   <dbl>      <dbl>
#>  1          1 head           1     1     1  0.651  -2.20        0.845
#>  2          1 head           1     1     2  0.344   0.580       0.614
#>  3          1 head           1     1     3  1.48    0.0628      0.836
#>  4          1 head           1     1     4  0.0720  1.78        0.869
#>  5          1 head           1     1     5  2.13    1.38        0.884
#>  6          1 head           1     1     6 -1.48    1.63        0.861
#>  7          1 head           1     1     7  0.408  -0.808       0.560
#>  8          1 head           1     1     8  1.39    0.0956      0.966
#>  9          1 head           1     1     9  0.360   0.917       0.274
#> 10          1 head           1     1    10  0.655   0.474       0.751
#> # ℹ 1,640 more rows

# Create a 1D example
example_aniframe(n_dims = 1)
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right, shoulder_left, abdomen, hip_right,
#> #   hip_left, knee_right, knee_left, foot_right, foot_left
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>      <dbl>
#>  1          1 head           1     1     1 -0.0883      0.611
#>  2          1 head           1     1     2  0.383       0.912
#>  3          1 head           1     1     3  1.69        0.584
#>  4          1 head           1     1     4 -1.38        0.689
#>  5          1 head           1     1     5 -1.72        0.687
#>  6          1 head           1     1     6 -0.795       0.812
#>  7          1 head           1     1     7 -0.441       0.635
#>  8          1 head           1     1     8  0.357       0.929
#>  9          1 head           1     1     9  2.15        0.576
#> 10          1 head           1     1    10 -0.203       0.626
#> # ℹ 1,640 more rows

# Create a 3D example
example_aniframe(n_dims = 3)
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right, shoulder_left, abdomen, hip_right,
#> #   hip_left, knee_right, knee_left, foot_right, foot_left
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time      x       y        z confidence
#>         <int> <fct>      <int> <int> <int>  <dbl>   <dbl>    <dbl>      <dbl>
#>  1          1 head           1     1     1 -0.801 -0.805   0.0915       0.900
#>  2          1 head           1     1     2  0.169  0.0930  1.37         0.620
#>  3          1 head           1     1     3  0.503 -0.642   0.00608      0.769
#>  4          1 head           1     1     4  0.708 -1.92   -0.437        0.545
#>  5          1 head           1     1     5 -0.598 -0.151  -1.15         0.892
#>  6          1 head           1     1     6  1.38  -1.37   -0.124        0.761
#>  7          1 head           1     1     7 -0.721  0.804  -0.287        0.763
#>  8          1 head           1     1     8  0.449 -1.28    0.320        0.720
#>  9          1 head           1     1     9  0.643  0.260   1.15         0.672
#> 10          1 head           1     1    10 -1.54   0.792  -1.48         0.338
#> # ℹ 1,640 more rows

# Create a smaller example with 2 individuals and 5 keypoints
example_aniframe(n_individuals = 2, n_keypoints = 5)
#> # Individuals: 1, 2
#> # Keypoints:   head, neck, shoulder_right, shoulder_left, abdomen
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time      x      y confidence
#>         <int> <fct>      <int> <int> <int>  <dbl>  <dbl>      <dbl>
#>  1          1 head           1     1     1  0.745 -0.147      0.633
#>  2          1 head           1     1     2  0.581 -0.131      0.880
#>  3          1 head           1     1     3  0.366 -1.27       0.694
#>  4          1 head           1     1     4 -0.465 -0.253      0.879
#>  5          1 head           1     1     5  0.281  0.237      0.795
#>  6          1 head           1     1     6 -0.612  0.462      0.583
#>  7          1 head           1     1     7 -0.361 -1.82       0.729
#>  8          1 head           1     1     8  1.29  -1.41       0.897
#>  9          1 head           1     1     9  1.05  -0.714      0.971
#> 10          1 head           1     1    10 -0.295 -0.359      0.901
#> # ℹ 490 more rows

# Create example with multiple trials and sessions
example_aniframe(n_obs = 100, n_trials = 3, n_sessions = 2)
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right, shoulder_left, abdomen, hip_right,
#> #   hip_left, knee_right, knee_left, foot_right, foot_left
#> # Sessions:    1, 2
#> # Trials:      1, 2, 3
#>    individual keypoint session trial  time       x        y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>    <dbl>      <dbl>
#>  1          1 head           1     1     1  1.25   -1.37         0.507
#>  2          1 head           1     1     2 -0.0212 -0.493        0.802
#>  3          1 head           1     1     3  0.182  -1.24         0.902
#>  4          1 head           1     1     4  1.36    2.43         0.559
#>  5          1 head           1     1     5  0.305  -1.24         0.782
#>  6          1 head           1     1     6 -0.777   1.17         0.756
#>  7          1 head           1     1     7 -0.0690 -0.00404      0.379
#>  8          1 head           1     1     8  1.35    0.0745       0.745
#>  9          1 head           1     1     9 -2.82    0.0444       0.914
#> 10          1 head           1     1    10 -1.13   -0.800        0.511
#> # ℹ 19,790 more rows

# Create minimal example with just centroid in 3D
example_aniframe(n_keypoints = 1, n_dims = 3)
#> # Individuals: 1, 2, 3
#> # Keypoints:   centroid
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x      y       z confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>  <dbl>   <dbl>      <dbl>
#>  1          1 centroid       1     1     1 -1.11   -0.919 -2.00        0.525
#>  2          1 centroid       1     1     2 -1.92    1.06   0.840       0.653
#>  3          1 centroid       1     1     3 -1.40    1.15   0.621       0.796
#>  4          1 centroid       1     1     4  1.29   -0.130 -0.729       0.587
#>  5          1 centroid       1     1     5 -0.583  -0.434 -1.18        0.782
#>  6          1 centroid       1     1     6  0.222  -0.237  0.470       0.612
#>  7          1 centroid       1     1     7  0.385  -0.161 -0.360       0.688
#>  8          1 centroid       1     1     8 -0.0305  0.122 -0.428       0.899
#>  9          1 centroid       1     1     9  0.265  -0.836 -0.512       0.648
#> 10          1 centroid       1     1    10 -0.120   1.43  -0.0130      0.752
#> # ℹ 140 more rows
```
