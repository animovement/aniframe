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
#>    individual keypoint session trial  time      x       y confidence
#>         <int> <fct>      <int> <int> <int>  <dbl>   <dbl>      <dbl>
#>  1          1 head           1     1     1  0.514 -1.59        0.658
#>  2          1 head           1     1     2 -1.75   2.07        0.469
#>  3          1 head           1     1     3  0.894 -1.70        0.724
#>  4          1 head           1     1     4  0.223  0.861       0.828
#>  5          1 head           1     1     5  0.581 -0.380       0.873
#>  6          1 head           1     1     6 -0.178  1.85        0.764
#>  7          1 head           1     1     7  0.741  1.04        0.932
#>  8          1 head           1     1     8 -0.997 -1.47        0.723
#>  9          1 head           1     1     9 -2.94  -2.78        0.558
#> 10          1 head           1     1    10  0.719 -0.0770      0.594
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
#>  1          1 head           1     1     1 -0.678       0.869
#>  2          1 head           1     1     2 -0.0496      0.738
#>  3          1 head           1     1     3  1.72        0.558
#>  4          1 head           1     1     4 -0.172       0.966
#>  5          1 head           1     1     5  0.536       0.354
#>  6          1 head           1     1     6  0.765       0.572
#>  7          1 head           1     1     7 -0.953       0.313
#>  8          1 head           1     1     8  0.308       0.557
#>  9          1 head           1     1     9  0.387       0.577
#> 10          1 head           1     1    10 -0.603       0.549
#> # ℹ 1,640 more rows

# Create a 3D example
example_aniframe(n_dims = 3)
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right, shoulder_left, abdomen, hip_right,
#> #   hip_left, knee_right, knee_left, foot_right, foot_left
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time      x       y      z confidence
#>         <int> <fct>      <int> <int> <int>  <dbl>   <dbl>  <dbl>      <dbl>
#>  1          1 head           1     1     1 -1.55  -0.419  -0.766      0.863
#>  2          1 head           1     1     2 -0.925 -0.683   0.294      0.723
#>  3          1 head           1     1     3  1.98   0.423   0.365      0.633
#>  4          1 head           1     1     4 -3.11   0.0307 -0.312      0.397
#>  5          1 head           1     1     5  1.01  -0.0851 -1.12       0.489
#>  6          1 head           1     1     6 -1.70   0.495   1.49       0.697
#>  7          1 head           1     1     7 -0.774 -2.64    0.762      0.559
#>  8          1 head           1     1     8  0.757  0.300  -0.270      0.839
#>  9          1 head           1     1     9 -1.25   0.0213 -1.56       0.664
#> 10          1 head           1     1    10  1.06   0.0850 -1.05       0.733
#> # ℹ 1,640 more rows

# Create a smaller example with 2 individuals and 5 keypoints
example_aniframe(n_individuals = 2, n_keypoints = 5)
#> # Individuals: 1, 2
#> # Keypoints:   head, neck, shoulder_right, shoulder_left, abdomen
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x      y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>  <dbl>      <dbl>
#>  1          1 head           1     1     1  2.79    0.883      0.613
#>  2          1 head           1     1     2  1.48    0.999      0.637
#>  3          1 head           1     1     3 -0.0150  1.09       0.749
#>  4          1 head           1     1     4 -0.595   0.771      0.874
#>  5          1 head           1     1     5 -0.466   0.904      0.783
#>  6          1 head           1     1     6  0.104   0.185      0.895
#>  7          1 head           1     1     7 -1.07    0.711      0.752
#>  8          1 head           1     1     8 -0.761  -0.933      0.704
#>  9          1 head           1     1     9  0.496   0.110      0.736
#> 10          1 head           1     1    10  0.184  -0.626      0.740
#> # ℹ 490 more rows

# Create example with multiple trials and sessions
example_aniframe(n_obs = 100, n_trials = 3, n_sessions = 2)
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right, shoulder_left, abdomen, hip_right,
#> #   hip_left, knee_right, knee_left, foot_right, foot_left
#> # Sessions:    1, 2
#> # Trials:      1, 2, 3
#>    individual keypoint session trial  time      x       y confidence
#>         <int> <fct>      <int> <int> <int>  <dbl>   <dbl>      <dbl>
#>  1          1 head           1     1     1  1.45  -0.104       0.902
#>  2          1 head           1     1     2  2.24   0.0875      0.601
#>  3          1 head           1     1     3  0.359 -1.12        0.765
#>  4          1 head           1     1     4 -0.163 -0.518       0.992
#>  5          1 head           1     1     5 -0.341  0.102       0.526
#>  6          1 head           1     1     6  0.627  1.16        0.848
#>  7          1 head           1     1     7  0.109 -0.576       0.498
#>  8          1 head           1     1     8  0.909  0.0758      0.499
#>  9          1 head           1     1     9 -0.816  0.101       0.857
#> 10          1 head           1     1    10  1.87  -0.496       0.939
#> # ℹ 19,790 more rows

# Create minimal example with just centroid in 3D
example_aniframe(n_keypoints = 1, n_dims = 3)
#> # Individuals: 1, 2, 3
#> # Keypoints:   centroid
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time      x      y      z confidence
#>         <int> <fct>      <int> <int> <int>  <dbl>  <dbl>  <dbl>      <dbl>
#>  1          1 centroid       1     1     1 -0.717 -0.468  0.808      0.841
#>  2          1 centroid       1     1     2  0.623  0.121  0.684      0.959
#>  3          1 centroid       1     1     3  0.809 -2.72   0.252      0.652
#>  4          1 centroid       1     1     4 -0.825  1.40   0.226      0.740
#>  5          1 centroid       1     1     5 -0.184 -1.25  -0.155      0.810
#>  6          1 centroid       1     1     6  1.27  -0.479 -0.661      0.763
#>  7          1 centroid       1     1     7  1.26   0.918  0.552      0.643
#>  8          1 centroid       1     1     8 -1.59  -1.49  -1.82       0.823
#>  9          1 centroid       1     1     9  0.774 -0.958 -0.585      0.730
#> 10          1 centroid       1     1    10 -0.304 -1.48   0.200      0.916
#> # ℹ 140 more rows
```
