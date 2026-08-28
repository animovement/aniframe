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
#>    individual keypoint session trial  time        x      y confidence
#>         <int> <fct>      <int> <int> <int>    <dbl>  <dbl>      <dbl>
#>  1          1 head           1     1     1 -1.58     0.428      0.780
#>  2          1 head           1     1     2 -0.0841   0.203      0.886
#>  3          1 head           1     1     3 -2.09     0.182      0.480
#>  4          1 head           1     1     4  0.00357  0.566      0.802
#>  5          1 head           1     1     5 -0.356   -0.278      0.881
#>  6          1 head           1     1     6  1.15    -1.36       0.672
#>  7          1 head           1     1     7 -0.221    1.30       0.268
#>  8          1 head           1     1     8  1.02    -1.99       0.589
#>  9          1 head           1     1     9 -0.264   -0.104      0.859
#> 10          1 head           1     1    10  1.66     0.300      0.694
#> # ℹ 1,640 more rows

# Create a 1D example
example_aniframe(n_dims = 1)
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right, shoulder_left, abdomen, hip_right,
#> #   hip_left, knee_right, knee_left, foot_right, foot_left
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time        x confidence
#>         <int> <fct>      <int> <int> <int>    <dbl>      <dbl>
#>  1          1 head           1     1     1  0.0476       0.834
#>  2          1 head           1     1     2  0.502        0.665
#>  3          1 head           1     1     3  0.00572      0.813
#>  4          1 head           1     1     4 -1.38         0.656
#>  5          1 head           1     1     5 -0.360        0.524
#>  6          1 head           1     1     6  0.583        0.460
#>  7          1 head           1     1     7  0.276        0.659
#>  8          1 head           1     1     8  0.590        0.843
#>  9          1 head           1     1     9  0.998        0.732
#> 10          1 head           1     1    10  1.30         0.702
#> # ℹ 1,640 more rows

# Create a 3D example
example_aniframe(n_dims = 3)
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right, shoulder_left, abdomen, hip_right,
#> #   hip_left, knee_right, knee_left, foot_right, foot_left
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x      y       z confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>  <dbl>   <dbl>      <dbl>
#>  1          1 head           1     1     1  0.785  -0.129 -0.0497      0.890
#>  2          1 head           1     1     2 -0.165  -0.139 -0.866       0.522
#>  3          1 head           1     1     3 -0.582  -1.41  -0.771       0.473
#>  4          1 head           1     1     4 -0.730  -0.689  1.57        0.730
#>  5          1 head           1     1     5 -0.838  -0.625  0.466       0.905
#>  6          1 head           1     1     6  0.805  -0.758  0.821       0.523
#>  7          1 head           1     1     7 -0.0723  0.115  0.0457      0.676
#>  8          1 head           1     1     8 -1.01   -0.410 -0.533       0.835
#>  9          1 head           1     1     9  0.911   1.13  -0.338       0.778
#> 10          1 head           1     1    10  0.547  -0.683  0.965       0.918
#> # ℹ 1,640 more rows

# Create a smaller example with 2 individuals and 5 keypoints
example_aniframe(n_individuals = 2, n_keypoints = 5)
#> # Individuals: 1, 2
#> # Keypoints:   head, neck, shoulder_right, shoulder_left, abdomen
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time      x       y confidence
#>         <int> <fct>      <int> <int> <int>  <dbl>   <dbl>      <dbl>
#>  1          1 head           1     1     1 -1.47  -0.0345      0.652
#>  2          1 head           1     1     2 -1.42  -0.579       0.974
#>  3          1 head           1     1     3  0.477 -0.834       0.395
#>  4          1 head           1     1     4 -0.349 -0.451       0.692
#>  5          1 head           1     1     5 -0.772  1.55        0.463
#>  6          1 head           1     1     6 -0.318 -2.27        0.630
#>  7          1 head           1     1     7 -0.191 -1.31        0.628
#>  8          1 head           1     1     8 -1.16  -0.885       0.633
#>  9          1 head           1     1     9  0.861  1.80        0.616
#> 10          1 head           1     1    10  2.12  -1.04        0.571
#> # ℹ 490 more rows

# Create example with multiple trials and sessions
example_aniframe(n_obs = 100, n_trials = 3, n_sessions = 2)
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right, shoulder_left, abdomen, hip_right,
#> #   hip_left, knee_right, knee_left, foot_right, foot_left
#> # Sessions:    1, 2
#> # Trials:      1, 2, 3
#>    individual keypoint session trial  time       x      y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>  <dbl>      <dbl>
#>  1          1 head           1     1     1  0.206  -0.403      0.763
#>  2          1 head           1     1     2 -0.401  -0.146      0.866
#>  3          1 head           1     1     3  0.854  -1.01       0.455
#>  4          1 head           1     1     4  1.62    2.45       0.814
#>  5          1 head           1     1     5 -0.696   0.336      0.623
#>  6          1 head           1     1     6  0.384   1.33       0.489
#>  7          1 head           1     1     7 -0.207   0.965      0.739
#>  8          1 head           1     1     8  0.110  -1.05       0.930
#>  9          1 head           1     1     9 -0.0791 -0.759      0.661
#> 10          1 head           1     1    10 -0.446  -0.633      0.605
#> # ℹ 19,790 more rows

# Create minimal example with just centroid in 3D
example_aniframe(n_keypoints = 1, n_dims = 3)
#> # Individuals: 1, 2, 3
#> # Keypoints:   centroid
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time      x       y       z confidence
#>         <int> <fct>      <int> <int> <int>  <dbl>   <dbl>   <dbl>      <dbl>
#>  1          1 centroid       1     1     1  0.813 -2.23    0.375       0.951
#>  2          1 centroid       1     1     2 -0.640 -0.428   0.584       0.872
#>  3          1 centroid       1     1     3 -0.166 -1.33    1.68        0.674
#>  4          1 centroid       1     1     4 -1.15  -0.0740  1.16        0.844
#>  5          1 centroid       1     1     5 -1.59   0.975  -0.0568      0.383
#>  6          1 centroid       1     1     6 -0.373 -1.17   -0.852       0.779
#>  7          1 centroid       1     1     7 -0.562 -1.14   -1.41        0.792
#>  8          1 centroid       1     1     8  1.63   0.425   1.48        0.880
#>  9          1 centroid       1     1     9  1.45   0.556  -0.455       0.865
#> 10          1 centroid       1     1    10  0.386 -0.950   0.277       0.452
#> # ℹ 140 more rows
```
