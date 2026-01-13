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
#>  1          1 head           1     1     1  1.48    0.0628      0.836
#>  2          1 head           1     1     2  0.0720  1.78        0.869
#>  3          1 head           1     1     3  2.13    1.38        0.884
#>  4          1 head           1     1     4 -1.48    1.63        0.861
#>  5          1 head           1     1     5  0.408  -0.808       0.560
#>  6          1 head           1     1     6  1.39    0.0956      0.966
#>  7          1 head           1     1     7  0.360   0.917       0.274
#>  8          1 head           1     1     8  0.655   0.474       0.751
#>  9          1 head           1     1     9  1.05   -0.395       0.621
#> 10          1 head           1     1    10 -1.98    0.429       0.830
#> # ℹ 1,640 more rows

# Create a 1D example
example_aniframe(n_dims = 1)
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right, shoulder_left, abdomen, hip_right,
#> #   hip_left, knee_right, knee_left, foot_right, foot_left
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time      x confidence
#>         <int> <fct>      <int> <int> <int>  <dbl>      <dbl>
#>  1          1 head           1     1     1  1.69       0.584
#>  2          1 head           1     1     2 -1.38       0.689
#>  3          1 head           1     1     3 -1.72       0.687
#>  4          1 head           1     1     4 -0.795      0.812
#>  5          1 head           1     1     5 -0.441      0.635
#>  6          1 head           1     1     6  0.357      0.929
#>  7          1 head           1     1     7  2.15       0.576
#>  8          1 head           1     1     8 -0.203      0.626
#>  9          1 head           1     1     9  0.642      0.823
#> 10          1 head           1     1    10  0.595      0.732
#> # ℹ 1,640 more rows

# Create a 3D example
example_aniframe(n_dims = 3)
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right, shoulder_left, abdomen, hip_right,
#> #   hip_left, knee_right, knee_left, foot_right, foot_left
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x        y        z confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>    <dbl>    <dbl>      <dbl>
#>  1          1 head           1     1     1  0.503  -0.642    0.00608      0.769
#>  2          1 head           1     1     2  0.708  -1.92    -0.437        0.545
#>  3          1 head           1     1     3 -0.598  -0.151   -1.15         0.892
#>  4          1 head           1     1     4  1.38   -1.37    -0.124        0.761
#>  5          1 head           1     1     5 -0.721   0.804   -0.287        0.763
#>  6          1 head           1     1     6  0.449  -1.28     0.320        0.720
#>  7          1 head           1     1     7  0.643   0.260    1.15         0.672
#>  8          1 head           1     1     8 -1.54    0.792   -1.48         0.338
#>  9          1 head           1     1     9  0.0678  0.00559 -0.324        0.769
#> 10          1 head           1     1    10 -1.08   -0.549   -0.197        0.589
#> # ℹ 1,640 more rows

# Create a smaller example with 2 individuals and 5 keypoints
example_aniframe(n_individuals = 2, n_keypoints = 5)
#> # Individuals: 1, 2
#> # Keypoints:   head, neck, shoulder_right, shoulder_left, abdomen
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time      x      y confidence
#>         <int> <fct>      <int> <int> <int>  <dbl>  <dbl>      <dbl>
#>  1          1 head           1     1     1  0.366 -1.27       0.694
#>  2          1 head           1     1     2 -0.465 -0.253      0.879
#>  3          1 head           1     1     3  0.281  0.237      0.795
#>  4          1 head           1     1     4 -0.612  0.462      0.583
#>  5          1 head           1     1     5 -0.361 -1.82       0.729
#>  6          1 head           1     1     6  1.29  -1.41       0.897
#>  7          1 head           1     1     7  1.05  -0.714      0.971
#>  8          1 head           1     1     8 -0.295 -0.359      0.901
#>  9          1 head           1     1     9  1.17  -2.08       0.720
#> 10          1 head           1     1    10 -1.46   0.920      0.834
#> # ℹ 490 more rows

# Create example with multiple trials and sessions
example_aniframe(n_obs = 100, n_trials = 3, n_sessions = 2)
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right, shoulder_left, abdomen, hip_right,
#> #   hip_left, knee_right, knee_left, foot_right, foot_left
#> # Sessions:    2
#> # Trials:      1, 2, 3
#>    individual keypoint session trial  time        x        y confidence
#>         <int> <fct>      <int> <int> <int>    <dbl>    <dbl>      <dbl>
#>  1          1 head           1     1     1  0.182   -1.24         0.902
#>  2          1 head           1     1     2  1.36     2.43         0.559
#>  3          1 head           1     1     3  0.305   -1.24         0.782
#>  4          1 head           1     1     4 -0.777    1.17         0.756
#>  5          1 head           1     1     5 -0.0690  -0.00404      0.379
#>  6          1 head           1     1     6  1.35     0.0745       0.745
#>  7          1 head           1     1     7 -2.82     0.0444       0.914
#>  8          1 head           1     1     8 -1.13    -0.800        0.511
#>  9          1 head           1     1     9  0.00323  1.05         0.619
#> 10          1 head           1     1    10  0.478    0.110        0.751
#> # ℹ 19,790 more rows

# Create minimal example with just centroid in 3D
example_aniframe(n_keypoints = 1, n_dims = 3)
#> # Individuals: 1, 2, 3
#> # Keypoints:   centroid
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x      y       z confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>  <dbl>   <dbl>      <dbl>
#>  1          1 centroid       1     1     1 -1.40    1.15   0.621       0.796
#>  2          1 centroid       1     1     2  1.29   -0.130 -0.729       0.587
#>  3          1 centroid       1     1     3 -0.583  -0.434 -1.18        0.782
#>  4          1 centroid       1     1     4  0.222  -0.237  0.470       0.612
#>  5          1 centroid       1     1     5  0.385  -0.161 -0.360       0.688
#>  6          1 centroid       1     1     6 -0.0305  0.122 -0.428       0.899
#>  7          1 centroid       1     1     7  0.265  -0.836 -0.512       0.648
#>  8          1 centroid       1     1     8 -0.120   1.43  -0.0130      0.752
#>  9          1 centroid       1     1     9  1.000   0.592  0.179       0.493
#> 10          1 centroid       1     1    10 -2.04   -0.366 -2.23        0.522
#> # ℹ 140 more rows
```
