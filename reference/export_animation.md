# Save your animation at a desired quality.

This function is a helepr for `gganimate`'s `anim_save()`, providing a
simplified, though less feature-rich, version of these functions.
Animations are saved as `.gif`s at the desired path. With this function,
publication-quality (high-resolution and smooth) animations are
possible, but take a long time to render.

## Usage

``` r
export_animation(
  anim_object,
  path,
  duration = 30,
  fps = 10,
  width = 7.5,
  height = 5.5,
  dpi = 100
)
```

## Arguments

- anim_object:

  A `gganimate` object.

- path:

  A string representing the desired path and name at which to save
  animation.

- duration:

  Optional. A numeric, in seconds, representing the length of the
  animation. Default is 30.

- fps:

  Optional. The frames per second of the saved animation. Default is 10.

- width:

  Optional. The width of the exported image, in inches. Default is 7.5

- height:

  Optional. The height of the exported image, in inches. Default is 5.5.

- dpi:

  Optional. The resolution, in dots per inch, of the image. Default is
  100.

## Examples

``` r
c53_traj <- new_transittraj_data("get_trajectory_fun")

# Set my parameters
my_features <- data.frame(name = c("16th & U Stop"),
                          distance = c(13402.281))
my_dist_range <- c(13300, 13500)

# Create `gganimate` object
anim_line <- plot_animated_line(trajectory = c53_traj,
                                feature_distances = my_features,
                                route_color = "firebrick4",
                                label_field = "name",
                                label_alpha = 0.8,
                                label_pos = "right",
                                distance_lim = my_dist_range,
                                center_vehicles = TRUE,
                                timestep = 1)

# Create a place to store your file
my_file_name <- tempfile("my_animation", fileext = ".gif")
print(my_file_name)
#> [1] "/tmp/RtmpIB02wk/my_animation200c4a4cbb69.gif"

# Run function: save animation locally
if (interactive()) {
  # Note: Due to long processing times, animations will only be rendered &
  #       displayed if run during an interactive session. See `example()`.
  export_animation(anim_object = anim_line,
                   path = my_file_name,
                   width = 4, height = 8, dpi = 150,
                   fps = 10, duration = 10)
}
```
