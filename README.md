# The FieldCoordinate Package

This package is intended to create a class for point lists with local coordinate systems.

## The coord.data.frame

The coord.data.frame is a data.frame with the following mandatory columns:

-   *id* - point identifying variable.

-   *x* - X-coordinate.

-   *y -* Y-coordinate.

A coord.data.frame additionally can contain point-attribute variables.

Construct a coord.data.frame with the `as.coord.data.frame` function.

``` r
df %>% as.coord.data.frame(id=id,x=x,y=y,diameter,species)
```

## Implemented Methods

`filter_radius` Filter coordinate points by maximum radius (inclusive). Filter origin can be set.

``` r
coords.df %>% filter_radius(x=2, y=3, radius=10)
```

`plot` Plot a coord.data.frame by calling `ggplot2`. This is a very preliminary function that assumes that your coord.data.frame has the variables 'diameter' and 'species'.

![A simple plot of a coord.data.frame](https://github.com/Silviculturalist/fieldcoordinate/blob/main/img/coordinate_plot.png?raw=true)

## Convenience functions

### Transform to and from Polar Coordinates.

`cartesian_to_polar_coordinate`

`polar_to_cartesian_coordinate`

### Transform between degree and radian

`degree_to_radian`

`radian_to_degree`

### Point Manipulations

`coordinate_rotation` Rotate a coord.data.frame by an angle of rotation (radian).

`translate_coordinates` Translate points in a coord.data.frame coordinate system by x, y.

`coordinate_reflect` Reflect points in a coord.data.frame around an axis of the coordinate system.

### Image registration

`produce_gauss_matrix` Represent a number of coordinate points in a `coord.data.frame` as a gaussian process, where the amplitude is equivalent to an attribute of the point, the standard deviation is the positional error, and a certain resolution (number of cells per radius). To distinguish between the data structures, the `coord.data.frame` is transformed into a `gauss.coord.data.frame` .

This can be plotted with `plot` .

![A gaussian signal representation of a coord.data.frame](https://github.com/Silviculturalist/fieldcoordinate/blob/main/img/gauss1.png?raw=true)

Image creation and saving is handled by `save_gauss` which takes an `gauss.coord.data.frame` and saves it as an .png without any borders.

Read PNG images into R with the `png::readPNG()` function. To outline a number of euclidean geometric transformations, we use the image registration software interface RNiftyReg.

```{r}
#Image registration.
affine_transformation <- niftyreg.linear(gauss1_img,gauss2_img)

#the transformation matrix
forward(affine_transformation)
```

The Affine matrix can be handed to `fieldcoordinate::coordinate_transform` to apply the transformations on the coord.data.frame rather than the gaussian representation. OBSERVE this only handles translation and rotation so far.

![source, target, transformed source](https://github.com/Silviculturalist/fieldcoordinate/blob/main/img/animation.gif?raw=true)
