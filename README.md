# The FieldCoordinate Package

This package is intended to create a class for point lists with local coordinate systems.

## The coord.data.frame

The coord.data.frame is a data.frame with the following mandatory columns:

-   *id* - point identifying variable.

-   *x* - X-coordinate.

-   *y -* Y-coordinate.

A coord.data.frame additionally can contain point-attribute variables.

Construct a coord.data.frame with the `as.coord.data.frame` function.

``` R
df %>% as.coord.data.frame(id=id,x=x,y=y,diameter,species)
```

## Implemented Methods

`filter_radius` Filter coordinate points by maximum radius (inclusive). Filter origin can be set.

``` R
coords.df %>% filter_radius(x=2, y=3, radius=10)
```

`plot` Plot a coord.data.frame by calling `ggplot2`. This is a very preliminary function that assumes that your coord.data.frame has the variables 'diameter' and 'species'.

<img src="http://127.0.0.1:42742/graphics/90a511d0-67de-4bbe-b1e0-440fcd8c604f.png" width="500"/>

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

![](http://127.0.0.1:42742/graphics/6d4ad1f4-03e8-42b2-8684-907efd18baf2.png)

Image creation and saving is handled by `save_gauss` which takes an `gauss.coord.data.frame` and saves it as an .png without any borders.

Read PNG images into R with the `png::readPNG()` function. To outline a number of euclidean geometric transformations, we use the image registration software interface RNiftyReg.

```{r}
#Image registration.
affine_transformation <- niftyreg.linear(gauss1_img,gauss2_img)

#the transformation matrix
forward(affine_transformation)
```

The Affine matrix can be handed to `fieldcoordinate::coordinate_transform` to apply the transformations on the coord.data.frame rather than the gaussian representation. OBSERVE this only handles translation and rotation so far.

![](animation.gif)
