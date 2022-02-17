#S3 classes for fieldplot

#NB if using ellipsis '...' in function, place arguments with defaults
# *after* the ellipsis argument. Otherwise argument will try to be filled with
# first input that should be handled by ellipsis.

#Constructor class for a coord.data.frame

#' Attempt to convert a data.frame to a coord.data.frame
#' @param .data A data.frame containing the data.
#' @param id Column containing the individual identifier of each coordinate point.
#' @param x Column containing the x-coordinate of each coordinate point.
#' @param y Column containing the y-coordinate of each coordinate point.
#' @param ... Attribute columns which should be preserved.
#' @export
#' @example
#'coords <- data.frame(id1,x1,y1,d1,species1)
#'names(coords) <- c("id","x","y","diameter","species")
#'coords %>% as.coord.data.frame(id = id,x = x,y = y,diameter,species)
as.coord.data.frame <- function(.data,id,x,y,...){
  stopifnot(is.logical(cartesian))
  quos <- enquos(...)
  output <- .data %>% select(id={{id}},x={{x}},y={{y}},!!!quos)
  class(output) <- c("coord.data.frame","data.frame")
  return(
    output
  )
}


#' Filter coordinate points
#' @description Filter coordinate points by maximum radius (inclusive).
#' @param radius Maximum radius from filter point.
#' @param x x coordinate of point to filter from.
#' @param y y coordinate of point to filter from.
#' @export
#' @example
#' coords <- data.frame(id1,x1,y1,d1,species1)
#'names(coords) <- c("id","x","y","diameter","species")
#'coords %>% as.coord.data.frame(id = id,x = x,y = y,diameter,species) %>% filter_radius()
filter_radius.coord.data.frame <- function(.data,x=0,y=0,radius=10){
   .data %>% mutate(distance=distance_2d(x1 = .data$x,y1 = .data$y,x2 = {{x}},y2 = {{y}})) %>% filter(distance<=radius)%>% select(-distance)
}

filter_radius <- function(.data,x=0,y=0,radius=10) UseMethod("filter_radius")


#' Rotation of coord.data.frame
#' @description Rotates coordinates in a coord.data.frame around the origin.
#' @param rotation Angle of rotation, radians. e.g. [fieldcoordinate::degree_to_radian()]
#' @return A coord.data.frame
#' @export
coordinate_rotation.coord.data.frame <- function(.data,x=x,y=y,rotation=0){

  .data %>% mutate(x=cos(rotation) * ({x}) + sin(rotation) * ({y}),
                   y=cos(rotation) * ({y}) - sin(rotation) * ({x})
                   )

}

#' @export
coordinate_rotation <- function(.data,x=0,y=0,rotation=0) UseMethod("coordinate_rotation")



#find rotation 0-360 which minimises the mean distance between matched trees.
#' @param rotation Rotation to exert on coordinate (x,y) in .data, in radians.
#' @return Mean distance between coordinates and their respective matches.
rotation_to_distance.coord.data.frame <- function(.data,rotation=0){
  .data %>% coordinate_rotation(rotation=rotation) %>%
    mutate(distance=distance_2d(x1=x,y1=y,x2=x.2,y2=y.2)) %>%
    summarise(mean(distance))
}

rotation_to_distance <- function(.data,rotation=0) UseMethod("rotation_to_distance")

#' Convert Cartesian coord.data.frame to Polar coordinate system.
#' @param .data A coord.data.frame with attribute 'coord.sys' set to 'cartesian'.
#' @return A coord.data.frame with attribute 'coord.sys' set to 'polar'.
#' @export

cartesian_to_polar_coordinate.coord.data.frame <- function(.data){
  stopifnot("x" %in% colnames(.data) & "y" %in% colnames(.data))

  .data %>% mutate(angle=atan2(y=.data$y,x=.data$x),
                   distance=sqrt((x)^2 + (y)^2)) %>% select(-c(x,y))

}

#'@export
cartesian_to_polar_coordinate <- function(x) UseMethod("cartesian_to_polar_coordinate")

#'Convert a Polar coord.data.frame to Cartesian coordinate system.
#'@param .data A coord.data.frame with attribute 'coord.sys' set to 'polar'.
#'@return A coord.data.frame with attribute 'coord.sys' set to 'cartesian'.

polar_to_cartesian_coordinate.coord.data.frame <- function(.data){

  stopifnot("distance" %in% colnames(.data) & "angle" %in% colnames(.data))

  .data %>% mutate(
  x = distance*cos(angle),
  y= distance*sin(angle)
  ) %>% select(-c(distance,angle))

}

#' @export
polar_to_cartesian_coordinate <- function(x) UseMethod("polar_to_cartesian_coordinate")




# list argument test
arglistfun <- function(required_match,.data,coord.df2){

}



#' Rotate to best match points between two coord.data.frames
#' @param .data A coord.data.frame which to rotate.
#' @param coord.df2 Data frame to which to rotate to.
#' @param diameter_tolerance Tolerance for an acceptable point match.
#' @return A list, containing a coord.data.frame with the rotated values; the minimising angle and the mean distance of the result rotation and coord.df2.
#' @export
match_rotate.coord.data.frame <- function(.data,coord.df2,diameter_tolerance=3){
  #enforce class.
  stopifnot("coord.data.frame" %in% class(.data)  & "coord.data.frame" %in% class(coord.df2))

  #Check required match columns are present in both coord.data.frames
  if(!all(c("diameter","species") %in% colnames(.data))) stop("Required columns for matching are not present in .data")
  if(!all(c("diameter","species") %in% colnames(coord.df2))) stop("Required columns for matching are not present in coord.df2")

  #Empty matches data.frame
  #add suffix to column names of coord.df2
  neighbouring <- as.data.frame(matrix(
    nrow = 0,
    ncol = (ncol(coords_df) + ncol(coord.df2)),
    dimnames = list(c(), c(
      colnames(coords.df), paste0(colnames(coords.df2), ".2")
    ))
  ))

  #For every row in the first data frame, find all acceptable matches.
  for(i in 1:nrow(.data)){
    point_species <- .data[i,"species"]
    point_diameter <- .data[i,"diameter"]
    point_x <- .data[i,"x"]
    point_y <- .data[i,"y"]

    neighbours <- coord.df2 %>% filter(species==point_species,
                                       diameter<=point_diameter+diameter_tolerance,
                                       diameter>=point_diameter-diameter_tolerance,
                                       !(id %in% neighbouring[,"id"])
                                       ) %>%
      mutate(distance=distance_2d(x1 =point_x,y1 = point_y,x2 = x,y2 = y)) %>%
      slice_min(order_by = distance,with_ties = FALSE) %>% select(-distance)

    #add to the output table.
    #Need to unname slice?
    neighbouring[i,] <- cbind(.data[i,],neighbours)

  }

  class(neighbouring) <- c("coord.data.frame","data.frame")



  #Which rotation minimises the mean distance between matched points?
  min_angle_f <- function(rotate_value,data){
    rotation_to_distance(.data=data,rotation=degree_to_radian(rotate_value))[[1]]
  }
  optim_result <- optimise(min_angle_f,lower = 0,upper = 360,data=neighbouring)

  #Minimising angle
  minimum_angle <- optim_result[[1]]

  #What is the resultant mean distance between matched points?
  mean_distance <- optim_result[[2]]

  #Return rotated.
  return(
    list(
      coord.df= coordinate_rotation(.data,rotation = minimum_angle),
      "minimum_angle"=minimum_angle,
      "mean_distance"=mean_distance
    )
  )

}

match_rotate <- function(.data,coord.df2,diameter_tolerance=3) UseMethod("match_rotate")



# Plotting a coord.data.frame
#'@export
plot.coord.data.frame <- function(data,radius=10){
 data %>% ggplot2::ggplot(aes(x=x,y=y)) +
    ggplot2::geom_point(size=(data$diameter)/100,color=data$species)+
    ggplot2::coord_fixed()+
    ggforce::geom_circle(aes(x0=0,y0=0,r=radius),inherit.aes=FALSE,size=1/10)
}
