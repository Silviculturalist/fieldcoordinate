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
coordinate_rotation.coord.data.frame <- function(.data,rotation=0){

  .data %>% mutate(x=cos(rotation) * (x) + sin(rotation) * (y),
                   y=cos(rotation) * (y) - sin(rotation) * (x)
                   )

}

coordinate_rotation <- function(.data,x=0,y=0,rotation=0) UseMethod("coordinate_rotation")


#' Convert Cartesian coord.data.frame to Polar coordinate system.
#' @param .data A coord.data.frame

cartesian_to_polar_coordinate.coord.data.frame <- function(.data){
  stopifnot("x" %in% colnames(.data) & "y" %in% colnames(.data))

  .data %>% mutate(angle=atan2(y=.data$y,x=.data$x),
                   distance=sqrt((x)^2 + (y)^2)) %>% select(-c(x,y))

}

cartesian_to_polar_coordinate <- function(x) UseMethod("cartesian_to_polar_coordinate")

#'Convert a Polar coord.data.frame to Cartesian coordinate system.
#'@param .data A coord.data.frame

polar_to_cartesian_coordinate.coord.data.frame <- function(.data){

  stopifnot("distance" %in% colnames(.data) & "angle" %in% colnames(.data))

  .data %>% mutate(
  x = distance*cos(angle),
  y= distance*sin(angle)
  ) %>% select(-c(distance,angle))

}

polar_to_cartesian_coordinate <- function(x) UseMethod("polar_to_cartesian_coordinate")


#Mean distance
coordinate_mean_distance.coord.data.frame <- function(.data){

}

coordinate_mean_distance <- function(x) UseMethod("coordinate_mean_distance")

