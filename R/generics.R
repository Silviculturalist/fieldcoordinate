#Generics

#' Cartesian to Polar coordinates
#' @param x x-coordinate.
#' @param y y-coordinate.
#' @param angle Logical. Default (TRUE).
#' @return A list containing two vectors 'angle' and 'distance'.
#'@export
cartesian_to_polar_coordinate <- function(x,y){
  return(list(
    "angle"=atan2(y={{y}},x={{x}}),
    "distance"=sqrt(({{x}})^2 + ({{y}})^2)
  ))
  }

#' Polar to Cartesian coordinates.
#' @param distance Distance from origin.
#' @param theta Angle (in radians)
#' @return A list containing two vectors 'x' and 'y'.
#' @export
polar_to_cartesian_coordinate <- function(distance,theta){
  return(list(
    x = distance*cos(theta),
    y= distance*sin(theta)
  ))
}

#' Calculate 2D distance between two coordinates
#' @param x1 X of coordinate 1
#' @param y1 Y of coordinate 1
#' @param x2 X of coordinate 2
#' @param y2 Y of coordinate 2
#' @return A vector of 2d distances
#' @export
distance_2d <- function(x1,y1,x2,y2){
  return(
    sqrt(((x2-x1)^2)+((y2-y1)^2))
  )
}

#'Convert an angle in degrees to radians.
#'@param degree Angle, in degrees.
#'@return Value of angle measured in Radians.
#'@export
degree_to_radian <- function(degree){
  degree * ((2 * pi) / 360)
}



#' Rotation of coordinates around origin.
#' @param rotation Angle of rotation, in radians.
#' @param x X coordinate
#' @param y Y coordinate
#' @return A list with vectors 'X' and 'Y'.
#' @export
coordinate_rotation <- function(x,y,rotation=0){
  return(
    list("x"=cos(theta) * (x) + sin(theta) * (y),
         "y"=cos(theta) * (y) - sin(theta) * (x)
         )
  )
}
