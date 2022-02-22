#Generics

#' Cartesian to Polar coordinates
#' @param x x-coordinate.
#' @param y y-coordinate.
#' @param angle Logical. Default (TRUE).
#' @return A list containing two vectors 'angle' and 'distance'.
#'@export
cartesian_to_polar_coordinate.numeric <- function(x,y){
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
polar_to_cartesian_coordinate.numeric <- function(distance,theta){
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

#' Convert an angle in radians to degrees.
#' @param radian Angle, in radians.
#' @return Value of angle measured in degrees.
#' @export
radian_to_degree <- function(radian){
  radian*(180/pi)
}


#' Rotation of coordinates around origin.
#' @param rotation Angle of rotation, in radians.
#' @param x X coordinate
#' @param y Y coordinate
#' @return A list with vectors 'X' and 'Y'.
#' @export
coordinate_rotation.numeric <- function(x,y,rotation=0){
  return(
    list("x"=cos(theta) * (x) + sin(theta) * (y),
         "y"=cos(theta) * (y) - sin(theta) * (x)
         )
  )
}

#' Circular filter for coordinate vectors
#' @param x Vector of x coordinates
#' @param y Vector of y coordinates
#' @param radius Maximum radius, inclusive.
#' @return list of two vectors: 'x' & 'y'.
filter_radius.numeric <- function(x,y,radius=10,...){

    distances <- distance_2d(x1 = 0,y1=0,x2 = {{x}},y2 = {{y}})

  return(
    list(
      "x"=x[which(distances<=radius)],
      "y"=y[which(distances<=radius)]
    )
  )
}


#'
#' #' Rotate match diameter
#' #' @param id1
#' #' @param id2
#' #' @param x1
#' #' @param x2
#' #' @param y1
#' #' @param y2
#' #' @diameter_tolerance
#' #' @return coord.data.frame
#' #'
#' rotate_match_diameter <- function(id1,x1,y1,d1,species1,id2,x2,y2,d2,species2,diameter_tolerance=3){
#'
#'   #Data frames
#'   coords <- data.frame("id"=id1,"x"=x1,"y"=y1,"diameter"=d1,"species"=species1)
#'   coords2 <- data.frame("id2"=id2,"x2"=x2,"y2"=y2,"diameter2"=d2,"species2"=species2)
#'
#'   #Minimise the mean distance between two trees with a diameter within the tolerance without replacement.
#'   #Trees of different species are not matched.
#'
#'   #Empty matches data.frame
#'   neighbouring <- as.data.frame(matrix(nrow=0,ncol=11,dimnames = list(c(),c("id","x","y","diameter","species","id2","x2","y2","diameter2","species2","distance"))))
#'
#'   for(i in 1:nrow(coords)){
#'     #trees of same species, suitable diameter.
#'     neighbours <- coords2[coords2$species2==coords[i,"species"] &
#'                             coords2$diameter2<=(coords[i,"diameter"]+diameter_tolerance) &
#'                             coords2$diameter2>=(coords[i,"diameter"]-diameter_tolerance) &
#'                             !(coords2$id2%in%neighbouring$id),]
#'
#'     neighbours$distance <- sqrt(((coords[i,"x"]-neighbours$x)^2) +((coords[i,"y"]-neighbours$y)^2))
#'
#'     #Populating matches data.frame
#'     neighbouring[i,]<- cbind(coords[i,],neighbours[which.min(neighbours$distance),])
#'   }
#'
#'   #Which degree rotation of (x2,y2) minimises the distance between the matching trees?
#'   minimum_angle <- optimise(rotation_to_distance,interval=c(0,360),x1=neighbouring$x,x.2=neighbouring$x2,y1=neighbouring$y,y.2=neighbouring$y2)[[1]]
#'
#'   #Value of mean distance between matches for minimum angle.
#'   mean_distance <- rotation_to_distance(rotation=minimum_angle,x1 = neighbouring$x,y1 = neighbouring$y,x.2=neighbouring$x2,y.2=neighbouring$y2)
#'
#'   xy <- polar_coordinate_rotation(rotation = minimum_angle,x=coords2$x2,y=coords2$y2)
#'   #Return coords2 rotated closest to coords 1. list(data.frame, error margin)
#'   output_data<- as.data.frame(cbind("id"=id2,xy,"diameter"=d2,"species"=species2))
#'   return(
#'     list(
#'       output_data,
#'       mean_distance
#'     )
#'   )
#'
#' }


#
# ## Reflection of coordinates.
# coordinate_reflection <- function(id1,x1,y1,d1,species1,id2,x2,y2,d2,species2,diameter_tol=3){
#
#
#   #Data frames
#   coords <- data.frame("id"=id1,"x"=x1,"y"=y1,"diameter"=d1,"species"=species1)
#   coords2 <- data.frame("id2"=id2,"x2"=x2,"y2"=y2,"diameter2"=d2,"species2"=species2)
#
#   coords2_reflected <- coords2 %>% mutate(x2=-x2)
#
#   reflect_value <- rotate_match_diameter(id1 = coords$id,
#                                          x1 = coords$x,
#                                          y1=coords$y,
#                                          d1 = coords$diameter,
#                                          species1 = coords$species,
#                                          id2 = coords2_reflected$id2,
#                                          x2 = coords2_reflected$x2,
#                                          y2 = coords2_reflected$y2,
#                                          d2 = coords2_reflected$diameter2,
#                                          species2 = coords2_reflected$species2,
#                                          diameter_tolerance = diameter_tol)[[2]]
#   original_value <- rotate_match_diameter(id1 = coords$id,
#                                           x1 = coords$x,
#                                           y1=coords$y,
#                                           d1 = coords$diameter,
#                                           species1 = coords$species,
#                                           id2 = coords2$id2,
#                                           x2 = coords2$x2,
#                                           y2 = coords2$y2,
#                                           d2 = coords2$diameter2,
#                                           species2 = coords2$species2,
#                                           diameter_tolerance = diameter_tol)[[2]]
#
#   #If reflect value (mean distance between matches) is lower than original_value, suggest or conduct a reflection of coords2.
#   #return also gain or loss ?
#   reflect_var <- ifelse(reflect_value<original_value,1,0)
#
#   reflect_gain <- original_value-reflect_value
#
#   return_coords <- if(reflect_var==1) coords2_reflected else coords2
#
#
#   return(
#     list(
#       "Coordinates" = return_coords,
#       "Reflection status"=reflect_var,
#       "Reflection gain"=reflect_gain
#     )
#   )
#
# }
#


