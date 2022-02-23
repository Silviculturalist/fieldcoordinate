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
#' @examples
#'coords <- data.frame(id1,x1,y1,d1,species1)
#'names(coords) <- c("id","x","y","diameter","species")
#'coords %>% as.coord.data.frame(id = id,x = x,y = y,diameter,species)
as.coord.data.frame <- function(.data,id,x,y,...){
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
#' @examples
#' coords <- data.frame(id1,x1,y1,d1,species1)
#'names(coords) <- c("id","x","y","diameter","species")
#'coords %>% as.coord.data.frame(id = id,x = x,y = y,diameter,species) %>% filter_radius()
filter_radius.coord.data.frame <- function(.data,x=0,y=0,radius=10){
   .data %>% mutate(distance=distance_2d(x1 = .data$x,y1 = .data$y,x2 = {{x}},y2 = {{y}})) %>% filter(distance<=radius)%>% select(-distance)
}

#' @export
filter_radius <- function(.data,x=0,y=0,radius=10) UseMethod("filter_radius")


#' Rotation of coord.data.frame
#' @description Rotates coordinates in a coord.data.frame around the origin.
#' @param rotation Angle of rotation, radians. e.g. [fieldcoordinate::degree_to_radian()]
#' @return A coord.data.frame
#' @export
coordinate_rotation.coord.data.frame <- function(data,rotation){
  return(
  data %>% mutate(x=(cos(rotation) * (x) + sin(rotation) * (y)),
                   y=(cos(rotation) * (y) - sin(rotation) * (x))
                   )
  )

}

#' @export
coordinate_rotation <- function(data,rotation){ UseMethod("coordinate_rotation")}



#' Find rotation 0-360 which minimises the mean distance between matched trees.
#' @param rotation Rotation to exert on coordinate (x,y) in .data, in radians.
#' @return Mean distance between coordinates and their respective matches.
rotation_to_distance.coord.data.frame <- function(data,rotation=0){
  data %>% coordinate_rotation(rotation=rotation) %>%
    mutate(distance=distance_2d(x1=x,y1=y,x2=x.2,y2=y.2)) %>%
    summarise(mean(distance))
}

#'@export
rotation_to_distance <- function(data,rotation){ UseMethod("rotation_to_distance")}

#' Convert Cartesian coord.data.frame to Polar coordinate system.
#' @param .data A coord.data.frame with attribute 'coord.sys' set to 'cartesian'.
#' @return A coord.data.frame with attribute 'coord.sys' set to 'polar'.
#' @export

cartesian_to_polar_coordinate.coord.data.frame <- function(data){
  stopifnot("x" %in% colnames(data) & "y" %in% colnames(data))

  data %>% mutate(angle=atan2(y=data$y,x=data$x),
                   distance=sqrt((x)^2 + (y)^2)) %>% select(-c(x,y))

}

#'@export
cartesian_to_polar_coordinate <- function(x){ UseMethod("cartesian_to_polar_coordinate")}

#'Convert a Polar coord.data.frame to Cartesian coordinate system.
#'@param .data A coord.data.frame with attribute 'coord.sys' set to 'polar'.
#'@return A coord.data.frame with attribute 'coord.sys' set to 'cartesian'.

polar_to_cartesian_coordinate.coord.data.frame <- function(data){

  stopifnot("distance" %in% colnames(data) & "angle" %in% colnames(data))

  data %>% mutate(
  x = distance*cos(angle),
  y= distance*sin(angle)
  ) %>% select(-c(distance,angle))

}

#' @export
polar_to_cartesian_coordinate <- function(x) UseMethod("polar_to_cartesian_coordinate")



#' Rotate to best match points between two coord.data.frames
#' @param .data A coord.data.frame which to rotate.
#' @param coord.df2 Data frame to which to rotate to.
#' @param diameter_tolerance Tolerance for an acceptable point match.
#' @return A list, containing a coord.data.frame with the rotated values; the minimising angle and the mean distance of the result rotation and coord.df2.
#' @export
match_rotate.coord.data.frame <- function(data,coord.df2,diameter_tolerance=3){
  #enforce class.
  stopifnot("coord.data.frame" %in% class(data)  & "coord.data.frame" %in% class(coord.df2))

  #Check required match columns are present in both coord.data.frames
  if(!all(c("diameter","species") %in% colnames(data))) stop("Required columns for matching are not present in data")
  if(!all(c("diameter","species") %in% colnames(coord.df2))) stop("Required columns for matching are not present in coord.df2")

  #Empty matches data.frame
  #add suffix to column names of coord.df2
  neighbouring <- as.data.frame(matrix(
    nrow = 0,
    ncol = (ncol(data) + ncol(coord.df2)),
    dimnames = list(c(), c(
      colnames(data), paste0(colnames(coord.df2), ".2")
    ))
  ))

  #For every row in the first data frame, find all acceptable matches.
  for(i in 1:nrow(data)){
    point_species <- data[i,"species"]
    point_diameter <- data[i,"diameter"]
    point_x <- data[i,"x"]
    point_y <- data[i,"y"]

    neighbours <- coord.df2 %>% filter(species==point_species,
                                       diameter<=point_diameter+diameter_tolerance,
                                       diameter>=point_diameter-diameter_tolerance,
                                       !(id %in% neighbouring[,"id"])
                                       ) %>%
      mutate(distance=distance_2d(x1 =point_x,y1 = point_y,x2 = x,y2 = y)) %>%
      slice_min(order_by = distance,with_ties = FALSE) %>% select(-distance)

    #add to the output table.
    #Need to unname slice?
    neighbouring[i,] <- cbind(data[i,],neighbours)

  }

  class(neighbouring) <- c("coord.data.frame","data.frame")



  #Which rotation minimises the mean distance between matched points?
  min_angle_f <- function(rotate_value,data){
    rotation_to_distance(data=data,rotation=degree_to_radian(rotate_value))[[1]]
  }
  optim_result <- optimise(min_angle_f,lower = 0,upper = 360,data=neighbouring)

  #Minimising angle
  minimum_angle <- optim_result[[1]]

  #What is the resultant mean distance between matched points?
  mean_distance <- optim_result[[2]]

  #Return rotated.
  return(
    list(
      coord.df= coordinate_rotation(data,rotation = minimum_angle),
      "minimum_angle"=minimum_angle,
      "mean_distance"=mean_distance
    )
  )

}

#' @export
match_rotate <- function(data,coord.df2,diameter_tolerance=3){ UseMethod("match_rotate")}



# Plotting a coord.data.frame
#'@export
plot.coord.data.frame <- function(data,radius=10){
 data %>% ggplot2::ggplot(aes(x=x,y=y)) +
    ggplot2::geom_point(size=(data$diameter)/100,color=data$species)+
    ggplot2::coord_fixed()+
    ggforce::geom_circle(aes(x0=0,y0=0,r=radius),inherit.aes=FALSE,size=1/10)
}


#' Reflect match
#'
#'@export
match_reflect.coord.data.frame <- function(data,coord.df2,diameter_tolerance=3){
  reflecteddata <- data %>% mutate(x=-x)
  normal_value <- match_rotate(data={data},coord.df2 = {coord.df2},diameter_tolerance = {diameter_tolerance})[[3]]
  reflect_value <- match_rotate(data=reflecteddata,coord.df2 = {coord.df2},diameter_tolerance = {diameter_tolerance})[[3]]

  reflect_gain <- normal_value-reflect_value

  return_coords <- if(reflect_value<normal_value) reflectdata else data

  return(
    list(
      "Coordinates" = return_coords,
      "Reflection status"=(reflect_value<normal_value),
      "Reflection gain"=reflect_gain
    )
  )


}

#'@export
match_reflect <- function(data,coord.df2,diameter_tolerance=3){UseMethod("match_reflect")}


#'@export
produce_gauss_matrix.coord.data.frame <- function(data,point_strength=diameter,position_error=0.5,radius=10,resolution=0.2){

  point_strength_vector <- data %>% select({{point_strength}})

  value_matrix <-  data.frame(x=seq(-radius,radius,resolution)) %>% merge(data.frame(y=seq(-radius,radius,resolution))) %>% mutate(value=0)

  value_matrix2 <- value_matrix

  for(i in 1:nrow(data)){
    x0 <- data[i,"x"]
    y0 <- data[i,"y"]
    point_strength_list <- data %>% select(!!{point_strength})
    point_strength_list <- point_strength_list[i,]

    value_matrix2 <- value_matrix2 %>% mutate(value2=round(point_strength_list*exp(-((((x-x0)^2)/(2*position_error^2))+((y-y0)^2)/(2*position_error^2))),digits = 3))

    value_matrix2 <- value_matrix2 %>% rowwise() %>% mutate(value= max(value,value2)) %>% select(-value2) %>% ungroup()
  }

  class(value_matrix2) <- c("gauss.coord.data.frame","data.frame")
  attr(value_matrix2,'res') <- resolution
  attr(value_matrix2,'radius') <- radius

  return(
    value_matrix2
  )

}

#'Convert a coord.data.frame to a matrix with points represented as a gaussian process.
#'@param data A coord.data.frame
#'@param point_strength An attribute to be used as the amplitude of the gaussian process.
#'@param position_error Standard deviation of gaussian process.
#'@param radius Radius of plot.
#'@param resolution Resolution of output matrix.
#'@export
#'@return A gauss.coord.data.frame containing columns: x,y,value.
produce_gauss_matrix <- function(data,point_strength,position_error=0.5,radius=10,resolution=0.2){UseMethod("produce_gauss_matrix")}


#'Plot a gauss.coord.data.frame
#'@export
plot.gauss.coord.data.frame <- function(x){
  x %>% ggplot2::ggplot()+ggplot2::geom_raster(ggplot2::aes(x=x,y=y,fill=value))+ggplot2::theme_void()+ggplot2::coord_fixed()+ggplot2::theme(legend.position = 0,
                                                                                       plot.margin = unit(c(0,0,0,0),"mm"))
}


#'@export
save_gauss.gauss.coord.data.frame <- function(x,filename){
  stopifnot(is.character(filename))
  if(!grepl(filename,pattern='.png$')) filename <- str_glue(filename,'.png')
  plot1 <- x %>% ggplot2::ggplot()+
    ggplot2::geom_raster(ggplot2::aes(x=x,y=y,fill=value))+
    ggplot2::coord_fixed()+
    cowplot::theme_nothing()+
    ggplot2::scale_x_continuous(expand=c(0,0)) +
    ggplot2::scale_y_continuous(expand=c(0,0)) +
    ggplot2::labs(x = NULL, y = NULL)
  png(filename={filename},width=attr(x,'radius')/attr(x,'res'),height = attr(x,'radius')/attr(x,'res'),units = 'px')
  print(plot1)
  dev.off()
}

#'Save a gauss.coord.data.frame as a .png in the working directory.
#'@param x A gauss.coord.data.frame
#'@param filename A filename, including extension.
#'@export
save_gauss <- function(x,filename){UseMethod("save_gauss")}


#' Translate a coordinate system.
#' @param x Points to be translated right.
#' @param y Points to be translated up.
#' @param coord.data.frame A coord.data.frame.
#' @return A coord.data.frame
#' @export

translate_coordinates.coord.data.frame <- function(coord.data.frame,x,y){
  coord.data.frame[,"x"] <- coord.data.frame[,"x"]+x
  coord.data.frame[,"y"] <- coord.data.frame[,"y"]+y
  return(
    coord.data.frame
  )
}

#'@export
translate_coordinates <- function(coord.data.frame,x,y){UseMethod("translate_coordinates")}



#' Apply a transform from NiftyReg on a coordinate.
#' @param coord.data.frame A coord.data.frame containing the coordinates to be
#' transformed.
#' @param affine_matrix A NiftyReg affine matrix. Class 'affine'.
#' @details OBSERVE, this only finds translation and skew.
#' @return A coord.data.frame
#' @export
coordinate_transform.coord.data.frame <- function(coord.data.frame, affine_matrix){

  yaw <- RNiftyReg::decomposeAffine(affine_matrix)[["angles"]][["yaw"]]
  translate_x <- RNiftyReg::decomposeAffine(affine_matrix)[["translation"]][["x"]]
  translate_y <- RNiftyReg::decomposeAffine(affine_matrix)[["translation"]][["y"]]

  #translation
  coord.data.frame2<- translate_coordinates(coord.data.frame = {{coord.data.frame}},x = translate_x,y = translate_y)

  #skew
  coord.data.frame2 <- coordinate_rotation(coord.data.frame2,rotation = yaw)

  return(
    coord.data.frame2
  )

}

#'@export
coordinate_transform <- function(coord.data.frame, affine_matrix) UseMethod("coordinate_transform")


#' Reflect a coordinate system.
#' @param coord.data.frame A coord.data.frame
#' @param x Logical. (Default: TRUE) Should the coordinates be reflected across the x-axis?
#' @param y Logical. (Default: FALSE) Should the coordinates be reflected across the y-axis?
#' @return A coord.data.frame
#' @export
coordinate_reflect.coord.data.frame <- function(coord.data.frame,x=TRUE,y=FALSE){
  stopifnot(is.logical(x) & is.logical(y))

  if(isTRUE(x)){
    coord.data.frame$x <- (coord.data.frame$x)*-1
  }

  if(isTRUE(y)){
    coord.data.frame$y <- (coord.data.frame$y)*-1
  }

  return(
    coord.data.frame
  )
}

#'@export
coordinate_reflect <- function(coord.data.frame,x=TRUE,y=FALSE) UseMethod("coordinate_reflect")


#'@param data A data.frame
#'@param stand_id A character with the variable name which identifies stands.
#'@param Year A character with the variable name denoting different years.
#'@param save TRUE (default) / FALSE. Saves the transformed coordinates.
#'@param filepath Folder in which to store matches (creates a subfolder for each stand)
#'@export
#'@examples
#'tree_coords2_split <- tree_coords2 %>% group_by(standnumber,standnr2) %>% group_split(.keep = TRUE)
#'lapply(tree_coords2_split,FUN = match_trees,stand_id = 'standnumber',Year = 'Year')

match_trees <- function(data, stand_id, Year='Year', filepath="matched_trees/", save=TRUE){
  #Check to make sure there are years to match between.
  #Include NA statement to avoid zero correlation matches - e.g. trees without Diameter.
  if(nrow(data %>% filter(!is.na(Diameter)) %>% group_nest(Year))<=1) return(NULL)


  stand_id <- data[[1,stand_id]]

  #number of years to index
  max_index <- length(data %>% group_nest(Year))

  #split data.frame by years.
  splits <- split(data,data[,{Year}]) %>% lapply(.,FUN = as.coord.data.frame,id=TreeID,x=GPSNorth,y=GPSEast,Species,Diameter,Height)

  #Check filepath exists, create if it doesn't.
  if(!dir.exists(filepath)) dir.create(filepath)

  #Create filepath for stand number.
  if(!dir.exists(str_glue(filepath,stand_id))) dir.create(str_glue(filepath,stand_id))

  #Save a representation for maximum year.
  splits[[max_index]] %>% as.coord.data.frame(id=id,x=x,y=y,Species,Diameter,Height) %>% produce_gauss_matrix(point_strength = 'Diameter',position_error = 0.5,radius=10,resolution=0.1) %>%  save_gauss(filename = str_glue(filepath,stand_id,"/",stand_id,"_year_",names(splits[max_index])))

  #read representation
  gauss_max <- png::readPNG(paste0(str_glue(filepath,stand_id,"/",stand_id,"_year_",names(splits[max_index]),".png")))

  #try to shift coordinates to latest year (max_index)
  for(i in 1:(max_index-1)){
    #save gaussian representation for year i.
    splits[[i]] %>% as.coord.data.frame(id=id,x=x,y=y,Species,Diameter,Height) %>%  produce_gauss_matrix(point_strength = 'Diameter',position_error = 0.5,radius=10,resolution=0.1) %>%  save_gauss(filename = str_glue(filepath,stand_id,"/",stand_id,"_year_",names(splits[i])))

    #find appropriate transform
    gauss_1 <- png::readPNG(source = paste0(str_glue(filepath,stand_id,"/",stand_id,"_year_",names(splits[i]),".png")))

    affine_transformation <- RNiftyReg::niftyreg.linear(source = gauss_1,target = gauss_max)

    #undertake transform
    splits[[i]]<- splits[[i]] %>% coordinate_transform(affine_matrix = RNiftyReg::forward(affine_transformation))

    #save out.. image
    png::writePNG(image=affine_transformation$image,target = paste0(filepath,stand_id,"/",stand_id,"_year_",names(splits[i]),"_project_to_",names(splits[max_index]),".png"))

    #save out.. transformation
    RNiftyReg::saveTransform(RNiftyReg::forward(affine_transformation),file=paste0(filepath,stand_id,"/",stand_id,"_year_",names(splits[i]),"_project_to_",names(splits[max_index]),".rds"))

  }

  if(isTRUE(save)) splits %>% bind_rows(.id = "Year") %>% write.csv(paste0(filepath,stand_id,"/",stand_id,".csv"),row.names = FALSE)
  print(stand_id)
}
