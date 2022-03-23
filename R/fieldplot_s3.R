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
  #NB ellipsis before args with default.
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
plot.coord.data.frame <- function(data,radius=10,size,color){
 data %>% ggplot2::ggplot(aes(x=x,y=y)) +
    ggplot2::geom_point(data=data,aes(size={{size}},color={{color}}))+
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



#Gauss array Rotate Left, Rotate Left, flip vertical.
#Byrow ... y inverted?
#'@export
produce_gauss_array.default <- function(data,point_strength,position_error=0.5,radius=10,res=0.1){

  point_strength_vector <- data %>% select({{point_strength}})

  value_matrix <-  data.frame(x=seq((-radius+{res}/2),(radius-{res}/2),{res})) %>% merge(data.frame(y=seq((radius-{res}/2),(-radius+{res}/2),-{res}))) %>% mutate(value=0)

  value_matrix2 <- value_matrix

  for(i in 1:nrow(data)){
    x0 <- data[i,"x"]
    y0 <- data[i,"y"]
    point_strength_list <- data %>% select(!!{point_strength})
    point_strength_list <- point_strength_list[i,]
    value_matrix2 <- value_matrix2 %>% mutate(value2=round(point_strength_list*exp(-((((x-x0)^2)/(2*position_error^2))+((y-y0)^2)/(2*position_error^2))),digits = 3))
    value_matrix2 <- value_matrix2 %>% rowwise() %>% mutate(value= max(value,value2,na.rm=TRUE)) %>% select(-value2) %>% ungroup()
  }

  value_matrix2 <- matrix(value_matrix2$value,ncol=2*radius/{res},nrow=2*radius/{res},dimnames=NULL,byrow=TRUE)

  class(value_matrix2) <- c("gauss.coord.array","matrix")
  attr(value_matrix2,'res') <- {res}
  attr(value_matrix2,'radius') <- radius

  return(
    value_matrix2
  )

}



#'Convert a coord.data.frame to a matrix with points represented as a gaussian process.
#'@param data A coord.data.frame
#'@param point_strength An attribute to be used as the amplitude of the gaussian process. Typically 'Diameter'.
#'@param position_error Standard deviation of gaussian process.
#'@param radius Radius of plot.
#'@param res Resolution of output matrix. Do not recommend higher resolution (lower values) than 0.1. Significant computing time.
#'@export
#'@return A gauss.coord.array containing columns: x,y,value.
produce_gauss_array <- function(data,point_strength,position_error=0.5,radius=10,res=0.1){UseMethod("produce_gauss_array")}


#'Plot a gauss.coord.array
#'@export
plot.gauss.coord.array <- function(x){

  x_range <- range(x[is.finite(x)])

  #Image has odd conventions... first flip matrix vertically and then transpose.
  x2 <- apply(x,2,rev)
  x2 <- t(x2)

  #Plot matrix as raster in greyscale.
  par(mai=c(0,0,0,0),xaxt="n",yaxt="n")
  image(x2,asp=1,useRaster=TRUE,col=grey(0:255/255),zlim=sort(x_range))

}


#'Save a gauss.coord.array as a png without any borders.
#'@export
save_gauss.default <- function(x,filename){
  stopifnot(is.character(filename))
  if(!grepl(filename,pattern='.png$')) filename <- stringr::str_glue(filename,'.png')

  #Z (color range)
  x_range <- range(x[is.finite(x)])


  #Image has odd conventions... first flip matrix vertically and then transpose.
  x2 <- apply(x,2,rev)
  x2 <- t(x2)

  #Create graphics device
  png(filename,width = dim(x),height=dim(x),units = 'px',res=1)

  #No margins, grey background
  par(mai=c(0,0,0,0),bg='grey70')

  #Plot matrix as raster in greyscale.
  image(x2,asp=1,useRaster=TRUE,col=grey(0:255/255),zlim=sort(x_range))

  #Close graphics device.
  dev.off()

}

#'Save a gauss.coord.array as a .png in the working directory.
#'@param x A gauss.coord.array
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


#' Scale a coordinate system
#' @param coord.data.frame A coord.data.frame
#' @param Sx Scaling factor for x-axis.
#' @param Sy Scaling factor for y-axis.
#' @return A coord.data.frame
#' @export
scale_coordinates.coord.data.frame <- function(coord.data.frame,Sx,Sy){

  coord.data.frame[,"x"] <- coord.data.frame[,"x"]*Sx
  coord.data.frame[,"y"] <- coord.data.frame[,"y"]*Sy

  return(
    coord.data.frame
  )
}

#'@export
scale_coordinates <- function(coord.data.frame,Sx,Sy){UseMethod("scale_coordinates")}


#' #' Shear a coordinate system
#' #' @param coord.data.frame A coord.data.frame
#' #' @param Qx Shear factor for X-axis.
#' #' @param Qy Shear factor for Y-axis.
#' #' @return A coord.data.frame
#' shear_coordinates <- function(coord.data.frame,Qx=0,Qy=0){UseMethod("shear_coordinates")}
#'
#' shear_coordinates.coord.data.frame <- function(coord.data.frame,Qx=0,Qy=0){
#'   coord.data.frame[,"x"] <- coord.data.frame[,"x"] + coord.data.frame[,"y"]*Qx
#'   coord.data.frame[,"y"] <- coord.data.frame[,"y"] + coord.data.frame[,"x"]*Qy
#'
#'
#'   return(
#'     coord.data.frame
#'   )
#' }


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

#' Match trees between years.
#'@param data A data.frame
#'@param stand_id A character with the variable name which identifies stands.
#'@param Year A character with the variable name denoting different years.
#'@param save TRUE (default) / FALSE. Saves the transformed coordinates.
#'@param filepath Folder in which to store matches (creates a subfolder for each stand)
#'@examples
#'tree_coords2_split <- tree_coords2 %>% group_by(standnumber,standnr2) %>% group_split(.keep = TRUE)
#'lapply(tree_coords2_split,FUN = match_trees,stand_id = 'standnumber',Year = 'Year')

match_trees <- function(data, stand_id, Year='Year', filepath="matched_trees/", save=TRUE,skipahead=TRUE){
  #Check to make sure there are years to match between.
  #Include NA statement to avoid zero correlation matches - e.g. trees without Diameter.
  if(nrow(data %>% filter(!is.na(Diameter)) %>% group_nest(Year))<=1) return(NULL)
  stand_id <- data[[1,stand_id]]
  if(skipahead & stand_id %in% dir(filepath)) return(print(stand_id))

  #number of years to index
  max_index <- length(data %>% group_nest(Year))

  #split data.frame by years.
  splits <- split(data,data[,{Year}]) %>% lapply(.,FUN = as.coord.data.frame,id=TreeID,x=GPSNorth,y=GPSEast,Species,Diameter,Height)

  #Check filepath exists, create if it doesn't.
  if(!dir.exists(filepath)) dir.create(filepath)

  #Create filepath for stand number.
  if(!dir.exists(str_glue(filepath,stand_id))) dir.create(str_glue(filepath,stand_id))

  #Save a representation for maximum year.
  splits[[max_index]] %>% as.coord.data.frame(id=id,x=x,y=y,Species,Diameter,Height) %>% produce_gauss_array(point_strength = 'Diameter',position_error = 0.5,radius=10,res=0.1) %>%  save_gauss(filename = str_glue(filepath,stand_id,"/",stand_id,"_year_",names(splits[max_index])))

  #read representation
  gauss_max <- png::readPNG(paste0(str_glue(filepath,stand_id,"/",stand_id,"_year_",names(splits[max_index]),".png")))

  #try to shift coordinates to latest year (max_index)
  for(i in 1:(max_index-1)){
    #save gaussian representation for year i.
    splits[[i]] %>% as.coord.data.frame(id=id,x=x,y=y,Species,Diameter,Height) %>%  produce_gauss_array(point_strength = 'Diameter',position_error = 0.5,radius=10,res=0.1) %>%  save_gauss(filename = str_glue(filepath,stand_id,"/",stand_id,"_year_",names(splits[i])))

    #find appropriate transform
    gauss_1 <- png::readPNG(source = paste0(str_glue(filepath,stand_id,"/",stand_id,"_year_",names(splits[i]),".png")))

    affine_transformation <- RNiftyReg::niftyreg.linear(source = gauss_1,target = gauss_max)

    #undertake transform
    splits[[i]]<- splits[[i]] %>% coordinate_transform(transform = RNiftyReg::forward(affine_transformation))

    #save out.. image
    png::writePNG(image=affine_transformation$image,target = paste0(filepath,stand_id,"/",stand_id,"_year_",names(splits[i]),"_project_to_",names(splits[max_index]),".png"))

    #save out.. transformation
    RNiftyReg::saveTransform(RNiftyReg::forward(affine_transformation),file=paste0(filepath,stand_id,"/",stand_id,"_year_",names(splits[i]),"_project_to_",names(splits[max_index]),".rds"))

  }

  if(isTRUE(save)) splits %>% bind_rows(.id = "Year") %>% write.csv(paste0(filepath,stand_id,"/",stand_id,".csv"),row.names = FALSE)
  print(stand_id)
}


#' Find the most similar image in a folder.
#' @param img_list A list of files contained in path, e.g. dir(path, full.names=FALSE)
#' @param path Path to the folder containing images to be evaluated.
#' @param img An internal Image or array (load with [png::readPNG]) which should
#'  be transformed to the images in img_list
#' @return A list containing i) The transformed source image, ii) The target image
#' which returned the highest similarity, iii) A list of the similarity scores
#' for all evaluated images.
most_similar_img <- function(img, img_list,path='matched_trees/mismatches/'){


  similarity_list <- list()
  for(i in 1:length(img_list)){
    assign('img_to_test',png::readPNG(paste0(path,{img_list[i]})))
    transformed_source <- RNiftyReg::niftyreg.linear(img,img_to_test)
    similarityvalue <- RNiftyReg::similarity(transformed_source$image,img_to_test)
    similarity_list[{img_list[i]}] <- similarityvalue
  }
  max_transform <- png::readPNG(paste0(path,names(which.max(similarity_list))))
  source_transform <- RNiftyReg::niftyreg.linear(img,max_transform)$image

  return(
    list(
      transformed_source = source_transform,
      #transforms target image to a 'niftiImage' so output doesn't clutter.
      target_image = RNifti::asNifti(png::readPNG(paste0(path,names(which.max(similarity_list))))),
      similarity_out = sort(unlist(similarity_list),decreasing=TRUE)
    )
  )
}


#' Apply the transform between two images to a coord.data.frame
#' @param data Coord.data.frame with points to be transformed. Must contain vars: 'Diameter','Species', 'Year', 'x','y'.
#' @param source_year Coordinate year to transform. From which to produce source image.
#' @param target_year Target coordinate year from which to produce target image.
#' @param radius Default (10 m radius plots) Plot radius, to calculate coordinate indices.
#' @param res Default (0.1) Plot resolution, to calculate coordinate indices.
#' @return A coord.data.frame
#' @name coordinate_transform1
#' @export
coordinate_transform <- function(data,target_year,source_year,radius=10,res=0.1){

  stopifnot("coord.data.frame" %in% class(data))

  # This matrix contains information about the resolution (diagonal) and the offset/origin (last column)
  xform <- matrix(c(res,    0, 0,  -radius,
                    0, -res, 0,   radius,
                    0,    0, 1,   0,
                    0,    0, 0,   1), 4, 4, byrow=TRUE)


  #Target and source data
  target_data <- data %>% filter(Year==target_year)
  source_data <- data %>% filter(Year==source_year)

  #create target and source arrays
  target <- target_data %>% produce_gauss_array(radius={radius},point_strength='Diameter',res={res})
  source <- source_data %>% produce_gauss_array(radius={radius},point_strength='Diameter',res={res})

  #set attributes to array for RNiftyReg image registering to work.
  RNiftyReg::pixdim(source) <- c({res},{res},1)
  RNiftyReg::sform(source) <- structure(xform,code=2L)
  RNiftyReg::pixdim(target) <- c({res},{res},1)
  RNiftyReg::sform(target) <- structure(xform,code=2L)

  #register
  reg <- RNiftyReg::niftyreg.linear(source = source,target = target,nLevels = 1,scope = 'rigid')

  # Transform the locations in three steps
  # World-to-voxel and voxel-to-world convert between indices and "real space" according to the corresponding image's xform matrix
  points <- RNifti::worldToVoxel(as.matrix(source_data[,c("x","y")]), source)
  points <- applyTransform(reverse(reg), points)
  points <- RNifti::voxelToWorld(points, target)
  result <- data.frame(cbind("x"=points[,1], "y"=points[,2], subset(source_data,select=-c(x,y))))

  out <- rbind(result,target_data) %>% as.coord.data.frame(id = id,x=x,y=y,Species,Diameter,Year)

  return(
    out
  )

}


#' A shadow image of coordinate_transform that also saves images and the transform in a filepath.
#' @description This function should be internal only.
#'
#' @param filepath A directory to create in which to store created images and transformation files.
#' @inheritParams coordinate_transform1
#' @keywords internal
coordinate_transform2 <- function(data,target_year,source_year,filepath="matched_trees",radius=10,res=0.1){
  stopifnot(data %>% distinct(standnumber) %>% nrow() == 1)
  stopifnot(all(c("id","x","y","Diameter","Species") %in% colnames(data)))

  #Get plotid for naming files.
  plotid <- (data %>% distinct(standnumber))[[1,1]]

  # This matrix contains information about the resolution (diagonal) and the offset/origin (last column)
  xform <- matrix(c(res,    0, 0,  -radius,
                    0, -res, 0,   radius,
                    0,    0, 1,   0,
                    0,    0, 0,   1), 4, 4, byrow=TRUE)


  #Target and source data
  target_data <- data %>% filter(Year==target_year)
  source_data <- data %>% filter(Year==source_year)

  #create target and source arrays
  target <- target_data %>% produce_gauss_array(radius={radius},point_strength='Diameter',res={res})
  source <- source_data %>% produce_gauss_array(radius={radius},point_strength='Diameter',res={res})

  #save images
  target %>% save_gauss(filename=paste0(filepath,"/",plotid,"/",plotid,"_",target_year,".png"))
  source %>% save_gauss(filename=paste0(filepath,"/",plotid,"/",plotid,"_",source_year,".png"))


  #set attributes to array for RNiftyReg image registering to work.
  RNiftyReg::pixdim(source) <- c({res},{res},1)
  RNiftyReg::sform(source) <- structure(xform,code=2L)
  RNiftyReg::pixdim(target) <- c({res},{res},1)
  RNiftyReg::sform(target) <- structure(xform,code=2L)

  #register
  reg <- RNiftyReg::niftyreg.linear(source = source,target = target,nLevels = 1,scope = 'rigid')

  # Transform the locations in three steps
  # World-to-voxel and voxel-to-world convert between indices and "real space" according to the corresponding image's xform matrix
  points <- RNifti::worldToVoxel(as.matrix(source_data[,c("x","y")]), source)
  points <- applyTransform(reverse(reg), points)
  points <- RNifti::voxelToWorld(points, target)
  result <- data.frame(cbind("x"=points[,1], "y"=points[,2], subset(source_data,select=-c(x,y))))

  out <- rbind(result,target_data) %>% as.coord.data.frame(id = id,x=x,y=y,Species,Diameter,Year)

  return(
    out
  )

}


#For running all stands..
#' @export
save_transformation <- function(data,source_year,target_year,filepath="matched_trees",radius=10,res=0.1){
  stopifnot(all(c("standnumber","id","x","y","Year","Diameter","Species")%in%colnames({data})))

  if(!dir.exists(filepath)) dir.create(filepath,showWarnings = FALSE) #Create results folder if not already exists.

  for(i in 1:(data %>% distinct(standnumber) %>% nrow())){

    #In case of error
    skip_to_next <- FALSE

    message(paste0(i," out of ",(data %>% distinct(standnumber) %>% nrow())))

    currentid <- (data %>% distinct(standnumber))[i,][[1]]
    dir.create(paste0(filepath,"/",currentid),showWarnings = FALSE) #Create plot results folder.

    data2 <- data %>% filter(standnumber == currentid)
    #Skip if not source year and target year are available
    if(!all(c(source_year, target_year)%in%data2$"Year")){
      message("Source or Target Year not available for plot. Jumping to next plot.")
      next()
    }

    message("Transforming coordinates...")
    #Transform coordinates with NiftyRegR and save images of source and target.
    #tryCatch  catches error and continues with next.
    tryCatch({
    reg <- data2 %>% fieldcoordinate:::coordinate_transform2(target_year = {target_year},source_year = {source_year},radius={radius},res={res},filepath="matched_trees")
    }, error=function(e){skip_to_next <<- TRUE}) #assign in a parent environment.

    if(skip_to_next) { next }

    #Save coordinates from transformed source and original target.
    write.csv(reg,row.names = FALSE,file = paste0(filepath,"/",currentid,"/",currentid,"_",source_year,"_to_",target_year,".csv"))

    message("Saving transformed image...")
    #Save image of transformed source.
    reg %>% filter(Year=={source_year}) %>% produce_gauss_array(point_strength = 'Diameter') %>% save_gauss(filename = paste0(filepath,"/",currentid,"/",currentid,"_",source_year,"_to_",target_year,".png"))


    #Print confirmation
    message(paste0("Plot ",currentid," Done."))


  }

  #message when finished.
  message("Finished all plots.")
}

