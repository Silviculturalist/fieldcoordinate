#S3 classes for fieldplot


#Constructor class for a coord.data.frame

#' Attempt to convert a data.frame to a coord.data.frame
#' @param .data A data.frame containing the data.
#' @param id Column containing the individual identifier of each coordinate point.
#' @param x Column containing the x-coordinate of each coordinate point.
#' @param y Column containing the y-coordinate of each coordinate point.
#' @param ... Attribute columns which should be preserved.
#' @export
as.coord.data.frame <- function(.data,id,x,y,...){
  quos <- enquos(...)
  output <- .data %>% select(id={{id}},x={{x}},y={{y}},!!!quos)
  class(output) <- c("data.frame","coord.df")
  return(
    output
  )
}

#' Filter coordinate points
#' @description Filter coordinate points by attribute, maximum radius (inclusive).
#' @param radius Maximum radius from filter point.
#' @param x x coordinate of point to filter from.
#' @param y y coordinate of point to filter from.
#' @param ... Additional filters, e.g. attributes.
radius_filter.coord.df <- function(.data,x=0,y=0,radius=10,...){
  quos <- enquos(...)
  .data %>% mutate(distance=distance_2d(x1 = .data["x"],y1 = .data["y"],x2 = x,y2 = y)) %>% filter(distance<=radius, !!!quos )
  NextMethod()
}

