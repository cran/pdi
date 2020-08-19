#' Min-max scaling
#' @description Variable min-max scaling.
#' @param vec vector of numbers to scale
#' @examples 
#' set.seed(1234)
#' 
#' d <- runif(20,1,10)
#' 
#' minMaxScale(d)
#' @export

minMaxScale <- function(vec){
  (vec - min(vec)) / (max(vec) - min(vec))
}