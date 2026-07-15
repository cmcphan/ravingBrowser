#' Zoom in or out on a region
#'
#' @description Utility function for calculating new region limits using
#'  the zoom toolbar buttons
#'
#' @param chr,start,end Region configuration for the zoom to be applied to
#' @param zoom Which direction to zoom in - may be "in" or "out"
#'
#' @return A named list of updated region limits
#'
#' @noRd
zoom <- function(chr, start, end, zoom){
  current_max = browser_data$hic_info[chr, "length"]
  width = end - start

  if(zoom == "in"){
    start = floor(start + (width*0.25))
    end = ceiling(end - (width*0.25))
  }
  else if(zoom == "out"){
    start = floor(start - (width*0.5))
    end = ceiling(end + (width*0.5))
  }
  else{
    stop("Zoom must be one of \"in\" or \"out\"")
  }
  
  if(start < 1){
    start = 1
  }
  if(end > current_max){
    end = current_max
  }

  width = end - start

  return(list(chr = chr, start = start, end = end, width = width))
}
