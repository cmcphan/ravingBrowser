#' Correctly place UI control elements
#'
#' @description Function to determine where to place new UI control elements
#'  using insertUI().
#'
#' @return A string to be used as the selector parameter in the insertUI call.
#'
#' @noRd
place_ui <- function(element, selected){
  idx = match(element, selected)
  if(idx == 1){
    return(paste0('#necessary_setup_controls'))
  }
  else{
    return(paste0('#', selected[idx-1], '_controls'))
  }
}
