#' Ensure region boundaries are valid
#'
#' @description Determine whether the given region is valid for plotting.
#'
#' @param chr An integer denoting the selected chromosome for the
#' configured region
#' @param start,end Integers denoting the start and end coordinates (in
#'  base pairs) of the configured region
#' @return TRUE if the region is valid, FALSE otherwise
#'
#' @noRd
validate_region <- function(chr, start, end){
  current_max <- browser_data$hic_info[chr, "length"]
	if(is.na(start) | is.na(end)){
		return(FALSE)
	}
	else if(start >= end){
		return(FALSE)
	}
	else if(start > current_max | end > current_max){
		return(FALSE)
	}
	else if(start == end){
		return(FALSE)
	}
	else { return(TRUE) }
}
