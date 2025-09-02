#' Functions for validating user inputs
#'
#' @description Determine whether the given region is valid for plotting.
#'
#' @param chr An integer denoting the selected chromosome for the
#' configured region
#' @param start,end Integers denoting the start and end coordinates (in
#'  base pairs) of the configured region
#' @return NA if the region is valid, otherwise returns an informative error message
#'  which can be used for feedback
#'
#' @noRd
validate_region <- function(chr, start, end){
  current_max = browser_data$hic_info[chr, "length"]
	if(is.na(start)){
		return('Start coordinate cannot be NA')
	}
	else if(is.na(end)){
		return('End coordinate cannot be NA')
	}
	else if(start >= end){
		return('Start coordinate must be greater than end coordinate')
	}
	else if(start > current_max-1){
		return(paste0('Start cannot be greater than ',current_max-1,' for this chromosome'))
	}
	else if(end > current_max){
		return(paste0('End cannot be greater than ',current_max,' for this chromosome'))
	}
	else if(start == end){
		return('Start coordinate cannot be the same as end coordinate')
	}
	else { return(NA) }
}

#' @description Determine whether the Hi-C plot configuration is valid.
#'
#' @param resolution An integer denoting the resolution (i.e. bin size), in base
#'  pairs, of the requested plot
#' @param normalization A string describing the normalization method for the requested
#'  plot
#' @param format A string describing the format of the requested plot
#' @return NA if the configuration is valid, otherwise returns an informative
#'  error message which can be used for feedback
#'
#' @noRd
validate_hic <- function(resolution, normalization, format){
	if(!(resolution %in% browser_data$resolutions)){
		return('Resolution not present in data - only use resolutions from the dropdown menu')
	}
	else if(!(normalization %in% browser_data$normalizations)){
		return('Normalization not present in data - only use normalizations from the dropdown menu')
	}
	else if(!(format %in% c('square', 'triangular', 'rectangular'))){
		return('Format must be one of square, triangular or rectangular')
	}
	else{ return(NA) }
}

## Add function which produces an 'error plot' - a ggplot object which is maybe just
##  some text on a blank plot canvas which can be passed to plot outputs to display an
##  error
