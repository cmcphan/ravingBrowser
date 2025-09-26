#' Extract region configuration from UI
#'
#' @description Finds the current region configuration information from
#'  the user inputs in [mod_necessary_setup_ui()]. Because the input list is
#'  made up of reactive values, this function can only be called inside of an
#'  active reactive context (e.g. an observeEvent).
#'
#' @param region Output from mod_necessary_setup_ui$region_config - 
#'  a list of reactive functions
#'
#' @return Returns a list of the relevant values to be passed to other module functions
#'
#' @noRd
get_region_config <- function(region) {
  # Build region config list - check which region bounds to use
  region_config = list(
    region_chr = region()$region_chr(),
    region_start = region()$region_start(),
    region_end = region()$region_end(),
    region_width = region()$region_width()
  )
  return(region_config)
}
