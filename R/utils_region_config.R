#' Extract region configuration from UI
#'
#' @description Finds the current region configuration information from
#'  the user inputs in [mod_necessary_setup_ui()]. Because the input list is
#'  made up of reactive values, this function can only be called inside of an
#'  active reactive context (e.g. an observeEvent).
#'
#' @param basic_config Output from mod_necessary_setup_ui - a list of reactive values
#'
#' @return Returns a list of the relevant values to be passed to other module functions
#'
#' @noRd
region_config <- function(basic_config) {
  # Build region config list - check which region bounds to use
  region_config = list(
    plot_types = basic_config$plot_type_select(),
    region_chr = basic_config$region_chr()
  )
  if(basic_config$toggle_region_size() %% 2 == 1){
    region_config$region_start = basic_config$region_size_direct_min()
    region_config$region_end = basic_config$region_size_direct_max()
  }
  else{
    region_config$region_start = basic_config$region_size_slider()[1]
    region_config$region_end = basic_config$region_size_slider()[2]
  }
  return(region_config)
}
