#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#' @param onStart A function that will be called before the app is actually run.
#' 		For this app we choose to set an option that prevents integers from being returned in scientific notation.
#' @param options Named options that should be passed to the `runApp` call
#'    (these can be any of the following: "port", "launch.browser", "host", "quiet",
#'    "display.mode" and "test.mode"). For this app we set launch.browser to FALSE to make sure the app 
#' 		runs without needing the default browser to be set.
#'
#' @export
#' @import ggplot2
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = function(){
    options(scipen=10)
    # source("data-raw/browser_data.R") Re-enable if data needs to be rebuilt
    library(ggplot2)
  },
  options = list(launch.browser=FALSE),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
	
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
