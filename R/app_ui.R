#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyFeedback useShinyFeedback
#' @importFrom shinycssloaders withSpinner
#' @import anndata
#' @import reticulate
#' @noRd
app_ui <- function(request) {
  if(!is.null(browser_data$plot_types$methylation)){
    # Workaround to make sure the methylation h5ad is loaded into the
    #  environment if present, given we can't use usethis
    file = list.files(path=paste0("data-raw/methylation"),
			pattern=paste0("\\.h5ad$"), full.names=TRUE)
    reticulate::py_require(c("anndata>=0.7.5"))
    methylation = anndata::read_h5ad(file)
    assign("methylation", methylation, envir=.GlobalEnv)
  }
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    # UI logic
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width=3,
          tags$h1("RAVING Browser"),
          mod_necessary_setup_ui("necessary_setup_1")
        ),
        mainPanel(
		      width=9,
		      plotOutput("plot_panel", height=0), # To get plot output widths
          mod_toolbar_ui("toolbar_1"),
		      mod_plot_ui("plot_1")
		    )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ravingBrowser"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
