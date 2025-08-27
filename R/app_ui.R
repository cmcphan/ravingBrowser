#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyFeedback useShinyFeedback
#' @noRd
app_ui <- function(request) {
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
          tags$h1('RAVING Browser'),
          tags$h2('What are you interested in visualizing?'),
          mod_necessary_setup_ui("necessary_setup_1"),
          uiOutput('plot_controls')
        ),
        mainPanel(
		      width=9, 
		      #mod_plot_hic_ui("plot_hic_1")
		      verbatimTextOutput('plot_pane')
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
