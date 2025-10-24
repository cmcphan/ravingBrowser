#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyFeedback useShinyFeedback
#' @importFrom shinycssloaders withSpinner
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    # This multicol CSS class comes from
    #  https://stackoverflow.com/questions/29738975/
    #  how-to-align-a-group-of-checkboxgroupinput-in-r-shiny
    tags$head(tags$style(HTML("
      .multicol { 
        -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
        -moz-column-count: 2;    /* Firefox */ 
        column-count: 2; 
        -moz-column-fill: auto;
        -column-fill: auto;
      } ")) 
    ),
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
