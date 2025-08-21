#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    useShinyjs(),
    # UI logic
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width=3,
          tags$h1('RAVING Browser'),
          tags$h2('What are you interested in visualizing?'),
          mod_necessary_setup_ui("necessary_setup_1")
        ),
        mainPanel(
		      width=9, 
		      tags$h1('Choices made'),
		      tags$h2('Plot types:'),
		      textOutput(outputId = 'plot_types'),
		      tags$h2('Chromosome:'),
		      textOutput(outputId = 'region_chr'),
		      tags$h2('Region limits slider:'),
		      textOutput(outputId = 'region_size_slider'),
		      tags$h2('Region limits direct input MIN:'),
		      textOutput(outputId = 'region_size_direct_min'),
		      tags$h2('Region limits direct input MAX:'),
		      textOutput(outputId = 'region_size_direct_max'),
		      tags$h2('Switch button presses:'),
		      textOutput(outputId = 'toggle_region_size')
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
