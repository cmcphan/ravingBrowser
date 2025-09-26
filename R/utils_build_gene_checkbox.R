#' Build gene features checkbox input
#'
#' @description Accessory function which builds the gene feature select
#'  checkbox UI element according to different requirements.
#' 
#' @param format A string describing the type of checkbox to build. Options are
#'  'simple', for initializing with default options, 'advanced', for building
#'  the full list of options. 
#'
#' @return A named list with 2 elements - choiceNames and choiceValues, corresponding
#'  to the respective arguments required by shiny::checkboxGroupInput or its update
#'  function.
#'
#' @noRd
build_gene_checkbox <- function(format = c('simple', 'advanced')){
  choiceNames = list()
  choiceValues = list()
  
  if(format == 'simple'){
    counts = browser_data$gene_feature_counts[1:5]
  }
  else if(format == 'advanced'){
    counts = browser_data$gene_feature_counts
  }

  for(type in names(counts)){
    htmlRaw = paste0('<span>',type,'</span>',
      '<span style="color: Grey;"> (',
      counts[type],
      ')</span>')
    choiceNames = c(choiceNames, tagList(tags$div(HTML(htmlRaw))))
    choiceValues = c(choiceValues, type)
  }
  
  return(list('choiceNames'=choiceNames, 'choiceValues'=choiceValues))
}