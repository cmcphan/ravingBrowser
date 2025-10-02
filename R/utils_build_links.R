#' Build list of links in specified format
#'
#' @description Given a set of IDs, builds a list of links in HTML that can be
#'  passed to a render function for display.  
#' 
#' @param ids List of IDs for the relevant service, from which links are built
#' @param service Which service to build the links to. Can be 'uniprot',
#'  'omim' or 'hgnc'.
#'
#' @return Text marked as HTML with <a> tags corresponding to the input specifications
#' 
#' @importFrom shiny tags tagList
#'
#' @noRd
build_links <- function(ids, service){
  if(service == 'uniprot'){
    link = function(id){
      paste0('https://www.uniprot.org/uniprotkb/',id,'/entry')
    }
  }
  else if(service == 'omim'){
    link = function(id){
      paste0('https://www.omim.org/entry/',id)
    }
  }
  else if(service == 'hgnc'){
    link = function(id){
      paste0('https://www.genenames.org/data/gene-symbol-report/#!/hgnc_id/HGNC:',id)
    }
  }
  else{stop('Service must be one of "uniprot", "omim" or "hgnc"')}

  links = NULL
  for(id in unlist(strsplit(ids, '; '))){
    if(is.na(id)){
      next
    }
    if(is.null(links)){
      links = tags$a(href=link(id), target='_blank', id)
    }
    else{
      links = c(tagList(links), tagList(tags$a(href=link(id), target='_blank', id)))
    }
  }
  return(links)
}