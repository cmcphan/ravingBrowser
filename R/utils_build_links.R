#' Build list of links in specified format
#'
#' @description Given a set of IDs, builds a list of links in HTML that can be
#'  passed to a render function for display.  
#' 
#' @param ids List of IDs for the relevant service, from which links are built
#' @param service Which service to build the links to. Can be "ucsc", "ensembl", 
#'  "uniprot", "omim", "hgnc", "ncbi" or "genecards".
#' @param chr,start,end Genomic coordinates for the given gene entry. Only required
#'  for the "ucsc" option. Used to provide an alternative for cases where an ID
#'  is missing.
#'
#' @return Text marked as HTML with <a> tags corresponding to the input specifications
#' 
#' @importFrom shiny tags tagList
#'
#' @noRd
build_links <- function(ids, service, chr=NULL, start=NULL, end=NULL){
  if(service == "ucsc"){
    link = function(id){
      paste0("https://genome.ucsc.edu/cgi-bin/hgSearch?search=",id)
    }
  }
  else if(service == "ensembl"){
    link = function(id){
      paste0("https://www.ensembl.org/Homo_sapiens/Gene/Summary?db=core;g=",id)
    }
  }
  else if(service == "uniprot"){
    link = function(id){
      paste0("https://www.uniprot.org/uniprotkb/",id,"/entry")
    }
  }
  else if(service == "omim"){
    link = function(id){
      paste0("https://www.omim.org/entry/",id)
    }
  }
  else if(service == "hgnc"){
    link = function(id){
      paste0("https://www.genenames.org/data/gene-symbol-report/#!/hgnc_id/HGNC:",id)
    }
  }
  else if(service == "ncbi"){
    link = function(id){
      paste0("https://www.ncbi.nlm.nih.gov/gene/",id)
    }
  }
  else if(service == "genecards"){
    link = function(id){
      paste0("https://www.genecards.org/cgi-bin/carddisp.pl?gene=",id)
    }
  }
  else{stop("Service must be one of \"ucsc\", \"ensembl\", \"uniprot\", 
    \"omim\", \"hgnc\", \"ncbi\" or \"genecards\"")}

  links = NULL
  if(!shiny::isTruthy(ids)){
    if(service == "ucsc"){
      if(is.null(chr) | is.null(start) | is.null(end)){
        stop("UCSC option requires genomic coordinates be provided when ID is missing")
      }
      else{
        link = paste0("https://genome.ucsc.edu/cgi-bin/hgTracks?db=hg38&position=chr",
          chr,"%3A",start,"-",end)
        return(tags$a(href=link, target="_blank", paste0("chr",chr,":",start,"-",end)))
      }
    } 
    else{
      return("None")
    }
  }
  for(id in unlist(strsplit(ids, "; "))){
    if(is.na(id)){
      next
    }
    if(is.null(links)){
      links = tags$a(href=link(id), target="_blank", id)
    }
    else{
      links = c(tagList(links), tagList(tags$a(href=link(id), target="_blank", id)))
    }
  }
  return(links)
}