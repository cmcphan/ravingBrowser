#' Utility functions for bigWig visualization
#'
#' @description These functions have been extracted from the larger `trackplot`
#'  codebase and modified for use in the ravingBrowser app. They perform the
#'  backend work involved in efficiently extracting required data from bigWig files.
#'
#' @seealso Source code: https://github.com/PoisonAlien/trackplot
#' 
#' @noRd

#' @description Read bigWig file column data, forms a necessary input for
#'  get_summaries()
#' 
#' @param bws Path(s) to bigWig file(s). If multiple files are to be read, paths
#'  should be provided as a character vector
#'
#' @import data.table
#' @importFrom parallel mclapply
read_coldata = function(bws = NULL, sample_names = NULL, build = "hg38",
  input_type = "bw", verbose=TRUE){
  
  if(is.null(bws)){
    stop("Please provide paths to bigWig files")
  }
  
  input_type = match.arg(arg = input_type, choices = c("bw", "peak"))
  
  if(verbose){
    message("Checking for files..")
  }
  bws = as.character(bws)
  lapply(bws, function(x){
    if(!file.exists(x)){
      stop(paste0(x, " does not exist!"))
    }
  })
  
  if(is.null(sample_names)){
    bw_sample_names = unlist(data.table::tstrsplit(x = basename(bws), split = "\\.",
      keep = 1))
  }else{
    bw_sample_names = as.character(sample_names)  
  }
  
  if(any(duplicated(bw_sample_names))){
    stop("Found duplicates. Samples names must be unique")
  }
  if(length(bw_sample_names) != length(bws)){
    stop("Please provide names for each input file")
  }
  
  coldata = data.table::data.table(bw_files = bws, bw_sample_names = bw_sample_names)
  
  attr(coldata, "refbuild") = build
  attr(coldata, "is_bw") =  input_type ==  "bw"
  if(verbose){
    message("Input type: ", input_type)
    message("Ref genome: ", build)
    message("OK!")
  }
  coldata
}

gen_windows = function(chr = NA, start, end, window_size = 50, op_dir = getwd()){
  bins = seq(from=start, to=end, by=window_size)
  window_dat = data.table::data.table(chr=paste0(chr),
    start=bins[-length(bins)], end=bins[-1])
  largest_bin = bins[length(bins)]
  if(largest_bin < end){
      window_dat = rbind(window_dat, data.frame(chr=paste0(chr),
        start=largest_bin, end=end))
  }

  op_dir = paste0(op_dir, "/")
  
  if(!dir.exists(paths = op_dir)){
    dir.create(path = op_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  temp_op_bed = tempfile(pattern = "trackr", tmpdir = op_dir, fileext = ".bed")
  data.table::fwrite(x = window_dat, file = temp_op_bed, sep = "\t", quote = FALSE,
    row.names = FALSE, col.names = FALSE)
  temp_op_bed
}

# 10/04/26 - Added metric parameter to control which summary column to return
# May need to add a parameter for a session/user ID to pass through which puts all generated
#  files in a separate directory for that ID to make sure things don't get overwritten with multiple
#  active users. That directory can then be deleted whenever a session ends.
get_summaries = function(bedSimple, bigWigs, op_dir = getwd(), nthreads = 1, metric = "max"){
  op_dir = paste0(op_dir, "/")
  
  if(!dir.exists(paths = op_dir)){
    dir.create(path = op_dir, showWarnings = FALSE, recursive = TRUE)
  }

  if(!metric %in% c("min", "max", "mean", "median", "sum")){
    stop("Metric must be one of \"min\", \"max\" (the default), \"mean\", \"median\" 
      or \"sum\".")
  }
  
  summaries = parallel::mclapply(bigWigs, FUN = function(bw){
    bn = gsub(pattern = "\\.bw$|\\.bigWig$", replacement = "", x = basename(bw))
    cmd = paste("bwtool summary -keep-bed -header -fill=0", bedSimple, bw,
      paste0(op_dir, bn, ".summary"))
    system(command = cmd, intern = TRUE)
    paste0(op_dir, bn, ".summary")
  }, mc.cores = nthreads)
  
  summary_list = lapply(summaries, function(x){
    x = data.table::fread(x)
    idx = switch(
      metric,
      "min" = c(1,6),
      "max" = c(1,7),
      "mean" = c(1,8),
      "median" = c(1,9),
      "sum" = c(1,10)
    )
    colnames(x)[idx] = c("chromosome","metric")
    cols = c("chromosome", "start", "end", "size", "metric")
    x = x[,..cols]
    if(all(is.na(x[,"metric"]))){
      message("No signal! Possible cause: chromosome name mismatch between bigWigs and
        queried loci.")
      x[, "metric" := 0]
    }
    x
  })
  
  #Remove intermediate files
  lapply(summaries, function(x) system(command = paste0("rm ", x), intern = TRUE))
  #system(command = paste0("rm ", bedSimple), intern = TRUE)
  
  names(summary_list) = gsub(pattern = "*\\.summary$", replacement = "",
    x = basename(path = unlist(summaries)))
  summary_list
}
