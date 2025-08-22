# This R script contains functions for bigWig visualization
#
# These functions have been extracted from the larger `trackplot` codebase and modified for use 
#  in the ravingBrowser app
#
# Source code: https://github.com/PoisonAlien/trackplot
#
# MIT License
# Copyright (c) 2020 Anand Mayakonda <anandmt3@gmail.com>
#
# Change log:
# User changes cmcphan [2025-08-06]
#   * Modified .gen_windows to improve speed
# Version: 1.6.00 [2024-10-28]
#   * Added argument `track_overlay` for a single track with line plot. #14
#   * Bug fix: color alpha differs between tracks. Issue: #34
# Version: 1.5.10 [2024-02-14]
#   * Added argument `layout_ord` and `bw_ord` to `track_plot()` re-order the overall tracks and bigWig tracks
#   * Added `xlab` and `ylab` arguments to `profile_plot()`
# Version: 1.5.01 [2023-10-17]
#   * Bug fix parsing loci while parsing GTF
#   * Small updates to profile_heatmap()
# Version: 1.5.00 [2023-08-24]
#   * Added `read_coldata` to import bigwig and bed files along with metadata. This streamlines all the downstream processes
#   * Added `profile_heatmap` for plotting heatmap
#   * Added `diffpeak` for minimal differential peak analysis based on peak intensities
#   * Added `volcano_plot` for diffpeak results visualization
#   * Added `summarize_homer_annots`
#   * Support for GTF files with `track_extract`
#   * `track_extract` now accepts gene name as input.
#   * More customization to `profile_extract` `profile_plot` and `plot_pca`
#   * Nicer output with `extract_summary` 
#   * Update mysql query for UCSC. Added `ideoTblName` argument for `track_extract`. Issue: #19
# Version: 1.4.00 [2023-07-27]
#   * Updated track_plot to include chromHMM tracks and top peaks tracks
#   * Support to draw narrowPeak or boradPeak files with track_plot
#   * Support to query ucsc for chromHMM tracks
#   * Additional arguments to track_plot to adjust heights of all the tracks and margins
#   * Improved track_extract - (extracts gene models and cytobands to avoid repetitive calling ucsc genome browser)
#   * Additional arguments to pca_plot for better plotting
#   * Added example datasets
# Version: 1.3.10 [2021-10-06]
#   * Support for negative values (Issue: https://github.com/PoisonAlien/trackplot/issues/6 )
#   * Added y_min argument to track_plot. 
#   * Change the default value for collapse_tx to TRUE
# Version: 1.3.05 [2021-06-07]
#   * Summarize and groupScaleByCondition tracks by condition. Issue: #4
#   * Allow the script to install as a package.
#   * Added y_max argument for custom y-axis limits in track_plot. 
# Version: 1.3.01 [2021-04-26]
#   * Fix gtf bug. Issue: #3
# Version: 1.3.0 [2021-03-26]
#   * modularize the code base to avoid repetitive data extraction and better plotting
# Version: 1.2.0 [2020-12-09]
#   * Added bwpcaplot()
# Version: 1.1.11 [2020-12-07]
#   * Bug fixes in profileplot(): Typo for .check_dt() and startFrom estimation
# Version: 1.1.1 [2020-12-04]
#   * trackplot() now plots ideogram of target chromosome
# Version: 1.1.0 [2020-12-01]
#   * Added profileplot()
# Version: 1.0.0 [2020-11-27]
#   * Initial release

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
read_coldata = function(bws = NULL, sample_names = NULL, build = "hg38", input_type = "bw"){
  
  if(is.null(bws)){
    stop("Please provide paths to bigWig files")
  }
  
  input_type = match.arg(arg = input_type, choices = c("bw", "peak"))
  
  message("Checking for files..")
  bws = as.character(bws)
  lapply(bws, function(x){
    if(!file.exists(x)){
      stop(paste0(x, " does not exist!"))
    }
  })
  
  if(is.null(sample_names)){
    bw_sample_names = unlist(data.table::tstrsplit(x = basename(bws), split = "\\.", keep = 1))
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
  message("Input type: ", input_type)
  message("Ref genome: ", build)
  message("OK!")
  
  coldata
}

gen_windows = function(chr = NA, start, end, window_size = 50, op_dir = getwd()){
  
  bins = seq(from=start, to=end, by=window_size)
  window_dat = data.table::data.table(chr=paste0('chr',chr), start=bins[-length(bins)], end=bins[-1])
  largest_bin = bins[length(bins)]
  if(largest_bin < end){
      window_dat = rbind(window_dat, data.frame(chr=paste0('chr',chr), start=largest_bin, end=end))
  }

  op_dir = paste0(op_dir, "/")
  
  if(!dir.exists(paths = op_dir)){
    dir.create(path = op_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  temp_op_bed = tempfile(pattern = "trackr", tmpdir = op_dir, fileext = ".bed")
  data.table::fwrite(x = window_dat, file = temp_op_bed, sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)
  temp_op_bed
}


get_summaries = function(bedSimple, bigWigs, op_dir = getwd(), nthreads = 1){
  #bedSimple = temp_op_bed; bigWigs = list.files(path = "./", pattern = "bw"); op_dir = getwd(); nthreads = 1
  op_dir = paste0(op_dir, "/")
  
  if(!dir.exists(paths = op_dir)){
    dir.create(path = op_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  summaries = parallel::mclapply(bigWigs, FUN = function(bw){
    bn = gsub(pattern = "\\.bw$|\\.bigWig$", replacement = "", x = basename(bw))
    cmd = paste("bwtool summary -with-sum -keep-bed -header", bedSimple, bw, paste0(op_dir, bn, ".summary"))
    system(command = cmd, intern = TRUE)
    paste0(op_dir, bn, ".summary")
  }, mc.cores = nthreads)
  
  summary_list = lapply(summaries, function(x){
    x = data.table::fread(x)
    colnames(x)[1] = 'chromosome'
    x = x[,.(chromosome, start, end, size, max)]
    if(all(is.na(x[,max]))){
      message("No signal! Possible cause: chromosome name mismatch between bigWigs and queried loci.")
      x[, max := 0]
    }
    x
  })
  
  
  #Remove intermediate files
  lapply(summaries, function(x) system(command = paste0("rm ", x), intern = TRUE))
  system(command = paste0("rm ", bedSimple), intern = TRUE)
  
  names(summary_list) = gsub(pattern = "*\\.summary$", replacement = "", x = basename(path = unlist(summaries)))
  summary_list
}
