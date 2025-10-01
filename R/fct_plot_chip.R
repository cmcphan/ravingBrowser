#' Plot a ChIP ribbon track
#'
#' @description Generate a ChIP ribbon track built in ggplot2
#'
#' @param chr An integer value representing the chromosome of the region to be plotted.
#' @param start,end Integer values representing the start and end coordinates
#'  (in base pairs) of the region to be plotted.
#' @param resolution Size of the bins (in base pairs) for which summary statistics are
#'  calculated from the bigWig files. A higher resolution (smaller bin size) will
#'  result in a more detailed ribbon but will take longer to generate.
#'  Defaults to 5000.
#'
#' @return A list of ggplot2 objects showing ChIP signals for the specified samples
#'  over the configured region.
#'
#' @noRd
#'
#' @import ggplot2
plot_chip <- function(chr, start, end, resolution=5000, chip_samples) {
  chip_query = paste0('chr',chr,':',start,'-',end)
  bedfile = gen_windows(chr=chr, start=start, end=end, window_size=resolution)
  bw = subset(browser_data$chip, bw_sample_names %in% chip_samples)
  chip_signal = get_summaries(bedSimple=bedfile, bigWigs=bw$bw_files)
  chip_signal_names = bw$bw_sample_names
  combined_data = data.frame()
  for(i in 1:length(chip_signal_names)){
    clean_signal = subset(chip_signal[[i]], !is.na(max))
    clean_signal$sample = chip_signal_names[i]
    combined_data = rbind(combined_data, clean_signal)
  }
  rm(chip_signal)
  chip_track = ggplot2::ggplot() +
    ggplot2::geom_area(ggplot2::aes(x=start, y=max), data=combined_data) +
    ggplot2::coord_cartesian(xlim=c(start, end), expand=FALSE) +
    ggplot2::theme(panel.background=ggplot2::element_blank(),
      plot.margin=ggplot2::margin(0, 0, 0, 0),
      axis.title=ggplot2::element_blank(),
      axis.ticks.y=ggplot2::element_blank(),
      axis.text.y=ggplot2::element_blank()) +
    ggplot2::facet_wrap(vars(sample), nrow=(length(chip_samples)),
      ncol=1, scales='free_y')
  rm(combined_data)
  return(chip_track)
}
