#' Plot a ChIP ribbon track
#'
#' @description Generate a ChIP ribbon track built in ggplot2
#'
#' @param c An integer value representing the cosome of the region to be plotted.
#' @param s,e Integer values representing the start and end coordinates
#'  (in base pairs) of the region to be plotted.
#' @param resolution Size of the bins (in base pairs) for which summary statistics are
#'  calculated from the bigWig files. A higher resolution (smaller bin size) will
#'  result in a more detailed ribbon but will take longer to generate.
#'  Defaults to 5000.
#' @param mark List of ChIP-seq marks selected for display by the user.
#' @param session Internal Shiny parameter containing session data.
#'
#' @return A list of ggplot2 objects showing ChIP signals for the specified samples
#'  over the configured region.
#'
#' @noRd
#'
#' @import ggplot2
#' @importFrom scales hue_pal
#' @importFrom ggblend blend
#' @importFrom quickcode mix.color
plot_chip <- function(c, s, e, resolution, marks, session) {
  w_ratio = session$userData$screen_width()/1920
  h_ratio = session$userData$screen_height()/965
  chip_query = paste0(c,':',s,'-',e)
  bedfile = gen_windows(chr=c, start=s, end=e, window_size=resolution)
  bw = subset(browser_data$chip_signal, mark %in% marks)
  chip_signal = get_summaries(bedSimple=bedfile, bigWigs=bw$bw_files, metric="mean")
  chip_signal_names = bw$bw_sample_names
  samples = unique(bw$sample)
  #bw_input = subset(browser_data$chip_signal, sample %in% samples & mark=="INPUT")
  #input_signal = get_summaries(bedSimple=bedfile, bigWigs=bw_input$bw_files, metric="mean")
  # Remove temporary bedfile now that we have no more use for it. This is ripped from the get_summaries
  #  function, where it was originally and modified to run asynchronously
  system(command = paste0("rm ", bedfile), intern = FALSE, wait=FALSE)
  COLOURS = list()
  scaleVals = sort(unique(browser_data$chip_peaks$broad[["samples"]]))
  scaleVals = unique(c(scaleVals, unique(browser_data$chip_peaks$narrow[["samples"]])))
  singles = unique(unlist(lapply(scaleVals, function(x){strsplit(x, split=",")[[1]]})))
  scale = scales::hue_pal()(length(singles))
  for(i in 1:length(singles)){
    if(!singles[i] %in% names(COLOURS)){
      COLOURS[[singles[i]]] = scale[i]
    }
  }
  for(i in 1:length(scaleVals)){
    split = strsplit(scaleVals[i], split=",")[[1]]
    if(length(split) > 1){
      blend = NULL
      for(x in split){
        blend = c(blend, COLOURS[[x]])
      }
      COLOURS[[scaleVals[i]]] = quickcode::mix.color(blend)
    }
  }
  plots = sapply(marks, function(m){
    delta_signal = list()
    for(sample in unique(subset(bw, mark==m)[["sample"]])){
      delta_signal[[sample]] = chip_signal[[paste(sample, m, sep="_")]]
      #delta_signal[[sample]][,"sample"] = sample
      #delta_signal[[sample]][,"metric"] = delta_signal[[sample]][["metric"]] - 
      #  input_signal[[paste(sample, "INPUT", sep="_")]][["metric"]]
      #delta_signal[[sample]][,"metric"] = delta_signal[[sample]][["metric"]] /
      #  max(delta_signal[[sample]][["metric"]])
    }
    peaksBroad = subset(browser_data$chip_peaks$broad, mark==m & chr==c & ((end >= s & start <= s) | 
      (start <= e & end >= e) | (start >= s & end <= e)))
    peaksNarrow = subset(browser_data$chip_peaks$narrow, mark==m & chr==c & ((end >= s & start <= s) | 
      (start <= e & end >= e) | (start >= s & end <= e)))
    # Calculate peak block positions
    maxVal = max(unlist(lapply(delta_signal, function(df){ max(df[,"metric"]) }))) # Get max signal value
    broadBlockMin = maxVal*1.05
    broadBlockMax = maxVal*1.15
    narrowBlockMin = maxVal*1.20
    narrowBlockMax = maxVal*1.30
    plot = ggplot2::ggplot() +
      lapply(names(delta_signal), function(n){ggplot2::geom_area(ggplot2::aes(x=start, y=metric, 
        fill=n), alpha=0.5, data=delta_signal[[n]])}) |> ggblend::blend("lighten") +
      ggplot2::geom_rect(data=peaksBroad, ggplot2::aes(xmin=start, xmax=end, ymin=maxVal, 
        ymax=broadBlockMax, fill=samples), alpha=1) +
      ggplot2::geom_rect(data=peaksNarrow, ggplot2::aes(xmin=start, xmax=end, ymin=narrowBlockMin, 
        ymax=narrowBlockMax, fill=samples), alpha=1) +
      ggplot2::coord_cartesian(xlim=c(s, e), expand=FALSE) +
      {if(m==marks[1]) ggplot2::scale_fill_manual(name="Sample",values=COLOURS)
      else ggplot2::scale_fill_manual(guide="none", values=COLOURS)} +
      #ggplot2::scale_fill_manual(guide="none", values=COLOURS) +
      ggplot2::labs(subtitle=m) +
      ggplot2::theme(aspect.ratio=0.05,
        panel.background=ggplot2::element_blank(),
        plot.margin=ggplot2::margin(0, 0, 0, 0),
        axis.ticks.y=ggplot2::element_blank(),
        axis.text.y=ggplot2::element_blank(),
        axis.title=ggplot2::element_blank(),
        plot.subtitle=ggplot2::element_text(vjust=-3),
        text=ggplot2::element_text(size=4.5*min(w_ratio, h_ratio)),
        legend.key.size=grid::unit(10*min(w_ratio, h_ratio), "points")
      )
    # Not sure where the extra 0.01 height is coming from but it's required
    #  otherwise the plots will be compressed on the x-axis when more than 2
    #  are drawn
    session$userData$plot_heights[[paste0("chip-",m)]] = 0.05
    return(plot)
  }, USE.NAMES=TRUE)
  rm(bw)
  rm(chip_signal)
  return(plots)
}
