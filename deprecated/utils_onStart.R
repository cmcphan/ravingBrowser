#' Perform app startup operations 
#'
#' @description Run operations necessary at app startup 
#'
#' @return None
#'
#' @noRd

utils_onStart <- function(){
	hic_path = 'data/Capture_merged_new.allValidPairs.hic'
	tads_path = 'data/dummy/tads_1_domains.bed'
	loops_path = 'data/dummy/loops_default.bedGraph'
	pca_path = 'data/dummy/25kb_pca1.bedGraph'
	chip_paths = c('data/38F_ChIP_H2Bk20ac.bigWig',
		'data/38F_ChIP_H3k4me3.bigWig')
	browser_data = BrowserData$new(hic_path, tads_path, loops_path, pca_path, chip_paths)
}
