#' Prepare necessary data for browser operations
#'
#' @description Script to prepare browser data for use in the app. Builds a
#'  BrowserData R6 object and initializes it for global use in the app.
#'  Does not need to be explicitly run by the user.
#'
#' @seealso [BrowserData()] for the R6 class definition
#' @importFrom usethis use_data
hic_path = '/home/cmcphan/Documents/ravenscroft/processing/capture/Capture_merged_new.allValidPairs.hic'
tads_path = '/home/cmcphan/Documents/ravenscroft/processing/capture/tads/tads_1_domains.bed'
loops_path = '/home/cmcphan/Documents/ravenscroft/processing/capture/loops/loops_default.bedGraph'
pca_path = '/home/cmcphan/Documents/ravenscroft/processing/capture/pca/25kb_pca1.bedGraph'
chip_paths = c('/home/cmcphan/Documents/ravenscroft/processing/chip-seq/38F_ChIP_H2Bk20ac.bigWig',
	'/home/cmcphan/Documents/ravenscroft/processing/chip-seq/38F_ChIP_H3k4me3.bigWig')
browser_data = BrowserData$new(hic_path, tads_path, loops_path, pca_path, chip_paths)

usethis::use_data(browser_data, overwrite = TRUE)
