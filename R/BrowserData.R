#' R6 object for storing backend browser data
#'
#' @description Class describing an R6 object capable of storing all necessary
#'  static data required for sharing among ravingBrowser modules, e.g. data
#'  frames for plot construction and back end metadata
#'
#' @field hic_path String representing the file path to the .hic file
#' @field hic_chrs Vector of strings enumerating the available chromosomes in the
#'  loaded HiC file
#' @field hic_info Data frame containing basic metadata about loaded HiC file. Has
#'  columns index (a double serving as a numerical ID), name (string describing
#'  the name of the chromosome), length (double describing the length of the
#'  chromosome in base pairs
#' @field resolutions,normalizations Vector of strings enumerating the available
#'  resolutions and normalization methods in the loaded HiC file
#' @field default_chr String describing the 'default' chromosome to be used when
#'  initializing UI elements - set as the first listed chromosome in hic_info
#' @field default_chr_length Double enumerating the length in base pairs of the
#'  default chromosome
#' @field tads,loops,pca Data frames containing data for topologically associated
#'  domains (TADs), loops and A/B compartmentalization scores (i.e. PCA
#'  scores) matching to the loaded HiC file
#' @field bw Bigwig file information used internally by [get_summaries()]
#' @field hg19data Data frame containing a variety of information about gene features
#'  from various sources. Produced by [genekitr::genInfo()]
#' 
#' @importFrom R6 R6Class
#' @import strawr
#' @importFrom readr read_tsv
#' @importFrom genekitr genInfo
BrowserData <- R6::R6Class(
	'BrowserData',
	public = list(
		hic_path = NULL,
		hic_chrs = NULL,
		hic_info = NULL,
		resolutions = NULL,
		normalizations = NULL,
		default_chr = NULL,
		default_chr_length = NULL,
		tads = NULL,
		loops = NULL,
		pca = NULL,
		bw = NULL,
		hg19data = NULL,
		#' @description Create a new BrowserData object and build the data from the input files provided.
		#' @param hic_path File path to the corresponding HiC matrix input file
		#' @param tads_path File path to the corresponding topologically associated domain input file
		#' @param loops_path File path to the corresponding loops input file
		#' @param pca_path File path to the corresponding PCA input file
		#' @param chip_paths File paths to the corresponding ChIP Bigwig input files
		#' @note Relative file paths should work from the root directory of the package.
		initialize = function(hic_path=NULL, tads_path=NULL, loops_path=NULL, 
									pca_path=NULL, chip_paths=NULL){
				if(!is.null(hic_path)){
					self$hic_path = hic_path
					self$hic_chrs = strawr::readHicChroms(hic_path)
					hic_info = sort_by.data.frame(self$hic_chrs, self$hic_chrs$index)
					rownames(hic_info) = hic_info$name
					hic_info = hic_info[(rownames(hic_info) != 'ALL'),]
					self$hic_info = hic_info
					self$resolutions = strawr::readHicBpResolutions(hic_path)
					self$normalizations = strawr::readHicNormTypes(hic_path)
					self$default_chr = hic_info$name[1]
					self$default_chr_length = hic_info[self$default_chr, 'length']
				}
				if(!is.null(tads_path)){
					tads = readr::read_tsv(tads_path, col_select=c(1, 2, 3), col_names=FALSE)
					colnames(tads) = c('tChr', 'tStart', 'tEnd')
					self$tads = tads
				}
				if(!is.null(loops_path)){
					loops = readr::read_tsv(loops_path, col_names=FALSE)
					colnames(loops) = c('lChr1', 'lStart1', 'lEnd1', 'lChr2', 'lStart2', 'lEnd2', 'lPval')
					# Simplify loop coordinates by taking the middle of each bin as our node position
					loops$from = (loops$lEnd1+loops$lStart1)/2
					loops$to = (loops$lEnd2+loops$lStart2)/2
					loops$lDist = loops$to - loops$from
					self$loops = loops
				}
				if(!is.null(pca_path)){
					pca = readr::read_tsv(pca_path, col_names=FALSE)
					colnames(pca) = c('pChr', 'pStart', 'pEnd', 'pScore')
					self$pca = pca
				}
				if(!is.null(chip_paths)){
					self$bw = read_coldata(bws=chip_paths, build='hg19')
				}
				hg19data = genekitr::genInfo(org='human', hgVersion='v19', unique=TRUE)
				# Rename columns so they don't clash with other variables and are consistent with other structures
				names(hg19data)[names(hg19data) == c('chr', 'start', 'end')] = c('gChr', 'gStart', 'gEnd')
				hg19data$gStart = as.numeric(hg19data$gStart)
				hg19data$gEnd = as.numeric(hg19data$gEnd)
				hg19data$width = as.numeric(hg19data$width)
				hg19data$strand[hg19data$strand=='-1'] = '0'
				hg19data$strand = as.numeric(hg19data$strand)
				self$hg19data = hg19data
		}
	)
)
