#' R6 object for storing backend browser data
#'
#' @description Class describing an R6 object capable of storing all necessary
#'  static data required for sharing among ravingBrowser modules, e.g. data
#'  frames for plot construction and back end metadata
#'
#' @field hic String representing the file path to the .hic file
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
#' @field chip Bigwig file information for ChIP datasets, used internally by
#'  [get_summaries()]
#' @field genes Data frame containing a variety of information about gene features
#'  from various sources. Produced by [genekitr::genInfo()]
#' @field gene_feature_counts Named vector of unique gene biotypes from `genes` data
#'  frame with values set to the feature count of each biotype. Sorted in descending 
#'  order 
#' @field plot_types Vector of strings detailing the plot types loaded in the data 
#'  structure
#' 
#' @importFrom R6 R6Class
#' @import strawr
#' @importFrom readr read_tsv
#' @importFrom genekitr genInfo
BrowserData <- R6::R6Class(
	'BrowserData',
	public = list(
		hic = NULL,
		hic_chrs = NULL,
		hic_info = NULL,
		resolutions = NULL,
		normalizations = NULL,
		default_chr = NULL,
		default_chr_length = NULL,
		tads = NULL,
		loops = NULL,
		pca = NULL,
		chip = NULL,
		genes = NULL,
    gene_feature_counts = NULL,
		plot_types = NULL,
		#' @description Create a new BrowserData object and build the data from the input
		#'  files provided.
		#' @param hic_path File path to the corresponding HiC matrix input file
		#' @param tads_path File path to the corresponding topologically associated domain
		#'  input file
		#' @param loops_path File path to the corresponding loops input file
		#' @param pca_path File path to the corresponding PCA input file
		#' @param chip_paths File paths to the corresponding ChIP Bigwig input files
		#' @note Relative file paths should work from the root directory of the package.
    #'  Generally not provided manually but set through build_data()
		initialize = function(hic_path=NULL, tads_path=NULL, loops_path=NULL, 
									pca_path=NULL, chip_paths=NULL){
			if(!is.null(hic_path)){
				self$hic = hic_path
				self$hic_chrs = strawr::readHicChroms(hic_path)
				hic_info = sort_by.data.frame(self$hic_chrs, self$hic_chrs$index)
				rownames(hic_info) = hic_info$name
				hic_info = hic_info[(rownames(hic_info) != "ALL"),]
				self$hic_info = hic_info
				self$resolutions = strawr::readHicBpResolutions(hic_path)
				self$normalizations = strawr::readHicNormTypes(hic_path)
				self$default_chr = hic_info$name[1]
				self$default_chr_length = hic_info[self$default_chr, "length"]
        self$plot_types[["hic"]] = "Hi-C"
			}
			if(!is.null(tads_path)){
				tads = readr::read_tsv(tads_path, col_select=c(1, 2, 3), col_names=FALSE)
				colnames(tads) = c("tChr", "tStart", "tEnd")
				self$tads = tads
			}
			if(!is.null(loops_path)){
				loops = readr::read_tsv(loops_path, col_names=FALSE)
				colnames(loops) = c("lChr1", "lStart1", "lEnd1", "lChr2", "lStart2",
					"lEnd2", "lPval")
				# Simplify loop coordinates by taking the middle of each bin as our node
				#  position
				loops$from = (loops$lEnd1+loops$lStart1)/2
				loops$to = (loops$lEnd2+loops$lStart2)/2
				loops$lDist = loops$to - loops$from
				self$loops = loops
			}
			if(!is.null(pca_path)){
				pca = readr::read_tsv(pca_path, col_names=FALSE)
				colnames(pca) = c("pChr", "pStart", "pEnd", "pScore")
				self$pca = pca
			}
			if(!is.null(chip_paths)){
				self$chip = read_coldata(bws=chip_paths, build="hg38")
        self$plot_types[["chip"]] = "ChIP-seq"
			}
			genes = genekitr::genInfo(org="human", hgVersion="v19", unique=TRUE)
			# Rename columns so they don"t clash with other variables and are consistent
			#  with other structures
			names(genes)[names(genes) == c("chr", "start", "end")] = c("gChr",
				"gStart", "gEnd")
      genes = subset(genes, !is.na(gene_biotype))
			genes$gStart = as.numeric(genes$gStart)
			genes$gEnd = as.numeric(genes$gEnd)
			genes$width = as.numeric(genes$width)
			genes$strand[genes$strand=="-1"] = "0"
			genes$strand = as.numeric(genes$strand)
      genes$rowId = seq(from=1, to=nrow(genes))
      message("Collapsing gene biotypes")
      genes$gene_biotype = gsub(".*_pseudogene", "pseudogene", genes$gene_biotype)
      genes$gene_biotype = gsub(".*_gene", "protein_coding", genes$gene_biotype)
      genes$gene_biotype = gsub("Mt_.*RNA", "misc_RNA", genes$gene_biotype)
      aliases = list()
      message("Compiling gene aliases - this may take a while")
      genes = genes %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(aliases=dplyr::if_else(
          is.na(ncbi_alias),
          dplyr::if_else(
            is.na(ensembl_alias),
            list("None"),
            list(unique(unlist(c(strsplit(ensembl_alias, ";")))))
          ),
          dplyr::if_else(
            is.na(ensembl_alias),
            list(unique(unlist(c(strsplit(ncbi_alias, ";"))))),
            list(unique(unlist(c(strsplit(ncbi_alias, ";"), strsplit(ensembl_alias,";")))))
          )
        )) %>%
        dplyr::select(-c("ncbi_alias", "ensembl_alias")) %>%
        dplyr::ungroup()
      message("Finished")
			self$genes = genes
      biotypes = unique(genes$gene_biotype)
      feature_counts = c()
      for(type in biotypes){
        feature_counts[type] = nrow(subset(self$genes, gene_biotype==type))
      }
      self$gene_feature_counts = sort(feature_counts, decreasing=TRUE)
      self$plot_types[["genes"]] = "Gene Features"
		}
	)
)
