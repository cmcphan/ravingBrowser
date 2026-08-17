#' R6 object for storing backend browser data
#'
#' @description Class describing an R6 object capable of storing all necessary
#'  static data required for sharing among ravingBrowser modules, e.g. data
#'  frames for plot construction and back end metadata
#'
#' @field genome String representing file path to genome file. Genome file
#'  should contain names and sizes for chromosomes to be visualised. Chromosome
#'  naming must be consistent with Hi-C data if present.
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
#' @field tads,pca Data frames containing data for topologically associated
#'  domains (TADs) and A/B compartmentalization scores (i.e. PCA
#'  scores) matching to the loaded HiC file
#' @field loops TSV file describing called loops matching the HiC data. This should be
#'  filtered to exclude trans interactions (i.e. bait and oef bins on different chromosomes)
#' @field chip_signal Bigwig file information for ChIP datasets, used internally by
#'  [get_summaries()]. Names should be of the form {sample}_{mark}.bigWig. Input samples
#'  should replace {mark} with INPUT.
#' @field chip_peaks A list of strings describing filepaths to BED-like files containing
#'  called ChIP-seq peaks. These are outputs from bedtools multiinter which describe overlapping
#'  intervals between processed and cleaned called peaks across all samples for the same mark. 
#'  File names should be of the form `{mark}_overlap.multiinter`. All files should
#'  have the same set of samples included. Any that don't include all samples should be processed
#'  with awk to add the remaining samples. A header line should be included.
#' @field chip_max_reps Integer value representing the number of reps present in the ChIP data.
#' @field chip_marks A list of unique ChIP-seq marks included in the data.
#' @field atac_signal Bigwig file information for ATAC datasets, used internally by
#'  [get_summaries()]. Names should be of the form {sample}.bigWig.
#' @field atac_peaks A list of strings describing filepaths to BED files containing
#'  processed and cleaned called ATAC-seq peaks. Filenames should be of the form 
#'  {peakset}_overlap.multiinter, where peakset may be any identifier to 
#'  group files of the same set, e.g. method used. Peakset should not include any underscores
#'  (_) to enable proper name parsing. All files should have the same set of samples included, and
#'  samples should match those in the atac_signal filenames. Any that don't include all samples 
#'  should be processed with awk to add the remaining samples. A header line should be included.
#' @field atac_max_reps Integer value representing the number of reps present in the ATAC data.
#' @field COLOURS A named list matching ChIP/ATAC tracks to colours used for their plots.
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
#' @importFrom readr read_tsv cols col_character
#' @importFrom genekitr genInfo
BrowserData <- R6::R6Class(
	'BrowserData',
	public = list(
    genome = NULL,
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
		chip_signal = NULL,
    chip_peaks = NULL,
    chip_max_reps = NULL,
    chip_marks = NULL,
    atac_signal = NULL,
    atac_peaks = NULL,
    atac_max_reps = NULL,
    COLOURS = NULL,
		genes = NULL,
    gene_feature_counts = NULL,
		plot_types = NULL,
		#' @description Create a new BrowserData object and build the data from the input
		#'  files provided.
    #' @param genome_path File path to the corresponding genome file
		#' @param hic_path File path to the corresponding HiC matrix input file
		#' @param tads_path File path to the corresponding topologically associated domain
		#'  input file
		#' @param loops_path File path to the corresponding loops input file
		#' @param pca_path File path to the corresponding PCA input file
		#' @param chip_signal_paths File paths to the corresponding ChIP Bigwig input files
    #' @param chip_peaks_paths File paths to the corresponding ChIP peak input files
    #' @param atac_signal_paths File paths to the corresponding ATAC Bigwig input files
    #' @param atac_peaks_paths File paths to the corresponding ATAC peak input files
		#' @note Relative file paths should work from the root directory of the package.
    #'  Generally not provided manually but set through build_data()
		initialize = function(genome_path=NULL, hic_path=NULL, tads_path=NULL, 
      loops_path=NULL, pca_path=NULL, chip_signal_paths=NULL, chip_peaks_paths=NULL, 
      atac_signal_paths=NULL, atac_peaks_paths=NULL){
			if(!is.null(genome_path)){
        genome = readr::read_tsv(genome_path, col_names=FALSE, show_col_types = FALSE)
        colnames(genome) = c("chr", "size")
        genome = tibble::column_to_rownames(genome, "chr")
        self$genome = genome
        self$default_chr = rownames(self$genome)[1]
				self$default_chr_length = self$genome[self$default_chr, "size"]
        # Get chromosome names using rownames(genome)
        # Get specific chromosome size using genome[chr, "size"]
      }
      if(!is.null(hic_path)){
				self$hic = hic_path
				self$hic_chrs = strawr::readHicChroms(hic_path)
				hic_info = sort_by.data.frame(self$hic_chrs, self$hic_chrs$index)
				rownames(hic_info) = hic_info$name
				hic_info = hic_info[(rownames(hic_info) != "ALL"),]
				self$hic_info = hic_info
				self$resolutions = strawr::readHicBpResolutions(hic_path)
				self$normalizations = strawr::readHicNormTypes(hic_path)
        self$plot_types[["hic"]] = "Hi-C"
			}
			if(!is.null(tads_path)){
				tads = readr::read_tsv(tads_path, col_select=c(1, 2, 3), col_names=FALSE, show_col_types = FALSE)
				colnames(tads) = c("tChr", "tStart", "tEnd")
				self$tads = tads
			}
			if(!is.null(loops_path)){
				loops = readr::read_tsv(loops_path, col_names=FALSE, show_col_types = FALSE)
				colnames(loops) = c("bait_chr", "bait_start", "bait_end", "bait_id", "oef_chr", "oef_start",
          "oef_end", "oef_id", "reads", "score", "region_id", "rep", "interaction", "support")
				# Simplify loop coordinates by taking the middle of each bin as our node
				#  position
				loops$from = (loops$bait_end+loops$bait_start)/2
				loops$to = (loops$oef_end+loops$oef_start)/2
				loops$dist = loops$to - loops$from
				self$loops = loops
			}
			if(!is.null(pca_path)){
				pca = readr::read_tsv(pca_path, col_names=FALSE, show_col_types = FALSE)
				colnames(pca) = c("pChr", "pStart", "pEnd", "pScore")
				self$pca = pca
			}
			if(!is.null(chip_signal_paths)){
				self$chip_signal = read_coldata(bws=chip_signal_paths, build="hg38")
        self$chip_signal[,"sample"] = unlist(lapply(self$chip_signal$bw_sample_names, 
          function(x){strsplit(x, "_")[[1]][1]}))
        self$chip_signal[,"mark"] = unlist(lapply(self$chip_signal$bw_sample_names, 
          function(x){strsplit(x, "_")[[1]][2]}))
        self$plot_types[["chip"]] = "ChIP-seq"
        self$chip_marks = unique(self$chip_signal$mark)
			}
      if(!is.null(chip_peaks_paths)){
        self$chip_peaks = list()
        cols = NULL
        for(f in chip_peaks_paths){
          base = strsplit(f, "/")[[1]]
          split = strsplit(base[length(base)], "_")[[1]]
          mark = split[1]
          # Need to force the read function to treat the samples column as characters otherwise it
          #  will default to double
          df = readr::read_tsv(f, col_names=TRUE, col_types=readr::cols(samples=readr::col_character()), 
            show_col_types = FALSE)
          self$chip_max_reps[[mark]] = max(df$nReps)
          df$nReps = as.factor(df$nReps)
          self$chip_peaks[[mark]] = df
        }
        if(!"chip" %in% names(self$plot_types)){
          # This allows either signal or peaks to be added individually but requires that each
          #  is also handled individually e.g. in plot draw functions 
          self$plot_types[["chip"]] = "ChIP-seq"
        }
      }
      if(!is.null(atac_signal_paths)){
				self$atac_signal = read_coldata(bws=atac_signal_paths, build="hg38")
        self$atac_signal[,"sample"] = unlist(lapply(self$atac_signal$bw_sample_names, 
          function(x){strsplit(x, "_")[[1]][1]}))
        self$plot_types[["atac"]] = "ATAC-seq"
			}
      if(!is.null(atac_peaks_paths)){
        self$atac_peaks = list()
        for(f in atac_peaks_paths){
          base = strsplit(f, "/")[[1]]
          split = strsplit(base[length(base)], "_")[[1]]
          peakset = split[1]
          df = readr::read_tsv(f, col_names=TRUE, show_col_types = FALSE)
          self$atac_max_reps[[peakset]] = max(df$nReps)
          df$nReps = as.factor(df$nReps)
          self$atac_peaks[[peakset]] = df
        }
        if(!"atac" %in% names(self$plot_types)){
          self$plot_types[["atac"]] = "ATAC-seq"
        }
      }
      colours = scales::hue_pal()(8)
      c = 0
      for(m in self$chip_marks){
        c = c+1
        if(c > 8){
          c = 1
        }
        self$COLOURS[[paste0("chip-",m)]] = colours[c]
      }
      for(p in names(self$atac_peaks)){
        c = c+1
        if(c > 8){
          c = 1
        }
        self$COLOURS[[paste0("atac-",p)]] = colours[c]
      }
			genes = genekitr::genInfo(org="human", hgVersion="v38", unique=TRUE)
			# Rename columns so they don"t clash with other variables and are consistent
			#  with other structures
			names(genes)[which(names(genes) %in% c("chr", "start", "end"))] = c("gChr",
				"gStart", "gEnd")
      genes = subset(genes, !is.na(gene_biotype))
      genes$gChr = paste0("chr", genes$gChr)
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
