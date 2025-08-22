#' Read in necessary data 
#'
#' @description Reads in all necessary data for all plot types, to be used across any active sessions. For the 
#'  online browser all sessions will use the same data compiled and analyzed by the RAVING Consortium. 
#'  THIS WILL PROBABLY BE REPURPOSED FOR UPLOADING USER DATA
#'
#' @return None
#'
#' @noRd

# This should all be done once at the beginning of the session 
#  See https://shiny.posit.co/r/articles/improve/scoping/ for explanation on Shiny scoping rules
#  Using the correct scoping you can define variables which are globally accessible across all connected sessions
#  We will want to do this for anything that doesn't change - all our back end data and metadata
######

# Specify path to hic file
hic_path = 'data/Capture_merged_new.allValidPairs.hic'
options(scipen = 10)
hic_chrs = strawr::readHicChroms(hic_path)
hic_info = sort_by.data.frame(hic_chrs, hic_chrs$index)[-1,-1] # Ignore the 'ALL' chr, plus the index column
rownames(hic_info) = hic_info$name # Can now access chr size using hic_info[{chr}, 'length']
resolutions = strawr::readHicBpResolutions(hic_path)
normalizations = strawr::readHicNormTypes(hic_path)
default_chr = hic_info$name[1]
default_chr_length = hic_info[default_chr, 'length']

# Load in TAD boundaries
tads_path = 'data/dummy/tads_1_domains.bed'
tads = readr::read_tsv(tads_path, col_select=c(1, 2, 3), col_names=FALSE)
colnames(tads) = c('tChr', 'tStart', 'tEnd')

# Load in loops
loops_path = 'data/dummy/loops_default.bedGraph'
loops = readr::read_tsv(loops_path, col_names=FALSE)
colnames(loops) = c('lChr1', 'lStart1', 'lEnd1', 'lChr2', 'lStart2', 'lEnd2', 'lPval')
# Simplify loop coordinates by taking the middle of each bin as our node position
loops$from = (loops$lEnd1+loops$lStart1)/2
loops$to = (loops$lEnd2+loops$lStart2)/2
loops$lDist = loops$to - loops$from

# Load in PCA data
pca_path = 'data/dummy/25kb_pca1.bedGraph'
pca = readr::read_tsv(pca_path, col_names=FALSE)
colnames(pca) = c('pChr', 'pStart', 'pEnd', 'pScore')

# Load in ChIP-seq data
source('R/utils_trackplot_accessory.R')
chip_paths = c('data/38F_ChIP_H2Bk20ac.bigWig', 
		     'data/38F_ChIP_H3k4me3.bigWig')
bw = read_coldata(bws=chip_paths, build='hg19')

# Load in gene data
hg19data = genekitr::genInfo(org='human', hgVersion='v19', unique=TRUE)
# Need to rename columns so they don't clash with other variables and are consistent with other structures
names(hg19data)[names(hg19data) == c('chr', 'start', 'end')] = c('gChr', 'gStart', 'gEnd')
hg19data$gStart = as.numeric(hg19data$gStart)
hg19data$gEnd = as.numeric(hg19data$gEnd)
hg19data$width = as.numeric(hg19data$width)
hg19data$strand[hg19data$strand=='-1'] = '0'
hg19data$strand = as.numeric(hg19data$strand)
