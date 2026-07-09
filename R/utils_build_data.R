#" Prepare necessary data for browser operations
#"
#" @description Script to prepare browser data for use in the app. Builds a
#"  BrowserData R6 object and initializes it and loads it into the global
#"  environment for use in the app.
#"
#" @seealso [BrowserData()] for the R6 class definition
#" @importFrom usethis use_data
#"
#" @noRd
build_data <- function(){
	files = list()
	filetypes = list("genome"=".genome", "hic"=".hic", "hic/tads"=".bed", 
    "hic/loops"=".tsv", "hic/pca"=".bedGraph", "chip/signal"=".bigWig", "chip/peaks"=".multiinter",
    "atac/signal"=".bigWig", "atac/peaks"=".multiinter")

	for(type in names(filetypes)){
		file = list.files(path=paste0("data-raw/",type),
			pattern=paste0("\\",filetypes[[type]],"$"), full.names=TRUE)
		if(length(file) != 0){
			files[type] = list(file)
			if(length(file) > 1){
				if(!type %in% c("chip/signal", "chip/peaks", "atac/signal", "atac/peaks")){
					stop(paste0("Multiple files found in ",type," folder.\n",
						"This folder should only contain a single file."))
				}
			}
		}
		else{
			files[type] = NULL
			message("NOTE: ",type," data not found. Is the extension correct?\nRequired: ",
				filetypes[[type]])
		}
	}

	# Currently, all the basic setup options for region configuration are based on
	#  the Hi-C data.
	### In future this should be optional
	if(is.null(files[["genome"]])){
		stop("No genome file found. Genome file is required for browser function.")
	}

	browser_data = BrowserData$new(genome_path = files[["genome"]],
    hic_path = files[["hic"]],
		tads_path = files[["hic/tads"]],
		loops_path = files[["hic/loops"]],
		pca_path = files[["hic/pca"]],
		chip_signal_paths = files[["chip/signal"]],
    chip_peaks_paths = files[["chip/peaks"]],
    atac_signal_paths = files[["atac/signal"]],
    atac_peaks_paths = files[["atac/peaks"]]
	)

	usethis::use_data(browser_data, overwrite = TRUE)

	load(file="data/browser_data.rda", .GlobalEnv)
}
