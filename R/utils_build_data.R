#' Prepare necessary data for browser operations
#'
#' @description Script to prepare browser data for use in the app. Builds a
#'  BrowserData R6 object and initializes it and loads it into the global
#'  environment for use in the app.
#'
#' @seealso [BrowserData()] for the R6 class definition
#' @importFrom usethis use_data
#'
#' @noRd
build_data <- function(){
	files = list()
	filetypes = list('hic'='.hic', 'hic/tads'='.bed', 'hic/loops'='.bedGraph',
		'hic/pca'='.bedGraph', 'chip'='.bigWig')

	for(type in names(filetypes)){
		file = list.files(path=paste0('data-raw/',type),
			pattern=paste0('\\',filetypes[[type]],'$'), full.names=TRUE)
		if(length(file) != 0){
			files[type] = list(file)
			if(length(file) > 1){
				if(type != 'chip'){
					stop(paste0('Multiple files found in ',type,' folder.\n',
						'Only the `chip` folder supports multiple files.'))
				}
			}
		}
		else{
			files[type] = NULL
			message('NOTE: ',type,' data not found. Is the extension correct?\nRequired: ',
				filetypes[[type]])
		}
	}

	# Currently, all the basic setup options for region configuration are based on
	#  the Hi-C data.
	### In future, this should be optional, or a genome file should be required
	if(is.null(files[['hic']])){
		stop('No Hi-C data found. Hi-C data is required for browser function.')
	}

	browser_data = BrowserData$new(hic_path = files[['hic']],
		tads_path = files[['hic/tads']],
		loops_path = files[['hic/loops']],
		pca_path = files[['hic/pca']],
		chip_paths = files[['chip']]
	)

	usethis::use_data(browser_data, overwrite = TRUE)

	load(file='data/browser_data.rda', .GlobalEnv)
}
