#' Back end browser data
#'
#' @description An R6 class used to share data across the browser app. Built at
#'  app startup, the data is read from the provided file paths and all data
#'  which is required for the app to function is read in and organised.
#'
#' @format An R6 object with 12 public fields
#' @seealso [BrowserData()] for the R6 class definition
"browser_data"
