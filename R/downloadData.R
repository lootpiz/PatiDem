#' Import TCGA metadata
#'
#' Download TCGA metadata (.xlsx) from the GDC data portal
#'
#' @param URL \code{character} A remote URL for the clinical information
#' @param fileName \code{character} A new file name to be saved
#' @param fileType \code{character} A file type
#' @param directory \code{character} A path to a directory where the file
#'     will be saved, create a directory if it does not exist
#' @param sheetIndex \code{numeric} The index of the sheet (.xlsx) to read data from
#' @param verbose \code{logical} Print status message during download, 
#'     defaults to TRUE
#'
#' @return A downloaded matrix
#'
#' @examples
#' tcga <- downloadData()
#'
#' @author Heewon Seo, \email{Heewon.Seo@uhnresearch.ca}
#'
#' @export
#' @importFrom downloader download
#' @importFrom openxlsx read.xlsx
downloadData <- function(URL = NULL, fileName = NULL, fileType = "EXCEL", 
    directory = file.path(".", "TCGA"), sheetIndex = 1, verbose = TRUE)
{
    if (is.null(URL)) {
        URL = "https://api.gdc.cancer.gov/data/1b5f413e-a8d1-4d10-92eb-7c4ae739ed81"
    }
    if (is.null(fileName)) {
        fileName = "TCGA_Patient_Info.xlsx"
    }
    if (sheetIndex < 1) {
        stop("Invalid index of the sheet. Please provide an index greater than 1.")
    }
    if (!file.exists(directory)) { # Create a directory
        dir.create(directory, recursive = TRUE)
    }
    if (file.exists(file.path(directory, fileName))) {
        warnings(sprintf("%s already exists in this location. The file will be replaced.",
            fileName))
    }
    downloader::download(url = URL, destfile = file.path(directory, fileName),
        quiet = !verbose)
    na.strings = c("[Not Available]", "[Not Applicable]", "[Not Evaluated]",
        "[Unknown]", "[Discrepancy]")
    if (fileType == "EXCEL") { # read an Excel file
        object <- openxlsx::read.xlsx(file.path(directory, fileName), sheet = sheetIndex,
            colNames = TRUE, na.strings = na.strings)
    } else if (fileType == "CSV") { # read a CSV file
        object <- read.csv(file.path(directory, fileName), na.strings = na.strings)
    } else if (fileType == "TXT") { # read a tab-delimited file
        object <- read.table(file.path(directory, fileName), header = TRUE, sep="\t",
            stringsAsFactor = FALSE, fill=TRUE, na.strings = na.strings)
    } else {
        stop("Unknown fileType. Please provide either EXCEL, CSV, or TXT.")
    }
    return(.initialFiltering(object))
}

.initialFiltering <- function(object, verbose = TRUE)
{
    if (missing(object)) {
	stop("Please provide an object to apply filter(s)") 
    }
    tumorStatus <- which(is.na(object$tumor_status)) # Select unknown tumor status
    if (verbose) {
        message(sprintf("The number of patients: %s\nThe number of pts w/o tumor status: %s",
            nrow(object), length(tumorStatus)))
    }
    return(object[-tumorStatus,])
}
