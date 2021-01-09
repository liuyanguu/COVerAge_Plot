#' Downloads one of the COVerAGE-DB
#' datasets hosted on [OSF](https://osf.io/mpwjq/).
#' It reads the downloaded data and converts it into a data frame.
#'
#' These functions use the OSF API to download the
#' publicly available COVerAGE-DB
#' datasets. It then returns the downloaded dataset as a data frame, data table,
#' or tibble. [download_covid()] Uses the \pkg{{osfr}} package as a backend.
#' [download_covid_version()] uses [utils::download.file()] to download the
#' specific requested file version.
#' There are 4 possible datasets available for download: "inputDB", "Output_5",
#' "Output_10", "qualityMetrics".
#' See <https://timriffe.github.io/covid_age/GettingStarted.html> for more
#' information about these datasets.
#'
#' If the download times out, consider increasing the 'timeout' option in
#' [options()]
#' @seealso [osfr::osf_retrieve()] for the OSF entity retrieval function;
#' [osfr::osf_download()] for the downloading function;
#' [data.table::fread()] for
#' the reading function.
#' @title Download COVerAGE-DB data
#' @param data The name of the dataset to download. Can be one of the
#' the following: "inputDB", "Output_5", "Output_10", "qualityMetrics".
#' @param dest Character. If 'temp' is set to FALSE, specifies the directory
#' the dataset should be downloaded to. By default,
#' the current working directory.
#' @param temp Logical. Should the dataset be downloaded
#' to a temporary directory?
#' @param download_only Logical. Should the dataset be downloaded without
#' reading it
#' @param return What should be the return type? Can be on of the following:
#' "data.frame", "data.table", "tibble".
#' @param progress Passed to [osfr::osf_download()]. Logical, if TRUE
#' progress bars are displayed for each file transfer. Mainly useful for
#' transferring large files. For tracking lots of small files, setting
#' ‘verbose = TRUE’ is more informative. For [download_covid_version()] this is
#' passed instead to [utils::download.file()]
#' @param conflicts Passed to [osfr::osf_download()]. This determines what
#' happens when a file with the same name
#' exists at the specified destination. Can be one of the
#' following:
#' * ‘"error"’ (the default): throw an error and abort the
#' file transfer operation.
#' * ‘"skip"’: skip the conflicting file(s) and continue
#' transferring the remaining files.
#' * ‘"overwrite"’: replace the existing file with the
#' transferred copy.
#' @param recurse Passed to [osfr::osf_download()]. Applies only to
#' OSF directories. If TRUE, a directory is fully recursed and all nested
#' files and subdirectories are downloaded. Alternatively, a positive number
#' will determine the number of levels to recurse.
#' @param verbose Passed to [osfr::osf_download()] Logical, indicating whether
#' to print informative messages about interactions with the OSF API
#' (default FALSE).
#' @param ... Additional named arguments passed to [data.table::fread()]
#' @return By default a data frame with the uncompressed dataset.
#' Can be set to return either a data table or
#' a tibble. The return type is controlled by the 'return' parameter.
#' If 'download_only' is set to TRUE, the function will
#' invisibly return NULL after downloading the dataset.
#' @author Erez Shomron
#' @examples
#' \dontrun{
#' # Basic usage
#' df <- download_covid("inputDB")
#' # Download data to the working directory.
#' df <- download_covid("Output_5", temp = FALSE)
#' # If you want a tibble instead of a data frame:
#' df <- download_covid("Output_10", return = "tibble")
#' # Get the first 'inputDB' version
#' df <- download_covid_version("InputDB", version = 1)
#' }
#'
#' @export
download_covid <- function(data = c("inputDB", "Output_5", "Output_10",
                                    "qualityMetrics"), dest = getwd(),
                           temp = FALSE, download_only = FALSE,
                           return = c("data.frame", "data.table", "tibble"),
                           progress = TRUE, conflicts = "overwrite",
                           recurse = FALSE, verbose = FALSE, ...) {
        stopifnot(is.character(data))

        rinfo <- get_rinfo(data[1])
        stopifnot(!is.null(rinfo)) # means 'data' wasn't one
                                   # of the listed datasets

        stopifnot(is.logical(temp), is.logical(download_only))
        if (temp && download_only) warning("'temp' set to TRUE, ",
                                           "ignoring 'download_only'")
        if (temp) {
                path <- tempdir() # cleaned up after reading the dataset
        } else {
                stopifnot(is.character(dest), length(dest) == 1)
                path <- dest
        }

        osf <- osfr::osf_retrieve_file(rinfo[[1]])
        osfr::osf_download(osf, path, progress = progress,
                           conflicts = conflicts, recurse = recurse,
                           verbose = verbose)
        if (!temp && download_only) return(invisible(NULL))

        filename    <- paste0(data[1], ".zip")
        zippath     <- file.path(path, filename)

        stopifnot(file.exists(zippath)) # The file was not downloaded or deleted
        if (temp) on.exit(unlink(zippath), add = TRUE) # Cleanup


        return(read_covid(zippath, data, return, ...))
}

#' @rdname download_covid
#' @param version Integer. Which file version to download?
#' @param download_method Passed to [utils::download.file()]. Method to be
#' used for downloading files.  Current download methods are
#' ‘"internal"’, ‘"wininet"’ (Windows only) ‘"libcurl"’, ‘"wget"’ and ‘"curl"’,
#' and there is a value ‘"auto"’: see ‘Details’ and ‘Note’.
#' @export
download_covid_version <- function(data = c("inputDB", "Output_5", "Output_10",
                                            "qualityMetrics"), version,
                                   dest = getwd(), temp = FALSE,
                                   download_method = "auto",
                                   download_only = FALSE,
                                   return = c("data.frame", "data.table",
                                              "tibble"),
                                   progress = TRUE, ...) {
        # This function is needed because the osfr package backend
        # doesn't currently support retrieving specific file versions
        stopifnot(is.character(data))

        rinfo <- get_rinfo(data[1])
        stopifnot(!is.null(rinfo))

        stopifnot(is.numeric(version) || is.integer(version),
                  length(version) == 1)

        stopifnot(is.logical(temp), is.logical(download_only))
        if (temp && download_only) warning("'temp' set to TRUE, ",
                                           "ignoring 'download_only'")
        if (temp) {
                path <- tempdir()
        } else {
                stopifnot(is.character(dest), length(dest) == 1)
                path <- dest
        }

        url <- paste0("https://osf.io/", rinfo[[1]], "/download?version=",
                      version)

        filename    <- paste0(data[1], "_v", version, ".zip")
        zippath     <- file.path(path, filename)

        message("Downloading ", filename, " (timeout set to ",
                getOption("timeout"), ")")

        stopifnot(is.logical(progress))
        return_code <- utils::download.file(url, zippath, download_method,
                                            !progress)

        stopifnot(file.exists(zippath)) # The file was not downloaded or deleted
        if (temp) on.exit(unlink(zippath), add = TRUE) # Cleanup
        if (return_code) {
                stop("Download failed with return code ", return_code)
        }
        if (!temp && download_only) return(invisible(NULL))

        return(read_covid(zippath, data, return, ...))
}
