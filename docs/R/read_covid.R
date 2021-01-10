#' Reads one of the COVerAGE-DB datasets that was locally saved. The advantage
#' over the usual reading functions, is that the columns are ensured to have
#' their correct data types according to COVerAGE-DB documentation.
#'
#' This function can be used to read a previously downloaded datasets. However,
#' to get the latest version, it's better to use [download_covid()].
#' Reading the complete 'inputDB' is quite memory intensive. It may fail
#' if *R* cannot allocate enough memory for the dataset.
#' @title Read downloaded COVerAGE-DB datasets
#' @param zippath Character. The local zip archive of the downloaded dataset.
#' @param data The name of the dataset that is to be read. Can be one of the
#' the following: "inputDB", "Output_5", "Output_10", "qualityMetrics".
#' @param return What should be the return type? Can be on of the following:
#' "data.frame", "data.table", "tibble".
#' @param ... Additional named arguments passed to [data.table::fread()]
#' @return By default a data frame with the uncompressed dataset.
#' Can be set to return either a data table or
#' a tibble. The return type is controlled by the 'return' parameter.
#'
#' @export
read_covid <- function(zippath, data = c("inputDB", "Output_5", "Output_10",
                                          "qualityMetrics"),
                                          return = c("data.frame", "data.table",
                                                     "tibble"), ...) {
        stopifnot(is.character(data), is.character(zippath),
                  length(zippath) == 1)
        rinfo <- get_rinfo(data[1])
        stopifnot(!is.null(rinfo))

        archivefile <- file.path("Data", paste0(data[1], ".csv"))
        filepath    <- utils::unzip(zippath, archivefile, exdir = tempdir())
        on.exit(unlink(filepath), add = TRUE) # Cleanup

        message("Reading ", filepath)
        out <- data.table::fread(filepath, sep = ",",
                                 colClasses = rinfo[[2]],
                                 skip = rinfo[[3]], ...)

        return <- return[1]
        switch(return,
               data.frame = return(collapse::qDF(out)),
               data.table = return(out),
               tibble     = return(collapse::qTBL(out)))

        warning("Invalid return type specified, returning data.frame")
        return(collapse::qDF(out))
}
