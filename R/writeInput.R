#' @title writeInput
#' @description writes the composite emissions scenario to a temporary folder, formatted to be read by MAGICC7
#' @author Michael Crawford
#'
#' @param inputEmissions data.frame representing the emissions to be written to disk and used by MAGICC
#' @param rawInput_dir directory in which to write the formatted .SCEN file (raw/input)
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_split
#' @importFrom readr write_delim
#'
#' @examples
#'   \dontrun{
#'     x <- formatInput(magpiemif_path, remindEmissions_path)
#'   }

writeInput <- function(inputEmissions, rawInput_dir) {
    
    gases <- colnames(inputEmissions)[5:length(colnames(inputEmissions))] %>%
        str_split(pattern = " ", simplify = TRUE)

    fn <- file.path(rawInput_dir, paste0(unique(inputEmissions$Scenario), ".SCEN7"))
    file.create(fn)

    # inputEmissions formatting must be exact in this header
    cat("---- HEADER ----\n\n",                                       file = fn, sep = " ", append = T)
    cat("Combined scenario:", unique(inputEmissions$Scenario), "\n",  file = fn, sep = " ", append = T)
    cat("\n",                                                         file = fn, sep = " ", append = T)

    cat("---- METADATA ----\n",                                       file = fn, sep = " ", append = T)
    cat("\n",                                                         file = fn, sep = " ", append = T)
    cat("Source: PIK,", format(Sys.time()), "\n",                     file = fn, sep = " ", append = T) # TODO if these files are eventually kept, it would be helpful to have a better source here.
    cat("\n",                                                         file = fn, sep = " ", append = T)
    cat("&THISFILE_SPECIFICATIONS\n",                                 file = fn, sep = " ", append = T)
    cat(c("THISFILE_DATACOLUMNS =", dim(gases)[[1]] - 1, "\n"),       file = fn, sep = " ", append = T) # off-by-one error
    cat(c("THISFILE_DATAROWS =", dim(inputEmissions)[[1]], "\n"),     file = fn, sep = " ", append = T) # But no OBO error here
    cat(c("THISFILE_FIRSTYEAR =", min(inputEmissions$Year), "\n"),    file = fn, sep = " ", append = T)
    cat(c("THISFILE_LASTYEAR =", max(inputEmissions$Year), "\n"),     file = fn, sep = " ", append = T)
    # Time step doesn't seem to matter, but must be an integer
    cat(c("THISFILE_ANNUALSTEPS =", "1", "\n"),                       file = fn, sep = " ", append = T)
    cat(c("THISFILE_UNITS =", "'MISC'", "\n"),                        file = fn, sep = " ", append = T)
    cat(c("THISFILE_DATTYPE =", "'SCEN7'", "\n"),                     file = fn, sep = " ", append = T)
    cat(c("THISFILE_REGIONMODE =", "'WORLD'", "\n"),                  file = fn, sep = " ", append = T)
    cat(c("THISFILE_FIRSTDATAROW =", "25", "\n"),                     file = fn, sep = " ", append = T)
    cat("/\n",                                                        file = fn, sep = " ", append = T)
    cat("\n",                                                         file = fn, sep = " ", append = T)

    cat(c("GAS", gases[,1], "\n"),                                    file = fn, sep = " ", append = T)
    cat(c("TODO", rep("SET", length(gases[,1]) - 1),  "\n"),          file = fn, sep = " ", append = T)
    cat(c("UNITS", gases[,2], "\n"),                                  file = fn, sep = " ", append = T)
    cat(c("YEARS", rep("WORLD", length(gases[,1]) - 1),  "\n"),       file = fn, sep = " ", append = T)

    write_delim(inputEmissions[, 4:length(inputEmissions)], col_names = FALSE, file = fn, append = T, delim = " ")
    
}
