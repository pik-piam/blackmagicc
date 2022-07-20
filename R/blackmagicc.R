#' @title blackmagicc
#' @description sets up and then runs MAGICC-v7.5.3 on a given MAgPIE report.mif, optionally with custom REMIND
#' reference emissions.
#' @author Michael Crawford
#'
#' @export
#'
#' @param magpiemif_path file path of the report.mif from an arbitrary MAgPIE scenario
#' @param remindEmissions_paths a list of file paths to emissions from REMIND. Both .mif and .RDS formats are accepted.
#' If none are supplied, the default REMIND reference emissions will be used.
#' @param dir the MAgPIE scenario output directory
#' @return a data.frame containing the MAGICC warming pathways for the union of the MAgPIE and REMIND emissions
#'
#' @importFrom withr local_tempdir
#' @importFrom utils untar
#' @importFrom dplyr %>%
#' @importFrom purrr map walk
#'
#' @examples
#'   \dontrun{
#'     x <- blackmagicc(magpiemif_path, remindEmissions_paths)
#'   }

blackmagicc <- function(magpiemif_path, remindEmissions_paths = NULL) {

    message("Creating temporary directory and extracting MAGICC-v7.5.3")
    tmpdir <- withr::local_tempdir()
    untar("/p/projects/magpie/magicc-v7.5.3.tgz", exdir = tmpdir)

    if (is.null(remindEmissions_paths)) {
        remindEmissions_dir   <- file.path(tmpdir, "default_remind_datasets")
        remindEmissions_paths <- list.files(remindEmissions_dir, pattern = ".mif", full.names = TRUE)
        # remindEmissions_paths <- list.files(remindEmissions_dir, pattern = ".RDS", full.names = TRUE)
    }

    message("Combining REMIND reference emissions with the updated MAgPIE emissions")
    emissions <- map(.x = remindEmissions_paths, .f = ~ formatInput(.x, magpiemif_path))

    message("Writing formatted MAGICC7 .SCEN files to raw/input")
    rawInput_dir <- file.path(tmpdir, "raw", "input")
    walk(.x = emissions, .f = ~ writeInput(.x, rawInput_dir))

    message("Running MAGICC7")
    emissions_fn <- list.files(rawInput_dir) %>% gsub(pattern = ".SCEN7$", replacement = "")
    walk(.x = emissions_fn, .f = ~ runMAGICC(.x, tmpdir))

    # Turn raw output into a convenient format
    message("Formatting raw output")
    rawOutput_dir <- file.path(tmpdir, "raw", "output")
    emissions_output <- formatOutput(rawOutput_dir)

    message("Done!")
    return(emissions_output)
}
