#' @title blackmagicc
#' @description Runs MAGICC-v7.5.3 on a given MAgPIE report.mif, optionally with custom REMIND
#' reference emissions.
#' @author Michael Crawford
#'
#' @export
#'
#' @param remindEmissions_name name of the desired reference scenario for REMIND. May or may not include file endings `.mif` 
#' or `.rds`. If NULL, all default reference scenarios are used.
#' @param dir the MAgPIE scenario output directory that contains the report.mif
#' @param append append the global surface temperature from MAGICC onto the report.mif and report.rds?
#' @return a magpie object containing the MAGICC warming pathways for the union of the MAgPIE and REMIND emissions
#'
#' @importFrom stringr str_detect str_remove
#' @importFrom withr local_tempdir
#' @importFrom utils untar
#' @importFrom dplyr %>% first pull
#' @importFrom purrr map walk
#' @importFrom magclass read.report getYears as.magpie 
#'
#' @examples
#'   \dontrun{
#'     x <- blackmagicc()
#'   }

blackmagicc <- function(remindEmissions_name = NULL, dir = ".", append = FALSE) {

    magpie_mif <- file.path(dir, paste0("report.mif"))
    magpie_rds <- file.path(dir, paste0("report.rds"))

    if (!file.exists(magpie_mif)) {
        stop("No report.mif found in the MAgPIE Scenario's output directory.")
    }

    message("Creating temporary directory and extracting MAGICC-v7.5.3")
    tmpdir <- withr::local_tempdir()
    untar("/p/projects/magpie/magicc-v7.5.3.tgz", exdir = tmpdir)

    remindDefaultEmissions_dir <- file.path(tmpdir, "default_remind_datasets")
    if (is.null(remindEmissions_name)) {
        remindEmissions_paths <- list.files(remindDefaultEmissions_dir, pattern = ".mif", full.names = TRUE)
        # remindEmissions_paths <- list.files(remindDefaultEmissions_dir, pattern = ".RDS", full.names = TRUE)
    } else {
        if (str_detect(string = remindEmissions_name, pattern = "\\..*")) {
            remindEmissions_name = str_remove(string = remindEmissions_name, pattern = "\\..*")
        }

        remindEmissions_paths <- list.files(path = c(dir, remindDefaultEmissions_dir),
                                            pattern = remindEmissions_name,
                                            full.names = TRUE) %>%
                                 first()
    }

    message("Combining REMIND reference emissions with the updated MAgPIE emissions")
    emissions <- map(.x = remindEmissions_paths, .f = ~ formatInput(.x, magpie_mif))

    message("Writing formatted MAGICC7 .SCEN files to raw/input")
    rawInput_dir <- file.path(tmpdir, "raw", "input")
    walk(.x = emissions, .f = ~ writeInput(.x, rawInput_dir))

    message("Running MAGICC7")
    emissions_fn <- list.files(rawInput_dir) %>% gsub(pattern = ".SCEN7$", replacement = "")
    walk(.x = emissions_fn, .f = ~ runMAGICC(.x, tmpdir))

    message("Formatting raw output")
    originalReport <- read.report(magpie_mif, as.list = FALSE) 
    yearsToKeep <- str_remove(string = getYears(originalReport), pattern = "y")

    rawOutput_dir <- file.path(tmpdir, "raw", "output")
    warmingOutput <- formatOutput(rawOutput_dir, yearsToKeep)
    warmingOutput <- as.magpie(warmingOutput)

    if (append)
    {

        message("Appending warming variables to ", magpie_mif, " and ", magpie_rds)
        oldWarmingVariables <- str_detect(getItems(originalReport, dim = 3), warmingOutput$Variable)
        if (any(oldWarmingVariables)) {
            message("Global Surface Temperature was already found in your report.mif. Removing those and adding the new ones.")
            originalReport <- originalReport[,,!oldWarmingVariables]
        }

        write.report(x = originalReport, file = magpie_mif)
        write.report(x = warmingOutput, file = magpie_mif, append = T)

        toSaveAsRDS <- read.report(magpie_mif)
        saveRDS(toSaveAsRDS, file = magpie_rds)
    }

    message("Done!")
    return(warmingOutput)
}
