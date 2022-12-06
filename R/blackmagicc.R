#' @title blackmagicc
#' @description Runs MAGICC-v7.5.3 on a given MAgPIE report.mif, optionally with custom REMIND reference emissions.
#' @author Michael Crawford
#'
#' @export
#'
#' @param dir the MAgPIE scenario output directory that contains the report.mif and config.yml
#' @param remind_name name of the desired reference scenario report.mif for REMIND. May or may not include
#' the `.mif` extension. If NULL, blackmagicc will try to read the scenario from the MAgPIE run's config.yml.
#' If a custom REMIND scenario is desired, it should be placed within `dir`.
#' @param save_MAGICC_io print intermediate input, output, and settings from MAGICC into the output folder
#'
#' These REMIND scenarios are packaged within blackmagicc:
#'      bjoernAR6_C_RemSDP-900-MagSSP1.mif
#'      bjoernAR6_C_RemSSP1-1000-MagSDP.mif
#'      bjoernAR6_C_SDP-NPi.mif
#'      bjoernAR6_C_SDP-PkBudg1000.mif
#'      bjoernAR6_C_SSP1-Base.mif
#'      bjoernAR6_C_SSP1-NDC.mif
#'      bjoernAR6_C_SSP1-NPi.mif
#'      bjoernAR6_C_SSP1-PkBudg900.mif
#'      bjoernAR6_C_SSP2-Base.mif
#'      bjoernAR6_C_SSP2-NDC.mif
#'      bjoernAR6_C_SSP2-NPi.mif
#'      bjoernAR6_C_SSP2-PkBudg900.mif
#' For more information about these scenarios' assumptions, see `https://www.nature.com/articles/s41558-021-01098-3`
#' @param append append the global surface temperature from MAGICC onto the report.mif and report.rds?
#'
#' @return a magpie object containing the MAGICC warming pathway
#'
#' @importFrom gms loadConfig
#' @importFrom stringr str_detect str_remove
#' @importFrom withr local_tempdir
#' @importFrom utils untar
#' @importFrom dplyr %>% first
#' @importFrom magclass read.report getYears as.magpie getItems write.report
#' @importFrom quitte as.quitte
#'
#' @examples
#'   \dontrun{
#'     x <- blackmagicc()
#'   }

blackmagicc <- function(dir = ".", remind_name = NULL, append = FALSE, save_MAGICC_io = TRUE) {

    scenarioConfig <- gms::loadConfig(file.path(dir, "config.yml"))

    blackmagicc_dir <- NULL
    if (save_MAGICC_io) {
        blackmagicc_dir <- file.path(dir, "..", "blackmagicc")
        if (!dir.exists(blackmagicc_dir)) {
            dir.create(blackmagicc_dir)
        } else {
            message("These scenarios already exist in the blackmagicc intermediates folder.
            Re-writing over old inputs.")
        }
    }

    tmpdir <- withr::local_tempdir()
    untar("/p/projects/magpie/magicc-v7.5.3.tgz", exdir = tmpdir)

    if (is.null(remind_name)) {
        remind_name <- scenarioConfig[["magicc_emis_scen"]]

        if (remind_name == "NULL" || is.null(remind_name)) {
            message("This MAgPIE scenario does not specify a REMIND emission scenario as a parameter
            or in its config.yml")
            message("Done!")
            return()
        }
    }

    if (str_detect(string = remind_name, pattern = "\\..*")) {
        remind_name <- str_remove(string = remind_name, pattern = "\\..*")
    }

    message("Finding a REMIND emission scenario with the name: ", remind_name)

    remind_defaultDir <- file.path(tmpdir, "default_remind_datasets")
    remind_potentialPaths <- c(file.path(dir, paste0(remind_name, ".mif")),
                               file.path(remind_defaultDir, paste0(remind_name, ".mif")))

    remindmif_path <- remind_potentialPaths[file.exists(remind_potentialPaths)] %>% first()

    if (is.null(remindmif_path)) {
        stop("Neither the MAgPIE scenario's output directory nor the default REMIND emissions directory
              contain a report.mif with that name. Please note that so far .rds files are not supported.")
    }

    magpiemif_path <- file.path(dir, "report.mif")

    if (is.null(magpiemif_path)) {
        stop("No report.mif found in the MAgPIE scenario's output directory. Please note that so far .rds
              files are not supported.")
    }

    emissions <- formatInput(remindmif_path, magpiemif_path, blackmagicc_dir)

    rawInput_dir <- file.path(tmpdir, "raw", "input")
    writeInput(emissions, rawInput_dir)

    emissions_files <- list.files(rawInput_dir) %>% gsub(pattern = ".SCEN7$", replacement = "")
    if (length(emissions_files) > 1) {
        stop("MAGICC raw/input directory should not have more than one file in it")
    }
    runMAGICC(emissions_files, tmpdir)

    originalReport <- read.report(magpiemif_path, as.list = FALSE)
    yearsToKeep <- str_remove(string = getYears(originalReport), pattern = "y")

    rawOutput_dir <- file.path(tmpdir, "raw", "output")
    warmingOutput <- formatOutput(rawOutput_dir, yearsToKeep)
    warmingOutput <- as.magpie(warmingOutput)

    if (append) {

        oldWarmingVariables <- str_detect(getItems(originalReport, dim = 3), getItems(warmingOutput, dim = 3.3))
        if (any(oldWarmingVariables)) {
            message("Global Surface Temperature was already found in your report.mif. Removing those and
            adding the new ones.")
            originalReport <- originalReport[, , !oldWarmingVariables]
        }

        write.report(x = originalReport, file = magpiemif_path)
        write.report(x = warmingOutput, file = magpiemif_path, append = TRUE)

        toSaveAsRDS <- read.report(magpiemif_path, as.list = FALSE)
        toSaveAsRDS <- as.quitte(toSaveAsRDS)
        magpieRDS_path <- file.path(dir, "report.rds")
        saveRDS(toSaveAsRDS, file = magpieRDS_path, version = 2)

    }

    message("Done!")

    return(warmingOutput)
}
