#' @title runMAGICC
#' @description runs the MAGICC7 model on a specific emissions scenario
#' @author Michael Crawford
#'
#' @param emissions_fn file name for the emissions (held in raw/input)
#' @param base_path root file path (the temporary directory)
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace_all str_extract_all str_remove_all
#' @importFrom readr read_file write_file read_lines
#' @importFrom withr with_dir
#'
#' @examples
#'   \dontrun{
#'     x <- runMAGICC(emissions_fn, base_path)
#'   }

runMAGICC <- function(emissions_fn, base_path) {

    rawInput_dir  <- file.path(base_path, "raw", "input")
    rawOutput_dir <- file.path(base_path, "raw", "output")
    magiccBin_dir <- file.path(base_path, "magicc-v7.5.3", "bin")
    magiccRun_dir <- file.path(base_path, "magicc-v7.5.3", "run")
    magiccOut_dir <- file.path(base_path, "magicc-v7.5.3", "out")

    # Customize MAGICC user configuration template to reflect the specific scenario
    user_config_template <- read_file(file.path(base_path, "MAGCFG_USER_TEMPLATE.CFG"))
    user_config <- user_config_template %>%
        str_replace_all(pattern = "DEFAULT", replacement = emissions_fn) %>%
        str_replace_all(pattern = "FILEPATH_IN",  replacement = paste0(magiccRun_dir, "/")) %>% # Must have ending `/`
        str_replace_all(pattern = "FILEPATH_OUT", replacement = paste0(magiccOut_dir, "/"))     # Must have ending `/`

    write_file(x = user_config, file = file.path(magiccRun_dir, "MAGCFG_USER.CFG"))

    # Copy emissions_file from the file with derived magicc input files to the magicc/run folder
    file.copy(from = file.path(rawInput_dir,  paste0(emissions_fn, ".SCEN7")),
              to   = file.path(magiccRun_dir, paste0(emissions_fn, ".SCEN7")))

    if (length(list.files(magiccOut_dir)) > 0 ) {
        file.remove(list.files(magiccOut_dir)) # Ensure that the magicc/out dir is empty
    }

    # Run MAGICC7
    withr::with_dir(magiccBin_dir, {
        system("chmod +x magicc")
        error <- system("./magicc 2>&1", intern = TRUE)

        # Ensure that MAGICC is not throwing unanticipated errors.
        error <- error %>%
            str_extract_all(pattern = "<ERROR>.*\n", simplify = TRUE) %>%
            str_remove_all(pattern = "[\r\n]")
        sunny_day_error <- read_lines(file = file.path(base_path, "magicc7_normalErrors.txt"))
        if (any(error != sunny_day_error)) {
            stop("MAGICC is throwing unexpected errors. Its output is unreliable.")
        }
    })

    # Rename and then move the output file from magic-v7.5.3c/out to raw/output
    file.copy(from = file.path(magiccOut_dir, "DAT_SURFACE_TEMP.OUT"),
              to   = file.path(rawOutput_dir, paste0(emissions_fn, ".out")))

    # Delete the various intermediate files within the magicc directories
    file.remove(file.path(magiccRun_dir, paste0(emissions_fn, ".SCEN7")))
    file.remove(file.path(magiccRun_dir, "MAGCFG_USER.CFG"))
    file.remove(file.path(magiccOut_dir, "DAT_SURFACE_TEMP.OUT"))

    # TODO As of now the PARAMETERS output file is not requested, though it can be useful for debugging. Future
    # iterations may want to include these files optionally.

}
