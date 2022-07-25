#' @title formatOutput
#' @description formats the MAGICC7 raw output files
#' @author Michael Crawford
#'
#' @param rawOutput_dir output directory containing the raw output from the MAGICC model
#' @param yearsToKeep vector of years to keep from MAGICC (which by default reports from 1750 on a yearly basis).
#'
#' @return a data.frame containing all the emissions scenarios' MAGICC7 warming pathways
#'
#' @importFrom readr read_table
#' @importFrom dplyr %>% select mutate rename filter
#' @importFrom tidyr separate
#' @importFrom stringr str_remove
#' @importFrom purrr map reduce
#'
#' @examples
#'   \dontrun{
#'     x <- formatOutput(rawOutput_dir)
#'   }

formatOutput <- function(rawOutput_dir, yearsToKeep) {

    out_files <- list.files(rawOutput_dir)

    .read_output <- function(fn, rawOutput_dir) {
        .d <- suppressMessages(read_table(file = file.path(rawOutput_dir, fn), col_names = TRUE, skip = 21))

        .d <- .d %>%
            select(YEARS, GLOBAL) %>%
            mutate(scenario = str_remove(string = fn, pattern = ".out")) %>%
            separate(col = scenario, into = c("REMIND", "MAgPIE"), sep = "__", remove = TRUE)

        return(.d)
    }

    out <- out_files %>%
        map(.f = ~ .read_output(.x, rawOutput_dir)) %>%
        reduce(.f = bind_rows) %>%
        select(REMIND, MAgPIE, YEARS, GLOBAL) %>%
        rename(Year = YEARS)

    # From MAGCFG_NMLYEARS.CFG:
    #   ! MAGICC does strange things in the last year of the run
    #   ! so we recommend running 5 years more than needed and
    #   ! cutting the output to get sensible results
    out <- out %>%
        mutate(GLOBAL = ifelse(Year < (max(Year) - 5), 
                               GLOBAL,
                               NA))

    # Format for .mif
    out <- out %>%
        mutate(Model = "MAgPIE",
               Scenario = paste0(MAgPIE, "--", REMIND),
               Region = "GLO",
               Variable = "GlobalSurfaceTemperature",
               Unit = "C") %>%
        rename(Value = GLOBAL) %>%
        filter(Year %in% yearsToKeep) %>%
        select(Model, Scenario, Variable, Unit, Year, Value) %>%
        arrange(Year) %>%
        pivot_wider(names_from = "Year", values_from = "Value")

    return(out)
}
