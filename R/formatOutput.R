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
#' @importFrom dplyr %>% select mutate rename filter pull
#' @importFrom tidyr separate
#' @importFrom stringr str_remove
#' @importFrom purrr map reduce
#' @importFrom rlang .data
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
            select(.data$YEARS, .data$GLOBAL) %>%
            mutate(scenario = str_remove(string = fn, pattern = ".out")) %>%
            separate(col = .data$scenario, into = c("REMIND", "MAgPIE"), sep = "__", remove = TRUE)

        return(.d)
    }

    out <- out_files %>%
        map(.f = ~ .read_output(.x, rawOutput_dir)) %>%
        reduce(.f = bind_rows) %>%
        select(.data$REMIND, .data$MAgPIE, .data$YEARS, .data$GLOBAL) %>%
        rename(Year = .data$YEARS)

    if (length(unique(out$REMIND)) != 1) {
        stop("At this time, blackmagicc is configured to only process one REMIND scenario per MAgPIE scenario.")
    }

    # From MAGCFG_NMLYEARS.CFG:
    #   ! MAGICC does strange things in the last year of the run
    #   ! so we recommend running 5 years more than needed and
    #   ! cutting the output to get sensible results
    # For visualization purposes, we replicate the last usable year
    # from MAGICC for this period, as otherwise y2100 is NA and our plots
    # break.

    replicatedLastYear <- out %>%
        filter(.data$Year == (max(.data$Year) - 6)) %>%
        pull(.data$GLOBAL)

    out <- out %>%
        mutate(GLOBAL = ifelse(.data$Year > (max(.data$Year) - 5),
                               replicatedLastYear,
                               .data$GLOBAL))

    # Format for .mif
    out <- out %>%
        mutate(Model = "MAgPIE",
               Scenario = .data$MAgPIE,
               Region = "GLO",
               Variable = "Global Surface Temperature",
               Unit = "C") %>%
        rename(Value = .data$GLOBAL) %>%
        filter(.data$Year %in% yearsToKeep) %>%
        select(.data$Model, .data$Scenario, .data$Variable, .data$Unit, .data$Year, .data$Value) %>%
        arrange(.data$Year) %>%
        pivot_wider(names_from = "Year", values_from = "Value")

    return(out)
}
