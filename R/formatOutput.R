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
#' @importFrom dplyr %>% select mutate rename filter pull case_when bind_rows group_by arrange 
#' @importFrom tidyr separate pivot_wider
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
            separate(col = .data$scenario, into = c("REMIND", "MAgPIE", "VARIABLE"), sep = "__", remove = TRUE)

        return(.d)
    }

    out <- out_files %>%
        map(.f = ~ .read_output(.x, rawOutput_dir)) %>%
        reduce(.f = bind_rows) %>%
        select(.data$REMIND, .data$MAgPIE, .data$VARIABLE, .data$YEARS, .data$GLOBAL) %>%
        rename(Year = .data$YEARS)

    out <- out %>%
        mutate(VARIABLE = case_when(
            VARIABLE == "DAT_CO2_CONC.OUT" ~ "AR6 climate diagnostics|Atmospheric Concentrations|CO2|MAGICCv7.5.3|Deterministic",
            VARIABLE == "DAT_SURFACE_TEMP.OUT" ~ "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|Deterministic",
            VARIABLE == "DAT_TOTAL_ANTHRO_RF.OUT" ~ "AR6 climate diagnostics|Effective Radiative Forcing|Basket|Anthropogenic|MAGICCv7.5.3|Deterministic"
        ))

    out <- out %>%
        mutate(UNIT = case_when(
            VARIABLE == "AR6 climate diagnostics|Atmospheric Concentrations|CO2|MAGICCv7.5.3|Deterministic" ~ "ppm",
            VARIABLE == "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|Deterministic" ~ "C",
            VARIABLE == "AR6 climate diagnostics|Effective Radiative Forcing|Basket|Anthropogenic|MAGICCv7.5.3|Deterministic" ~ "W/m^2"
        ))

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
        group_by(.data$VARIABLE) %>%
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
               Variable = .data$VARIABLE,
               Unit = .data$UNIT) %>%
        rename(Value = .data$GLOBAL) %>%
        filter(.data$Year %in% yearsToKeep) %>%
        select(.data$Model, .data$Scenario, .data$Variable, .data$Unit, .data$Year, .data$Value) %>%
        arrange(.data$Year) %>%
        pivot_wider(names_from = "Year", values_from = "Value")

    return(out)
}
