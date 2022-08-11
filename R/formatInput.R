#' @title formatInput
#' @description converts MAgPIE emissions data, combining it with user-selected REMIND emissions, into the format
#' necessary to run MAGICC7
#' @author Michael Crawford
#'
#' @param remindmif_path file path for the reference REMIND emissions
#' @param magpiemif_path file path for the MAgPIE emissions
#'
#' @return a composite data.frame containing the scenario's MAgPIE emissions and reference REMIND emissions
#'
#' @importFrom utils read.csv
#' @importFrom dplyr %>% filter select mutate contains group_by ungroup starts_with rename bind_rows summarise arrange
#' @importFrom stringr str_detect str_count str_extract str_replace str_split str_remove
#' @importFrom tidyr as_tibble pivot_longer unnest pivot_wider
#' @importFrom purrr pmap
#' @importFrom rlang .data
#'
#' @examples
#'   \dontrun{
#'     x <- formatInput(remindmif_path, magpiemif_path)
#'   }

formatInput <- function(remindmif_path, magpiemif_path) {

  #
  # Remind emissions
  #

  remind <- read.csv(remindmif_path, header = TRUE, sep = ";", stringsAsFactors = FALSE)

  # Filter for only World-level emissions
  remind <- remind %>%
    filter(str_detect(string = .data$Variable, pattern = "Emi\\|"),
           .data$Region == "World") %>%
    as_tibble()

  # Remove all emissions derived from MAgPIE
  remindEmissions_fromMagpie <- c("Emi|CO2|Land-Use Change",
                                  "Emi|CH4|Land Use",
                                  "Emi|N2O|Land Use")

  remind <- remind %>%
    filter(!.data$Variable %in% remindEmissions_fromMagpie) %>%
    mutate(Model = "REMIND")

  # Select only top-level emissions, or HFCs, and no emission-equivalents
  remind <- remind %>%
    filter(str_count(string = .data$Variable, pattern = "\\|") == 1 |
             str_detect(string = .data$Variable, pattern = "Emi\\|HFC\\|HFC")) %>%
    filter(str_detect(string = .data$Unit, pattern = "equiv", negate = TRUE))

  # Simplify Variable's reporting name to reflect the overall Unit
  remind <- remind %>%
    mutate(Variable = str_extract(string = .data$Unit, pattern = "(?<=\\s)(.+)(?=/)"))

  # Reformat to tibble-style
  remind <- remind %>%
    select(-.data$X) %>% # There is additional column called "X"
    pivot_longer(cols = contains("X"),
                 names_to = "Year",
                 values_to = "Value") %>%
    mutate(Year = str_remove(.data$Year, "X")) %>%
    mutate(Year = as.numeric(.data$Year),
           Value = as.numeric(.data$Value))

  .convertRemindUnits <- function(.variable, .unit, .value) {

    if (.unit == "Mt CO2/yr") {
      .variable <- "CO2I"
      .unit     <- "Gt C/yr"
      .value    <- .value * (12 / 44) * 0.001 # Convert to CO2 to C and Mt to Gt
    } else if (.unit == "Mt NOX/yr") {
      .unit  <- "Mt N/yr"
      .value <- .value * (14 / 46)
    } else if (.unit == "Mt NH3/yr") {
      .unit  <- "Mt N/yr"
      .value <- .value * (14 / 17)
    } else if (.unit == "kt N2O/yr") {
      .unit  <- "Mt N2ON/yr"
      .value <- .value * (28 / 44) * 0.0001 # Covert kt to Mt
    } else if (str_detect(string = .unit, pattern = "HFC")) {
      .variable <- toupper(.variable)
      .variable <- str_remove(.variable, "-")

      .unit <- .unit %>%
        str_replace(pattern = "/", replacement = " ") %>%
        str_split(pattern = " ") %>%
        unlist()

      .unit[2] <- toupper(.unit[2])
      .unit[2] <- str_remove(.unit[2], "-")

      .unit <- paste0(.unit[1], " ", .unit[2], "/", .unit[3])
    }
    return(data.frame(.variable, .unit, .value))

  }

  remind <- remind %>%
    ungroup() %>%
    mutate(vars = pmap(.l = list(.data$Variable, .data$Unit, .data$Value),
                       .f = ~ .convertRemindUnits(..1, ..2, ..3))) %>%
    select(-.data$Variable, -.data$Unit, -.data$Value) %>%
    unnest(.data$vars) %>%
    mutate(.variable = as.character(.data$.variable),
           .unit     = as.character(.data$.unit)) %>%
    rename(Variable = .data$.variable,
           Unit     = .data$.unit,
           Value    = .data$.value)

  #
  # MAgPIE emissions
  #

  magpie_emissionCategories <- c("Emissions|CO2|Land|+|Land-use Change",
                                 "Emissions|N2O|Land|Agriculture|+|Animal Waste Management",
                                 "Emissions|N2O|Land|Agriculture|Agricultural Soils|+|Inorganic Fertilizers",
                                 "Emissions|N2O|Land|Agriculture|Agricultural Soils|+|Manure applied to Croplands",
                                 "Emissions|N2O|Land|Agriculture|Agricultural Soils|+|Decay of Crop Residues",
                                 "Emissions|N2O|Land|Agriculture|Agricultural Soils|+|Soil Organic Matter Loss",
                                 "Emissions|N2O|Land|Agriculture|Agricultural Soils|+|Pasture",
                                 "Emissions|CH4|Land|Agriculture|+|Rice",
                                 "Emissions|CH4|Land|Agriculture|+|Animal waste management",
                                 "Emissions|CH4|Land|Agriculture|+|Enteric fermentation")

  magpie <- read.csv(magpiemif_path, header = TRUE, sep = ";", stringsAsFactors = FALSE) %>%
    filter(.data$Variable %in% magpie_emissionCategories,
           .data$Region == "World") %>%
    as_tibble()

  # Reformat to tibble-style
  magpie <- magpie %>%
    select(-.data$X) %>% # There is additional column called "X"
    pivot_longer(cols = starts_with("X"),
                 names_to = "Year",
                 values_to = "Value")

  magpie <- magpie %>%
    mutate(Year = str_remove(string = .data$Year, pattern = "X")) %>%
    filter(.data$Year %in% remind$Year) %>%
    mutate(Year = as.numeric(.data$Year),
           Value = as.numeric(.data$Value))

  # Sum across emission types
  magpie <- magpie %>%
    group_by(.data$Model, .data$Scenario, .data$Region, .data$Unit, .data$Year) %>%
    summarise(Value = sum(.data$Value))

  # Re-integrate top-level emission variable from the Unit for MAGICC's read-in
  magpie <- magpie %>%
    mutate(Variable = str_extract(string = .data$Unit, pattern = "(?<=\\s)(.+)(?=/)"))

  .convertMAgPIEUnits <- function(.variable, .unit, .value) {
    if (.unit == "Mt CO2/yr") {
      .variable <- "CO2B"
      .unit     <- "Gt C/yr"
      .value    <- .value * (12 / 44) * 0.001 # Convert to CO2 to C and Mt to Gt
    } else if (.unit == "Mt N2O/yr") {
      .unit  <- "Mt N2ON/yr"
      .value <- .value * (28 / 44)
    }
    return(data.frame(.variable, .unit, .value))
  }

  magpie <- magpie %>%
    ungroup() %>%
    mutate(vars = pmap(.l = list(.data$Variable, .data$Unit, .data$Value),
                       .f = ~ .convertMAgPIEUnits(..1, ..2, ..3))) %>%
    select(-.data$Variable, -.data$Unit, -.data$Value) %>%
    unnest(.data$vars) %>%
    mutate(.variable = as.character(.data$.variable),
           .unit     = as.character(.data$.unit)) %>%
    rename(Variable = .data$.variable,
           Unit     = .data$.unit,
           Value    = .data$.value)

  #
  # Integrate the REMIND emissions and the new MAgPIE emissions
  #

  all <- bind_rows(remind, magpie) %>%
    filter(.data$Year %in% unique(magpie$Year)) %>%
    mutate(Unit = str_replace(string = .data$Unit, pattern = " ", replacement = "_"),
           Unit = str_replace(string = .data$Unit, pattern = "/", replacement = "_per_"))

  if (length(unique(remind$Scenario)) > 1 || length(unique(magpie$Scenario)) > 1) {
    stop("blackmagicc::formatInput was called on a report containing more than one scenario.\n
              This function should only be called on original reports, not merged reports.")
  }

  scenario <- paste0(unique(remind$Scenario), "__", unique(magpie$Scenario))

  all <- all %>%
    group_by(.data$Region, .data$Variable, .data$Unit, .data$Year) %>%
    summarise(Value = sum(.data$Value)) %>%
    mutate(Model = "REMIND-MAgPIE", Scenario = scenario) %>%
    select(.data$Model, .data$Region, .data$Scenario, .data$Unit, .data$Variable, .data$Year, .data$Value) %>%
    arrange(.data$Variable) %>% # Order of the variables matters after the pivot!
    pivot_wider(names_from = c("Variable", "Unit"), values_from = "Value", names_sep = " ")

  # Ensure that the MAGICC data is used during the historical period, ensuring a better pattern
  # during the historical period. The end warming (i.e. year 2099) is left unaffected regardless.
  all <- all %>% filter(.data$Year > 2014)

  return(all)
}
