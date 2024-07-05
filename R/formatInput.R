#' @title formatInput
#' @description converts MAgPIE emissions data, combining it with user-selected REMIND emissions, into the format
#' necessary to run MAGICC7
#' @author Michael Crawford
#'
#' @param remindmifPath file path for the reference REMIND emissions
#' @param magpiemifPath file path for the MAgPIE emissions
#' @param blackmagiccDir directory to save intermediate inputs used by MAGICC
#'
#' @return a composite data.frame containing the scenario's MAgPIE emissions and reference REMIND emissions
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @examples
#'   \dontrun{
#'     x <- formatInput(remindmifPath, magpiemifPath, blackmagiccDir)
#'   }

formatInput <- function(remindmifPath, magpiemifPath, blackmagiccDir = NULL) {

  #
  # REMIND emissions
  #

  # Emissions from REMIND, omitting these MAgPIE emissions, to be added back in from the magpie .mif
  # Emi|CH4|Land-Use Change|+|Peatland
  # Emi|N2O|+|Agriculture
  # Emi|CO2|+|Land-Use Change
  # Emi|CH4|+|Agriculture
  remindEmissions <- c(
    "Emi|BC",
    # "Emi|C2F6",
    # "Emi|C6F14",
    # "Emi|CF4",
    "Emi|CH4|+|Energy Supply",
    "Emi|CH4|+|Extraction",
    "Emi|CH4|+|Waste",
    "Emi|CH4|Land-Use Change|+|Forest Burning",
    "Emi|CH4|Land-Use Change|+|Savanna Burning",
    "Emi|CO",
    "Emi|CO2|+|Energy",
    "Emi|CO2|+|Industrial Processes",
    "Emi|CO2|+|non-BECCS CDR",
    # "Emi|HFC",
    "Emi|N2O|+|Energy Supply",
    "Emi|N2O|+|Industry",
    "Emi|N2O|+|Land-Use Change",
    "Emi|N2O|+|Transport",
    "Emi|N2O|+|Waste",
    "Emi|NH3",
    "Emi|NOX",
    "Emi|OC",
    # "Emi|PFC",
    # "Emi|SF6",
    "Emi|SO2",
    "Emi|VOC"
  )

  remind <- utils::read.csv(remindmifPath, header = TRUE, sep = ";", stringsAsFactors = FALSE) %>%
    tidyr::as_tibble()

  # Check if all required REMIND emissions are present
  missingRemindEmissions <- setdiff(remindEmissions, unique(remind$Variable))
  if (length(missingRemindEmissions) > 0) {
    stop(paste("The following expected REMIND emissions are missing from the dataset:\n",
               paste(missingRemindEmissions, collapse = "\n")))
  }

  remind <- remind %>%
    dplyr::filter(.data$Variable %in% remindEmissions) %>%
    dplyr::filter(.data$Region == "World")

  remind <- remind %>%
    dplyr::mutate(Model = "REMIND") %>%
    dplyr::mutate(Variable = stringr::str_replace(.data$Variable, "^Emi\\|", "Emissions|"))

  # Reformat to tibble-style
  remind <- remind %>%
    dplyr::select(-.data$X) %>% # There is an additional column called "X"
    tidyr::pivot_longer(cols = dplyr::contains("X"), names_to = "Year", values_to = "Value") %>%
    dplyr::mutate(Year = stringr::str_remove(.data$Year, "X")) %>%
    dplyr::mutate(Year = as.numeric(.data$Year),
                  Value = as.numeric(.data$Value))

  # Convert kt N2O/yr to Mt N2O/yr
  remind <- remind %>%
    dplyr::mutate(Value = dplyr::if_else(.data$Unit == "kt N2O/yr", .data$Value / 1000, .data$Value),
                  Unit = dplyr::if_else(.data$Unit == "kt N2O/yr", "Mt N2O/yr", .data$Unit))

  #
  # New MAgPIE emissions
  #

  magpieEmissionCategories <- c("Emissions|CO2|Land|+|Land-use Change",
                                "Emissions|N2O|Land|+|Agriculture",
                                "Emissions|CH4|Land|+|Agriculture",
                                "Emissions|CH4|Land|+|Peatland")

  magpie <- utils::read.csv(magpiemifPath, header = TRUE, sep = ";", stringsAsFactors = FALSE) %>%
    tidyr::as_tibble()

  # Check if all required MAgPIE emissions are present
  missingMagpieEmissions <- setdiff(magpieEmissionCategories, unique(magpie$Variable))
  if (length(missingMagpieEmissions) > 0) {
    stop(paste("The following MAgPIE emissions are missing from the dataset:\n",
               paste(missingMagpieEmissions, collapse = "\n")))
  }

  magpie <- magpie %>%
    dplyr::filter(.data$Variable %in% magpieEmissionCategories,
                  .data$Region == "World")

  # Reformat to tibble-style
  magpie <- magpie %>%
    dplyr::select(-.data$X) %>% # There is an additional column called "X"
    tidyr::pivot_longer(cols = dplyr::starts_with("X"), names_to = "Year", values_to = "Value") %>%
    dplyr::mutate(Year = stringr::str_remove(string = .data$Year, pattern = "X")) %>%
    dplyr::filter(.data$Year %in% remind$Year) %>%
    dplyr::mutate(Year = as.numeric(.data$Year),
                  Value = as.numeric(.data$Value))

  #
  # Integrate the REMIND emissions and the new MAgPIE emissions
  #

  if (length(unique(remind$Scenario)) > 1 || length(unique(magpie$Scenario)) > 1) {
    stop("blackmagicc::formatInput was called on a report containing more than one scenario.\n
              This function should only be called on original reports, not merged reports.")
  }

  allData <- dplyr::bind_rows(remind, magpie) %>%
    dplyr::filter(.data$Year %in% unique(magpie$Year))

  # Ensure that the MAGICC data is used during the historical period, ensuring a better pattern
  # during the historical period. The end warming (i.e. year 2099) is left unaffected regardless.
  allData <- allData %>% dplyr::filter(.data$Year > 2014)

  scenario <- paste0(unique(remind$Scenario), "__", unique(magpie$Scenario))

  allData <- allData %>% dplyr::mutate(Model = "REMIND-MAgPIE", Scenario = scenario)

  nonCO2 <- allData %>%
    dplyr::filter(!stringr::str_detect(.data$Variable, "Emissions\\|CO2")) %>%
    dplyr::mutate(TopLevel = stringr::str_extract(.data$Variable, "^Emissions\\|[^|]+")) %>%
    dplyr::group_by(.data$Model, .data$Scenario, .data$Region, .data$TopLevel, .data$Unit, .data$Year) %>%
    dplyr::summarise(Value = sum(.data$Value, na.rm = TRUE)) %>%
    dplyr::rename(Variable = .data$TopLevel)

  co2 <- allData %>%
    dplyr::filter(stringr::str_detect(.data$Variable, "Emissions\\|CO2")) %>%
    dplyr::mutate(TopLevel = dplyr::if_else(stringr::str_detect(.data$Variable, "Land-use Change"),
                                            "Emissions|CO2|AFOLU",
                                            "Emissions|CO2|Energy and Industrial Processes")) %>%
    dplyr::group_by(.data$Model, .data$Scenario, .data$Region, .data$TopLevel, .data$Unit, .data$Year) %>%
    dplyr::summarise(Value = sum(.data$Value, na.rm = TRUE)) %>%
    dplyr::rename(Variable = .data$TopLevel)

  allData <- dplyr::bind_rows(co2, nonCO2)

  # Write IAMC compatible .csv file
  if (!is.null(blackmagiccDir)) {
    iamcXlsxDir <- file.path(blackmagiccDir, "IAMC_csvs")
    dir.create(iamcXlsxDir, showWarnings = FALSE)

    iamcXlsx <- quitte::as.quitte(allData)
    quitte::write.IAMCxlsx(iamcXlsx, path = file.path(iamcXlsxDir, paste0(scenario, ".csv")))
  }

  #
  # Generate .scen file for deterministic MAGICC run
  #

  allData <- allData %>%
    dplyr::mutate(Variable = dplyr::case_when(
      Variable == "Emissions|CO2|AFOLU" ~ "CO2B",
      Variable == "Emissions|CO2|Energy and Industrial Processes" ~ "CO2I",
      TRUE ~ sub("Emissions\\|", "", Variable)
    ))

  .convertUnits <- function(unit, value) {
    if (unit == "Mt CO2/yr") {
      unit <- "Gt C/yr"
      value <- value * (12 / 44) * 0.001 # Convert to CO2 to C and Mt to Gt
    } else if (unit == "Mt NOX/yr") {
      unit <- "Mt N/yr"
      value <- value * (14 / 46)
    } else if (unit == "Mt NH3/yr") {
      unit <- "Mt N/yr"
      value <- value * (14 / 17)
    } else if (unit == "Mt N2O/yr") {
      unit <- "Mt N2ON/yr"
      value <- value * (28 / 44) # N2O to N2ON, not N like NOX and NH3
    }
    return(data.frame(unit, value))
  }

  # Convert Units and Values to .scen compatible formats
  allData <- allData %>%
    dplyr::ungroup() %>%
    dplyr::mutate(vars = purrr::map2(.x = .data$Unit, .y = .data$Value, .f = ~ .convertUnits(.x, .y))) %>%
    dplyr::select(-.data$Unit, -.data$Value) %>%
    tidyr::unnest(.data$vars) %>%
    dplyr::rename(Unit = .data$unit, Value = .data$value)

  allData <- allData %>%
    dplyr::select(.data$Model, .data$Scenario, .data$Region, .data$Variable, .data$Unit, .data$Year, .data$Value) %>%
    dplyr::arrange(.data$Variable) # Order of the variables matters after the pivot!

  # Generate .scen-oriented data.frame for Zebedee
  # After pivot to ROpenSCMRunner and IIASA formatting, this can be removed
  if (!is.null(blackmagiccDir)) {
    inputCsvDir <- file.path(blackmagiccDir, "input_csvs")
    dir.create(inputCsvDir, showWarnings = FALSE)

    inputMifs <- allData %>%
      dplyr::group_by(.data$Model, .data$Region, .data$Scenario, .data$Variable, .data$Unit) %>%
      tidyr::pivot_wider(names_from = "Year", values_from = "Value", names_sep = " ")

    utils::write.csv(x = inputMifs, file = file.path(inputCsvDir, paste0(scenario, ".csv")), row.names = FALSE)
  }

  allData <- allData %>%
    dplyr::mutate(Unit = stringr::str_replace(string = .data$Unit, pattern = " ", replacement = "_"),
                  Unit = stringr::str_replace(string = .data$Unit, pattern = "/", replacement = "_per_")) %>%
    tidyr::pivot_wider(names_from = c("Variable", "Unit"), values_from = "Value", names_sep = " ")

  return(allData)
}