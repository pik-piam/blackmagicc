#' @title formatInput
#' @description converts MAgPIE emissions data, combining it with user-selected REMIND emissions, into the format
#' necessary to run MAGICC7
#' @author Michael Crawford
#'
#' @param remindEmissions_path file path for the reference REMIND emissions
#' @param magpiemif_path file path for the MAgPIE emissions
#'
#' @return a composite data.frame containing the scenario's MAgPIE emissions and reference REMIND emissions
#'
#' @importFrom utils read.csv
#' @importFrom dplyr %>% filter select mutate group_by ungroup rename bind_rows summarise arrange
#' @importFrom stringr str_detect str_count str_extract str_replace str_split str_remove
#' @importFrom tidyr as_tibble pivot_longer unnest pivot_wider
#' @importFrom purrr pmap
#'
#' @examples
#'   \dontrun{
#'     x <- formatInput(magpiemif_path, remindEmissions_path)
#'   }

formatInput <- function(remindEmissions_path, magpiemif_path) {

    #
    # Remind emissions
    #

    # Both .RDS and .mif formats are acceptable
    if (str_detect(string = remindEmissions_path, pattern = ".RDS")) {
        remind <- readRDS(remindEmissions_path)
    } else {
        remind <- read.csv(remindEmissions_path, header = TRUE, sep = ';', stringsAsFactors = FALSE)
    }

# TODO Only run all this stuff if the emissions haven't been already cleaned ...

    # Filter for only World-level emissions
    remind <- remind %>%
        filter(str_detect(string = Variable, pattern = 'Emi\\|'),
               Region == "World") %>%
        as_tibble()

      # Remove all emissions derived from MAgPIE
    remind_emissions_fromMagpie <- c("Emi|CO2|Land-Use Change",
                                     "Emi|CH4|Land Use",
                                     "Emi|N2O|Land Use")

    remind <- remind %>%
        filter(!Variable %in% remind_emissions_fromMagpie) %>%
        mutate(Model = "REMIND")

    # Select only top-level emissions, or HFCs, and no emission-equivalents
    remind <- remind %>%
        filter(str_count(string = Variable, pattern = '\\|') == 1 |
                   str_detect(string = Variable, pattern = 'Emi\\|HFC\\|HFC')) %>%
        filter(str_detect(string = Unit, pattern = "equiv", negate = TRUE))

    # Remove "Emi|" in Unit column
    remind <- remind %>%
        mutate(Variable = str_extract(string = Unit, pattern = "(?<=\\s)(.+)(?=/)"))

    # Reformat to tibble-style
    remind <- remind %>%
        select(-X) %>% # There is additional column called "X"
        pivot_longer(cols = contains("X"),
                     names_to = "Year",
                     values_to = "Value") %>%
        mutate(Year = str_remove(Year, "X")) %>%
        mutate(Year = as.numeric(Year),
               Value = as.numeric(Value))

    .convertRemindUnits <- function(.variable, .unit, .value) {
        # TODO Error-checking, make sure that the units are consistent with expectations
        if (.variable == "CO2") {
            .variable <- "CO2I"
            .unit     <- "Gt C/yr"
            .value    <- .value * 12/44 * 0.001 # Convert to CO2 to C and Mt to Gt
        } else if (.variable == "NOX") {
            .unit  <- "Mt N/yr"
            .value <- .value * 14/46
        } else if (.variable == "NH3") {
            .unit  <- "Mt N/yr"
            .value <- .value * 14/17
        } else if (.variable == "N2O") {
            .unit  <- "Mt N2ON/yr"
            .value <- .value * 28/44 * 0.0001 # Covert kt to Mt
        } else if (str_detect(string = .variable, pattern = "HFC")) {
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
        mutate(vars = pmap(.l = list(Variable, Unit, Value),
                           .f = ~ .convertRemindUnits(..1, ..2, ..3))) %>%
        unnest(vars) %>%
        mutate(.variable = as.character(.variable),
               .unit     = as.character(.unit)) %>%
        select(-Variable, -Unit, -Value) %>%
        rename(Variable = .variable,
               Unit     = .unit,
               Value    = .value)

#END ONLY RUN IF...

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

    magpie <- read.csv(magpiemif_path, header = TRUE, sep = ';', stringsAsFactors = FALSE) %>%
        filter(Variable %in% magpie_emissionCategories,
               Region == "World") %>%
        as_tibble()

    # Reformat to tibble-style
    magpie <- magpie %>%
        select(-X) %>% # There is additional column called "X"
        pivot_longer(cols = starts_with("X"),
                     names_to = "Year",
                     values_to = "Value") %>%
        mutate(Year = str_remove(Year, "X")) %>%
        filter(Year %in% remind$Year) %>%
        mutate(Year = as.numeric(Year),
               Value = as.numeric(Value))

    # Sum across emission types
    magpie <- magpie %>%
        group_by(Model, Scenario, Region, Unit, Year) %>%
        summarise(Value = sum(Value))

    # Re-integrate top-level emission variable from the Unit for MAGICC's read in
    magpie <- magpie %>%
        mutate(Variable = str_extract(string = Unit, pattern = "(?<=\\s)(.+)(?=/)"))

    .convertMAgPIEUnits <- function(.variable, .unit, .value)
    {
        # TODO Error-checking, make sure that the units are consistent with expectations
        if (.variable == "CO2") {
            .variable <- "CO2B"
            .unit     <- "Gt C/yr"
            .value    <- .value * 12/44 * 0.001 # Convert to CO2 to C and Mt to Gt
        } else if (.variable == "N2O") {
            .unit  <- "Mt N2ON/yr"
            .value <- .value * 28/44
        }
        return(data.frame(.variable, .unit, .value))
    }

    magpie <- magpie %>%
        ungroup() %>%
        mutate(vars = pmap(.l = list(Variable, Unit, Value),
                           .f = ~ .convertMAgPIEUnits(..1, ..2, ..3))) %>%
        unnest(vars) %>%
        mutate(.variable = as.character(.variable),
               .unit     = as.character(.unit)) %>%
        select(-Variable, -Unit, -Value) %>%
        rename(Variable = .variable,
               Unit     = .unit,
               Value    = .value)

    #
    # Integrate the REMIND emissions and the new MAgPIE emissions
    #

    all <- bind_rows(remind, magpie) %>%
        filter(Year %in% unique(magpie$Year)) %>%
        mutate(Unit = str_replace(string = Unit, pattern = " ", replacement = "_"),
               Unit = str_replace(string = Unit, pattern = "/", replacement = "_per_"))

    scenario <- paste0(unique(remind$Scenario), "__", unique(magpie$Scenario))

    all <- all %>%
        group_by(Region, Variable, Unit, Year) %>%
        summarise(Value = sum(Value)) %>%
        mutate(Model = "REMIND-MAgPIE", Scenario = scenario) %>%
        select(Model, Region, Scenario, Unit, Variable, Year, Value) %>%
        arrange(Variable) %>% # Order of the variables matters after the pivot!
        pivot_wider(names_from = c("Variable", "Unit"), values_from = "Value", names_sep = " ")

    # Ensure that the MAGICC data is used during the historical period, ensuring a better pattern
    # during the historical period. The end warming (i.e. year 2099) is left unaffected regardless.
    all <- all %>% filter(Year > 2014)

    return(all)

}
