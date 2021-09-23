# This shiny module loads data from outside the drake workflow

# Loads required packages
# library(wbstats)
library(tidyverse)
# library(pwt9)
library(pwt10)

################################################################################
## Wrangling socioeconomic data into correct format
################################################################################

if(nrow(Socioecon_data) > 0){

  # Getting index year for OECD and non-OECD countries

  iyears <- Agg_primary_data %>%
    dplyr::filter(Aggregation.by == "Total") %>%
    dplyr::group_by(Country, Method, Energy.type, Stage, Gross.Net, E.product, Flow, Aggregation.by) %>%
    dplyr::filter(Year == min(Year)) %>%
    dplyr::ungroup() %>%
    dplyr::select(Country, Year) %>%
    magrittr::set_colnames(c("Country", "iYear"))

  # Adding index years
  Socioecon_data_iyear <- Socioecon_data %>%
    dplyr::left_join(iyears, by = "Country") %>%
    dplyr::group_by(Country) %>%
    dplyr::filter(Year == iYear) %>%
    magrittr::set_colnames(c("Country", "Year", "rgdpe.iyear", "rgdpo.iyear",
                             "rgdpna.iyear", "L.iyear", "Ladj.iyear", "K.iyear",
                             "Kserv.iyear", "iYear")) %>%
    dplyr::select(-Year) %>%
    dplyr::ungroup()

  Socioecon_data_i <- Socioecon_data %>%
    dplyr::left_join(Socioecon_data_iyear, by = c("Country")) %>%
    dplyr::filter(Year >= iYear) %>%
    dplyr::mutate("rgdpe.i" = ifelse(is.na(rgdpe), 1, rgdpe / rgdpe.iyear)) %>%
    dplyr::mutate("rgdpo.i" = ifelse(is.na(rgdpo), 1 , rgdpo / rgdpo.iyear)) %>%
    dplyr::mutate("rgdpna.i" = ifelse(is.na(rgdpna), 1 , rgdpna / rgdpna.iyear)) %>%
    dplyr::mutate("L.i" = ifelse(is.na(L), 1 , L / L.iyear)) %>%
    dplyr::mutate("Ladj.i" = ifelse(is.na(Ladj), 1 , Ladj / Ladj.iyear)) %>%
    dplyr::mutate("K.i" = ifelse(is.na(K), 1 , K / K.iyear)) %>%
    dplyr::mutate("Kserv.i" = ifelse(is.na(Kserv), 1 , Kserv / Kserv.iyear)) %>%
    dplyr::select(Country, Year, iYear, rgdpe.i, rgdpo.i, rgdpna.i, L.i, Ladj.i, K.i, Kserv.i)

  Socioecon_data_i <- Socioecon_data_i %>%
    dplyr::group_by(Country) %>%
    dplyr::mutate(Year.Index = 1:(unique(max(Year)) - unique(iYear) + 1), .after = "Year") %>%
    dplyr::ungroup()

  GDP_metrics_abs <- Socioecon_data %>%
    dplyr::select(Country, Year, rgdpe, rgdpo, rgdpna) %>%
    tidyr::pivot_longer(cols = c("rgdpe", "rgdpo", "rgdpna"), names_to = "GDP_Metric", values_to = "GDP")

  GDP_metrics_i <- Socioecon_data_i %>%
    dplyr::select(Country, Year, rgdpe.i, rgdpo.i, rgdpna.i) %>%
    magrittr::set_colnames(c("Country", "Year", "rgdpe", "rgdpo", "rgdpna")) %>%
    tidyr::pivot_longer(cols = c("rgdpe", "rgdpo", "rgdpna"), names_to = "GDP_Metric", values_to = "GDP.i")

  GDP_metrics <- GDP_metrics_abs %>%
    dplyr::left_join(GDP_metrics_i, by = c("Country", "Year", "GDP_Metric"))

  EX_Econ_Data <- PSUT_metrics_total_i %>%
    dplyr::filter(Gross.Net != "Gross") %>% # Filters out data in Gross terms, leaving only Primary - Neither, Final - Net, and Useful - Net
    dplyr::select(-Gross.Net) %>%
    dplyr::left_join(PSUT_etas, by = c("Country", "Method", "Energy.type", "Year")) %>% # Etas data in Gross terms has already been filtered out.
    dplyr::left_join(GDP_metrics, by = c("Country", "Year")) %>%
    dplyr::group_by(Country, Method, Energy.type, GDP_Metric) %>% #, Gross.Net
    dplyr::mutate("EX_intensity" = EX / GDP) %>%
    dplyr::mutate("EX_intensity.i" = EX.i / GDP.i) %>%
    dplyr::ungroup()

  ################################################################################
  ## Creating data frame for causality tests and apf fitting
  ##
  ## !!!This will need to be changed once we have moved from energy to exergy!!!
  ################################################################################

  # Creates a data frame containing the indexed economic data needed for granger tests
  EA_data_econ <- Socioecon_data_i %>%
    dplyr::select(-iYear) %>%
    magrittr::set_colnames(c("Country", "Year", "iYear",
                             "rgdpe", "rgdpo", "rgdpna", "L",
                             "Ladj", "K", "Kserv"))

  # Pivots PSUT data wider, selects indexed data only
  PSUT_metrics_total_wide_net_i <- PSUT_metrics_total_i %>%
    dplyr::filter(is.na(Gross.Net) | Gross.Net != "Gross") %>%
    dplyr::select(-EX, -Gross.Net, -E.product, -Flow.Sector, -Aggregation.by) %>% # -Gross.Net,
    tidyr::pivot_wider(names_from = Stage,
                       values_from = EX.i) %>%
    magrittr::set_colnames(c("Country", "Method", "Energy.type", "Year", "PE", "FEG", "UEG"))

  # Creates a data frame containing the indexed energy data needed for granger tests
  EA_data_energy <- PSUT_metrics_total_wide_net_i %>%
    dplyr::left_join(iyears, by = "Country") %>%
    dplyr::select(-Method, -Energy.type) %>%
    dplyr::group_by(Country) %>%
    dplyr::mutate(iYear = 1:(max_year - unique(iYear) + 1)) %>%
    dplyr::select(-Year) %>%
    # Create dummy indexed exergy data
    group_by(iYear) %>%
    dplyr::mutate(PX = if_else(iYear == 1, PE, PE * 1.1)) %>%
    dplyr::mutate(FXG = if_else(iYear == 1, FEG, FEG * 1.1)) %>%
    dplyr::mutate(UXG = if_else(iYear == 1, UEG, UEG * 0.5)) %>%
    dplyr::relocate(iYear, .after = Country)

  # Creates the data frame required for Emmanuel's granger causality test script
  EA_data_final <- EA_data_econ %>%
    dplyr::left_join(EA_data_energy, by = c("Country", "iYear")) %>%
    dplyr::filter(Year <= max_year)

} else if(nrow(Socioecon_data) == 0){



}

################################################################################
## Running APF fitting models
################################################################################


################################################################################
## Downloading Data in wbstats
################################################################################

# Provides a snapshot of available countries, indicators, and other relevant information
# str(wbstats::wb_cachelist, max.level = 1)

# Updates World Bank API cache, stores in list
# new_cache <- wbstats::wb_cache()

# Searches for indicators containing relevant terms
# gdp_inds <- wbstats::wb_search("gdp")


# # Creates a dataframe containing GDP per capita (constant 2010 US$)
# gdp_pc <- wbstats::wb_data("NY.GDP.PCAP.KD") %>%
#   dplyr::select(2,4,5) %>%
#   magrittr::set_colnames(c("Country", "Year", "GDP_pc"))

# Creates a dataframe containing GDP (constant 2010 US$)
# gdp <- wbstats::wb_data("NY.GDP.MKTP.KD") %>%
#   dplyr::select(2,4,5) %>%
#   magrittr::set_colnames(c("Country", "Year", "GDP"))

# # Creates a tibble containing all of the required wbstats economic data
# gdp_all <- gdp_pc %>%
#   dplyr::left_join(gdp, by = c("Country", "Year")) %>%
#   tibble::as_tibble()

################################################################################
## Wrangling Data in wbstats
################################################################################

# Adds GDP data
# GDP_metrics <- gdp %>%
#   dplyr::filter(Country %in% countries)
#
# GDP_metrics_indexes <- GDP_metrics %>%
#   dplyr::group_by(Country) %>%
#   dplyr::filter(Year == min(Year)) %>%
#   dplyr::select(-c(Year)) %>%
#   magrittr::set_colnames(c("Country", "GDP.index")) %>%
#   dplyr::ungroup()

# GDP_metrics <- GDP_metrics %>%
#   dplyr::left_join(GDP_metrics_indexes, by = c("Country")) %>%
#   dplyr::mutate("GDP.i" = GDP / GDP.index) %>%
#   dplyr::select(-GDP.index)
