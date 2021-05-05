# This shiny module creates the path to the drake cache and loads the required data

# Loads required packages
library(drake)
library(rprojroot)
library(matsbyname)
library(matsindf)
library(tidyverse)
library(IEATools)

################################################################################
## Downloading data from drake cache
################################################################################

## Establishes path to the drake cache
# Set up the root directory
root <- rprojroot::is_rstudio_project

# Identifies the file path for the drake cache
cache_path <- root$find_file(".drake")

# Creates a list of the countries in the drake workflow, set in the _drake.R script
countries <- drake::readd(SEAPSUTWorkflow::target_names$countries, 
                          path = cache_path, 
                          character_only = TRUE)

# Creates the data frame for the geographical map coverage.
map_data <- data.frame(name = countries, color = countries)

# Creates a list of years in the drake workflow
max_year <- drake::readd(SEAPSUTWorkflow::target_names$max_year, 
                         path = cache_path, 
                         character_only = TRUE)

# Creates a list of years
years <- paste(1960:max_year)

# Creates a df with the Balanced IEA Data for the selected countries  
balanced_iea_data <- drake::readd(SEAPSUTWorkflow::target_names$BalancedIEAData, 
                                  path = cache_path, 
                                  character_only = TRUE)
# Loads Machine data target
MachineData <- drake::readd(SEAPSUTWorkflow::target_names$AllMachineData, 
                            path = cache_path, 
                            character_only = TRUE)


# Creates a df with the final-to-useful efficiency (eta) and exergy-to-energy ratio (phi) data target
etaphi_data <- drake::readd(SEAPSUTWorkflow::target_names$CompletedEfficiencyTables,
                            path = cache_path,
                            character_only = TRUE)

# Creates a df with the Destination-Machine&Eu.product allocation data target
allocations_data <- drake::readd(SEAPSUTWorkflow::target_names$CompletedAllocationTables, 
                                 path = cache_path, 
                                 character_only = TRUE)

# Adds a combined Machine-Eu.product column
allocations_data$Machine_Eu.product = paste(allocations_data$Machine," - ", allocations_data$Eu.product)

# Reads PSUT_useful target, also contains PSUT_final data target
PSUT_useful_data <- drake::readd(SEAPSUTWorkflow::target_names$PSUT_useful, 
                                 path = cache_path, 
                                 character_only = TRUE)

# Reads aggregate final and useful energy/exergy data target
Agg_finaluseful_data <- drake::readd(SEAPSUTWorkflow::target_names$AggregateFinalUsefulData, 
                                     path = cache_path, 
                                     character_only = TRUE)

# Reads aggregate primary energy/exergy data target
Agg_primary_data <- drake::readd(SEAPSUTWorkflow::target_names$AggregatePrimaryData, 
                                     path = cache_path, 
                                     character_only = TRUE)
# Reads socioeconomic data target
Socioecon_data <- drake::readd(SEAPSUTWorkflow::target_names$SocioEconData, 
                               path = cache_path, 
                               character_only = TRUE)


################################################################################
## Wrangling Agg_primary_data and Agg_finaluseful_data
################################################################################

Agg_finaluseful_data_newnames <- Agg_finaluseful_data %>%
  magrittr::set_colnames(c("Country", "Method", "Energy.type", "Stage", "Gross.Net", "E.product", "Flow.Sector", "Aggregation.by", "Year", "EX"))

Agg_primary_data_newnames <- Agg_primary_data %>%
  magrittr::set_colnames(c("Country", "Method", "Energy.type", "Stage", "Gross.Net", "E.product", "Flow.Sector", "Aggregation.by", "Year", "EX"))

Agg_all_data <- Agg_primary_data_newnames %>%
  rbind(Agg_finaluseful_data_newnames) %>%
  dplyr::mutate(Gross.Net = ifelse(is.na(Gross.Net), "Neither", Gross.Net))

PSUT_metrics_total <- Agg_all_data %>%
  dplyr::filter(Aggregation.by == "Total")

################################################################################
## Add index years
################################################################################

PSUT_metrics_total_iyear <- PSUT_metrics_total %>%
  dplyr::group_by(Country, Method, Energy.type, Stage, Gross.Net) %>%
  dplyr::filter(Year == min(Year)) %>%
  dplyr::select(-E.product, -Flow.Sector, -Aggregation.by, -Year) %>%
  magrittr::set_colnames(c("Country", "Method", "Energy.type", "Stage", 
                           "Gross.Net", "EX.iyear")) %>%
  dplyr::ungroup()

PSUT_metrics_total_i <- PSUT_metrics_total %>%
  dplyr::left_join(PSUT_metrics_total_iyear, by = c("Country", "Method", "Energy.type", "Stage", "Gross.Net")) %>%
  dplyr::mutate("EX.i" = EX/EX.iyear) %>%
  dplyr::select(-EX.iyear) %>%
  dplyr::mutate(Gross.Net = ifelse(is.na(Gross.Net), "Neither", Gross.Net))

################################################################################
## Calculating efficiency values
################################################################################

# Creates a tibble containing aggregate final-to-useful and primary-to-useful efficiencies for each year
PSUT_etas <- PSUT_metrics_total_i %>%
  dplyr::select(-EX.i, -Aggregation.by) %>%
  tidyr::pivot_wider(names_from = Stage,
                     values_from = EX) %>%
  dplyr::group_by(Country, Method, Energy.type, Year) %>% 
  tidyr::fill(Primary, .direction = "down", .by_group = TRUE) %>%
  dplyr::filter(Gross.Net != "Neither") %>%
  dplyr::group_by(Country, Method, Energy.type, Gross.Net, Year) %>%
  dplyr::mutate("Eta.fu" = Useful / Final) %>%
  dplyr::mutate("Eta.pf" = Final / Primary) %>%
  dplyr::mutate("Eta.pu" = Useful / Primary) %>%
  dplyr::select(Country, Method, Energy.type, 
                Gross.Net, Year, Eta.fu, Eta.pf, Eta.pu) %>%
  tidyr::pivot_longer(cols = Eta.fu:Eta.pu,
                      names_to = "Stages",
                      values_to = "Eta") %>%
  tidyr::drop_na(Eta) %>%
  dplyr::mutate(Stages = stringr::str_replace(Stages, "Eta.fu", "Final-Useful")) %>%
  dplyr::mutate(Stages = stringr::str_replace(Stages, "Eta.pf", "Primary-Final")) %>%
  dplyr::mutate(Stages = stringr::str_replace(Stages, "Eta.pu", "Primary-Useful")) %>%
  dplyr::ungroup()


# Calculates index values for each metric in PSUT_etas
PSUT_etas_indexes <- PSUT_etas %>%
  dplyr::group_by(Country, Method, Energy.type, Gross.Net, Stages) %>% 
  dplyr::filter(Year == min(Year)) %>%
  dplyr::select(-Year) %>%
  magrittr::set_colnames(c("Country", "Method", "Energy.type",
                           "Gross.Net", "Stages", "Eta.index")) %>%
  dplyr::ungroup()

# Adds index values to PSUT_etas tibble, and calculated yearly indexed data
PSUT_etas <- PSUT_etas %>%
  dplyr::left_join(PSUT_etas_indexes, by = c("Country", "Method", "Energy.type", "Gross.Net", "Stages")) %>% 
  dplyr::mutate("Eta.i" = Eta / Eta.index) %>%
  dplyr::select(-Eta.index)

# Removes data with Gross.Net == “Gross”, etas therefore only report:
# 1) Primary (Neither) --> Final (Net)
# 2) Primary (Neither) --> Useful (Net)
# 3) Final (Net) --> Useful (Net)
PSUT_etas <- PSUT_etas %>%
  dplyr::filter(Gross.Net != "Gross") %>%
  dplyr::select(-Gross.Net)




################################################################################
## Calculating Net Energy Analysis metrics
################################################################################

# PSUT_useful_USA <- PSUT_useful_data %>%
#   dplyr::filter(Country == "USA")
# 
# PSUT_useful_USA_2010 <- PSUT_useful_data %>%
#   dplyr::filter(Country == "USA", Year == 2010)
# 
# PSUT_nea_data <- PSUT_useful_USA_2010 %>%
#   Recca::calc_io_mats() %>%
#   Recca::calc_ERRs_gamma() %>%
#   Recca::calc_E_EIOU() %>%
#   Recca::calc_erois() %>%
#   dplyr::select(Country, Method, Energy.type, Last.stage, Year, 
#                 ger_gamma, ner_gamma, r_gamma, E_EIOU, e_EIOU, 
#                 eroi_g_p, eroi_n_p, eroi_g_i, eroi_n_i, eroi_g_p_feed, 
#                 eroi_n_p_feed, eroi_g_i_feed, eroi_n_i_feed)





















