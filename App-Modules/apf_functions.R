# Loads required packages
library(dplyr)
library(micEconCES) # script working with version 0.9.8
library(MacroGrowth) # script working with version 0.2.9000

# Returns fitted values for all aggregate production functions for selected country
apf_fit <- function(country, .df) {
  
  data <- .df %>%
    dplyr::filter(Country == country)
  
  # How do you pass variables to the formula argument!!
  
################################################################################
# rgdpna
################################################################################
  
  sf_rgdpna_model <- MacroGrowth::sfModel(formula = rgdpna ~ K + iYear, data = data)
  cd_rgdpna_model <- MacroGrowth::cdModel(formula = rgdpna ~ K + L + iYear, data = data)
  
  ## Primary Energy (PE)
  # ces.kl.e_rgdpna_PE_model <- MacroGrowth::cesModel(formula = rgdpna ~ K + L + PE + iYear, data = data)
  # ces.ke.l_rgdpna_PE_model <- MacroGrowth::cesModel(formula = rgdpna ~ K + PE + L +  Year, data = data)
  # ces.le.k_rgdpna_PE_model <- MacroGrowth::cesModel(formula = rgdpna ~ L + PE + K + Year, data = data)
  
  ces.kl.e_rgdpna_PE_model <- micEconCES::cesEst(data = data,
                                                 yName = "rgdpna",
                                                 xNames = c("K", "L", "PE"),
                                                 tName = NULL,
                                                 vrs = FALSE,
                                                 method = "PORT")
  
  ces.ke.l_rgdpna_PE_model <- micEconCES::cesEst(data = data,
                                                 yName = "rgdpna",
                                                 xNames = c("K", "PE", "L"),
                                                 tName = NULL,
                                                 vrs = FALSE,
                                                 method = "PORT")
  
  ces.le.k_rgdpna_PE_model <- micEconCES::cesEst(data = data,
                                                 yName = "rgdpna",
                                                 xNames = c("L", "PE", "K"),
                                                 tName = NULL,
                                                 vrs = FALSE,
                                                 method = "PORT")
  
  linex_rgdpna_PE_model <- MacroGrowth::linexModel(formula = rgdpna ~ K + L + PE + iYear, data = data)
  
  
  
  ## Final Energy (FEG)
  # ces.kl.e_model_rgdpna_FEG <- MacroGrowth::cesModel(formula = rgdpna ~ K + L + FEG + Year, data = data)
  # ces.ke.l_model_rgdpna_FEG <- MacroGrowth::cesModel(formula = rgdpna ~ K + FEG + L +  Year, data = data)
  # ces.le.k_model_rgdpna_FEG <- MacroGrowth::cesModel(formula = rgdpna ~ L + FEG + K + Year, data = data)
  
  ces.kl.e_rgdpna_FEG_model <- micEconCES::cesEst(data = data,
                                                 yName = "rgdpna",
                                                 xNames = c("K", "L", "FEG"),
                                                 tName = NULL,
                                                 vrs = FALSE,
                                                 method = "PORT")
  
  ces.ke.l_rgdpna_FEG_model <- micEconCES::cesEst(data = data,
                                                 yName = "rgdpna",
                                                 xNames = c("K", "FEG", "L"),
                                                 tName = NULL,
                                                 vrs = FALSE,
                                                 method = "PORT")
  
  ces.le.k_rgdpna_FEG_model <- micEconCES::cesEst(data = data,
                                                 yName = "rgdpna",
                                                 xNames = c("L", "FEG", "K"),
                                                 tName = NULL,
                                                 vrs = FALSE,
                                                 method = "PORT")
  
  linex_rgdpna_FEG_model <- MacroGrowth::linexModel(formula = rgdpna ~ K + L + FEG + Year, data = data)


  # Useful Energy (UEG)
  # ces.kl.e_model_rgdpna_UEG <- MacroGrowth::cesModel(formula = rgdpna ~ K + L + UEG + Year, data = data)
  # ces.ke.l_model_rgdpna_UEG <- MacroGrowth::cesModel(formula = rgdpna ~ K + UEG + L +  Year, data = data)
  # ces.le.k_model_rgdpna_UEG <- MacroGrowth::cesModel(formula = rgdpna ~ L + UEG + K + Year, data = data)
  
  ces.kl.e_rgdpna_UEG_model <- micEconCES::cesEst(data = data,
                                                 yName = "rgdpna",
                                                 xNames = c("K", "L", "UEG"),
                                                 tName = NULL,
                                                 vrs = FALSE,
                                                 method = "PORT")
  
  ces.ke.l_rgdpna_UEG_model <- micEconCES::cesEst(data = data,
                                                 yName = "rgdpna",
                                                 xNames = c("K", "UEG", "L"),
                                                 tName = NULL,
                                                 vrs = FALSE,
                                                 method = "PORT")
  
  ces.le.k_rgdpna_UEG_model <- micEconCES::cesEst(data = data,
                                                 yName = "rgdpna",
                                                 xNames = c("L", "UEG", "K"),
                                                 tName = NULL,
                                                 vrs = FALSE,
                                                 method = "PORT")
  
  linex_rgdpna_UEG_model <- MacroGrowth::linexModel(formula = rgdpna ~ K + L + UEG + Year, data = data)
  
################################################################################
# rgdpe
################################################################################
  
  sf_rgdpe_model <- MacroGrowth::sfModel(formula = rgdpe ~ K + iYear, data = data)
  cd_rgdpe_model <- MacroGrowth::cdModel(formula = rgdpe ~ K + L + iYear, data = data)
  
  ## Primary Energy (PE)
  ces.kl.e_rgdpe_PE_model <- micEconCES::cesEst(data = data,
                                                 yName = "rgdpe",
                                                 xNames = c("K", "L", "PE"),
                                                 tName = NULL,
                                                 vrs = FALSE,
                                                 method = "PORT")
  
  ces.ke.l_rgdpe_PE_model <- micEconCES::cesEst(data = data,
                                                 yName = "rgdpe",
                                                 xNames = c("K", "PE", "L"),
                                                 tName = NULL,
                                                 vrs = FALSE,
                                                 method = "PORT")
  
  ces.le.k_rgdpe_PE_model <- micEconCES::cesEst(data = data,
                                                 yName = "rgdpe",
                                                 xNames = c("L", "PE", "K"),
                                                 tName = NULL,
                                                 vrs = FALSE,
                                                 method = "PORT")
  
  linex_rgdpe_PE_model <- MacroGrowth::linexModel(formula = rgdpe ~ K + L + PE + iYear, data = data)
  
  
  
  ## Final Energy (FEG)
  ces.kl.e_rgdpe_FEG_model <- micEconCES::cesEst(data = data,
                                                  yName = "rgdpe",
                                                  xNames = c("K", "L", "FEG"),
                                                  tName = NULL,
                                                  vrs = FALSE,
                                                  method = "PORT")
  
  ces.ke.l_rgdpe_FEG_model <- micEconCES::cesEst(data = data,
                                                  yName = "rgdpe",
                                                  xNames = c("K", "FEG", "L"),
                                                  tName = NULL,
                                                  vrs = FALSE,
                                                  method = "PORT")
  
  ces.le.k_rgdpe_FEG_model <- micEconCES::cesEst(data = data,
                                                  yName = "rgdpe",
                                                  xNames = c("L", "FEG", "K"),
                                                  tName = NULL,
                                                  vrs = FALSE,
                                                  method = "PORT")
  
  linex_rgdpe_FEG_model <- MacroGrowth::linexModel(formula = rgdpe ~ K + L + FEG + Year, data = data)
  
  
  # Useful Energy (UEG)
  ces.kl.e_rgdpe_UEG_model <- micEconCES::cesEst(data = data,
                                                  yName = "rgdpe",
                                                  xNames = c("K", "L", "UEG"),
                                                  tName = NULL,
                                                  vrs = FALSE,
                                                  method = "PORT")
  
  ces.ke.l_rgdpe_UEG_model <- micEconCES::cesEst(data = data,
                                                  yName = "rgdpe",
                                                  xNames = c("K", "UEG", "L"),
                                                  tName = NULL,
                                                  vrs = FALSE,
                                                  method = "PORT")
  
  ces.le.k_rgdpe_UEG_model <- micEconCES::cesEst(data = data,
                                                  yName = "rgdpe",
                                                  xNames = c("L", "UEG", "K"),
                                                  tName = NULL,
                                                  vrs = FALSE,
                                                  method = "PORT")
  
  linex_rgdpe_UEG_model <- MacroGrowth::linexModel(formula = rgdpe ~ K + L + UEG + Year, data = data)
  
################################################################################
# rgdpo
################################################################################
  
  sf_rgdpo_model <- MacroGrowth::sfModel(formula = rgdpo ~ K + iYear, data = data)
  cd_rgdpo_model <- MacroGrowth::cdModel(formula = rgdpo ~ K + L + iYear, data = data)
  
  ## Primary Energy (PE)
  ces.kl.e_rgdpo_PE_model <- micEconCES::cesEst(data = data,
                                                yName = "rgdpo",
                                                xNames = c("K", "L", "PE"),
                                                tName = NULL,
                                                vrs = FALSE,
                                                method = "PORT")
  
  ces.ke.l_rgdpo_PE_model <- micEconCES::cesEst(data = data,
                                                yName = "rgdpo",
                                                xNames = c("K", "PE", "L"),
                                                tName = NULL,
                                                vrs = FALSE,
                                                method = "PORT")
  
  ces.le.k_rgdpo_PE_model <- micEconCES::cesEst(data = data,
                                                yName = "rgdpo",
                                                xNames = c("L", "PE", "K"),
                                                tName = NULL,
                                                vrs = FALSE,
                                                method = "PORT")
  
  linex_rgdpo_PE_model <- MacroGrowth::linexModel(formula = rgdpo ~ K + L + PE + iYear, data = data)
  
  
  
  ## Final Energy (FEG)
  ces.kl.e_rgdpo_FEG_model <- micEconCES::cesEst(data = data,
                                                 yName = "rgdpo",
                                                 xNames = c("K", "L", "FEG"),
                                                 tName = NULL,
                                                 vrs = FALSE,
                                                 method = "PORT")
  
  ces.ke.l_rgdpo_FEG_model <- micEconCES::cesEst(data = data,
                                                 yName = "rgdpo",
                                                 xNames = c("K", "FEG", "L"),
                                                 tName = NULL,
                                                 vrs = FALSE,
                                                 method = "PORT")
  
  ces.le.k_rgdpo_FEG_model <- micEconCES::cesEst(data = data,
                                                 yName = "rgdpo",
                                                 xNames = c("L", "FEG", "K"),
                                                 tName = NULL,
                                                 vrs = FALSE,
                                                 method = "PORT")
  
  linex_rgdpo_FEG_model <- MacroGrowth::linexModel(formula = rgdpo ~ K + L + FEG + Year, data = data)
  
  
  # Useful Energy (UEG)
  ces.kl.e_rgdpo_UEG_model <- micEconCES::cesEst(data = data,
                                                 yName = "rgdpo",
                                                 xNames = c("K", "L", "UEG"),
                                                 tName = NULL,
                                                 vrs = FALSE,
                                                 method = "PORT")
  
  ces.ke.l_rgdpo_UEG_model <- micEconCES::cesEst(data = data,
                                                 yName = "rgdpo",
                                                 xNames = c("K", "UEG", "L"),
                                                 tName = NULL,
                                                 vrs = FALSE,
                                                 method = "PORT")
  
  ces.le.k_rgdpo_UEG_model <- micEconCES::cesEst(data = data,
                                                 yName = "rgdpo",
                                                 xNames = c("L", "UEG", "K"),
                                                 tName = NULL,
                                                 vrs = FALSE,
                                                 method = "PORT")
  
  linex_rgdpo_UEG_model <- MacroGrowth::linexModel(formula = rgdpo ~ K + L + UEG + Year, data = data)
  
################################################################################
# Retieves fitted values for each model and set of parameters and 
################################################################################
  
  Actual_GDP_Data <- data %>%
    dplyr::select(Country, Year, iYear, rgdpe, rgdpo, rgdpna) %>%
    tidyr::pivot_longer(cols = rgdpe:rgdpna,
                        names_to = "GDP_Metric",
                        values_to = "GDP")
  
  
  SF_Fitted_Data <- data %>%
    
    # Add Single-Factor APF tests for rgdpna
    dplyr::mutate("PE_rgdpna_sf" = MacroGrowth::yhat(sf_rgdpna_model)) %>%
    dplyr::mutate("FEG_rgdpna_sf" = MacroGrowth::yhat(sf_rgdpna_model)) %>%
    dplyr::mutate("UEG_rgdpna_sf" = MacroGrowth::yhat(sf_rgdpna_model)) %>%
    
    # Add Single-Factor APF tests for rgdpe
    dplyr::mutate("PE_rgdpe_sf" = MacroGrowth::yhat(sf_rgdpe_model)) %>%
    dplyr::mutate("FEG_rgdpe_sf" = MacroGrowth::yhat(sf_rgdpe_model)) %>%
    dplyr::mutate("UEG_rgdpe_sf" = MacroGrowth::yhat(sf_rgdpe_model)) %>%
    
    # Add Single-Factor APF tests for rgdpo
    dplyr::mutate("PE_rgdpo_sf" = MacroGrowth::yhat(sf_rgdpo_model)) %>%
    dplyr::mutate("FEG_rgdpo_sf" = MacroGrowth::yhat(sf_rgdpo_model)) %>%
    dplyr::mutate("UEG_rgdpo_sf" = MacroGrowth::yhat(sf_rgdpo_model)) %>%
    
    # Removes columns that are no longer needed
    dplyr::select(-rgdpna, -rgdpe, -rgdpo, 
                  -L, -Ladj, -K, -Kserv, 
                  -PE, -FEG, -UEG, -PX, -FXG, -UXG) %>%
  
    # Pivots Single-Factor data longer
    tidyr::pivot_longer(cols = PE_rgdpna_sf:UEG_rgdpo_sf,
                        names_to = c("Stage", "GDP_Metric", "APF"),
                        names_pattern = "(\\w+)_(\\w+)_(\\w+)",
                        values_to = "GDP.Fitted")
  
    
  CD_Fitted_Data <- data %>%
    # Add Cobb-Douglas APF tests for rgdpna
    dplyr::mutate("PE_rgdpna_cd" = MacroGrowth::yhat(cd_rgdpna_model)) %>%
    dplyr::mutate("FEG_rgdpna_cd" = MacroGrowth::yhat(cd_rgdpna_model)) %>%
    dplyr::mutate("UEG_rgdpna_cd" = MacroGrowth::yhat(cd_rgdpna_model)) %>%
    
    # Add Cobb-Douglas APF tests for rgdpe
    dplyr::mutate("PE_rgdpe_cd" = MacroGrowth::yhat(cd_rgdpe_model)) %>%
    dplyr::mutate("FEG_rgdpe_cd" = MacroGrowth::yhat(cd_rgdpe_model)) %>%
    dplyr::mutate("UEG_rgdpe_cd" = MacroGrowth::yhat(cd_rgdpe_model)) %>%
    
    # Add Cobb-Douglas APF tests for rgdpo
    dplyr::mutate("PE_rgdpo_cd" = MacroGrowth::yhat(cd_rgdpo_model)) %>%
    dplyr::mutate("FEG_rgdpo_cd" = MacroGrowth::yhat(cd_rgdpo_model)) %>%
    dplyr::mutate("UEG_rgdpo_cd" = MacroGrowth::yhat(cd_rgdpo_model)) %>%
    
    # Removes columns that are no longer needed
    dplyr::select(-rgdpna, -rgdpe, -rgdpo, 
                  -L, -Ladj, -K, -Kserv, 
                  -PE, -FEG, -UEG, -PX, -FXG, -UXG) %>%
    
    # Pivots Cobb-Douglas data longer
    tidyr::pivot_longer(cols = PE_rgdpna_cd:UEG_rgdpo_cd,
                        names_to = c("Stage", "GDP_Metric", "APF"),
                        names_pattern = "(\\w+)_(\\w+)_(\\w+)",
                        values_to = "GDP.Fitted")
    
    
  
  CES_Fitted_Data <- data %>%
    
    # Add CES tests using Primary, Final and Useful energy for rgdpna
    dplyr::mutate("PE_rgdpna_ces.kl.e" = MacroGrowth::yhat(ces.kl.e_rgdpna_PE_model)) %>%
    dplyr::mutate("PE_rgdpna_ces.ke.l" = MacroGrowth::yhat(ces.ke.l_rgdpna_PE_model)) %>%
    dplyr::mutate("PE_rgdpna_ces.le.k" = MacroGrowth::yhat(ces.le.k_rgdpna_PE_model)) %>%
    
    dplyr::mutate("FEG_rgdpna_ces.kl.e" = MacroGrowth::yhat(ces.kl.e_rgdpna_FEG_model)) %>%
    dplyr::mutate("FEG_rgdpna_ces.ke.l" = MacroGrowth::yhat(ces.ke.l_rgdpna_FEG_model)) %>%
    dplyr::mutate("FEG_rgdpna_ces.le.k" = MacroGrowth::yhat(ces.le.k_rgdpna_FEG_model)) %>%

    dplyr::mutate("UEG_rgdpna_ces.kl.e" = MacroGrowth::yhat(ces.kl.e_rgdpna_UEG_model)) %>%
    dplyr::mutate("UEG_rgdpna_ces.ke.l" = MacroGrowth::yhat(ces.ke.l_rgdpna_UEG_model)) %>%
    dplyr::mutate("UEG_rgdpna_ces.le.k" = MacroGrowth::yhat(ces.le.k_rgdpna_UEG_model)) %>%
  
    # Add CES tests using Primary, Final and Useful energy for rgdpe
    dplyr::mutate("PE_rgdpe_ces.kl.e" = MacroGrowth::yhat(ces.kl.e_rgdpe_PE_model)) %>%
    dplyr::mutate("PE_rgdpe_ces.ke.l" = MacroGrowth::yhat(ces.ke.l_rgdpe_PE_model)) %>%
    dplyr::mutate("PE_rgdpe_ces.le.k" = MacroGrowth::yhat(ces.le.k_rgdpe_PE_model)) %>%
    
    dplyr::mutate("FEG_rgdpe_ces.kl.e" = MacroGrowth::yhat(ces.kl.e_rgdpe_FEG_model)) %>%
    dplyr::mutate("FEG_rgdpe_ces.ke.l" = MacroGrowth::yhat(ces.ke.l_rgdpe_FEG_model)) %>%
    dplyr::mutate("FEG_rgdpe_ces.le.k" = MacroGrowth::yhat(ces.le.k_rgdpe_FEG_model)) %>%
    
    dplyr::mutate("UEG_rgdpe_ces.kl.e" = MacroGrowth::yhat(ces.kl.e_rgdpe_UEG_model)) %>%
    dplyr::mutate("UEG_rgdpe_ces.ke.l" = MacroGrowth::yhat(ces.ke.l_rgdpe_UEG_model)) %>%
    dplyr::mutate("UEG_rgdpe_ces.le.k" = MacroGrowth::yhat(ces.le.k_rgdpe_UEG_model)) %>%
    
    # Add CES tests using Primary, Final and Useful energy for rgdpo
    dplyr::mutate("PE_rgdpo_ces.kl.e" = MacroGrowth::yhat(ces.kl.e_rgdpo_PE_model)) %>%
    dplyr::mutate("PE_rgdpo_ces.ke.l" = MacroGrowth::yhat(ces.ke.l_rgdpo_PE_model)) %>%
    dplyr::mutate("PE_rgdpo_ces.le.k" = MacroGrowth::yhat(ces.le.k_rgdpo_PE_model)) %>%
    
    dplyr::mutate("FEG_rgdpo_ces.kl.e" = MacroGrowth::yhat(ces.kl.e_rgdpo_FEG_model)) %>%
    dplyr::mutate("FEG_rgdpo_ces.ke.l" = MacroGrowth::yhat(ces.ke.l_rgdpo_FEG_model)) %>%
    dplyr::mutate("FEG_rgdpo_ces.le.k" = MacroGrowth::yhat(ces.le.k_rgdpo_FEG_model)) %>%
    
    dplyr::mutate("UEG_rgdpo_ces.kl.e" = MacroGrowth::yhat(ces.kl.e_rgdpo_UEG_model)) %>%
    dplyr::mutate("UEG_rgdpo_ces.ke.l" = MacroGrowth::yhat(ces.ke.l_rgdpo_UEG_model)) %>%
    dplyr::mutate("UEG_rgdpo_ces.le.k" = MacroGrowth::yhat(ces.le.k_rgdpo_UEG_model)) %>%
    
    # Removes columns that are no longer needed
    dplyr::select(-rgdpna, -rgdpe, -rgdpo, 
                  -L, -Ladj, -K, -Kserv, 
                  -PE, -FEG, -UEG, -PX, -FXG, -UXG) %>%
    
    # Pivots CES and Linex data longer
    tidyr::pivot_longer(cols = PE_rgdpna_ces.kl.e:UEG_rgdpo_ces.le.k,
                        names_to = c("Stage", "GDP_Metric", "APF"),
                        names_pattern = "(\\w+)_(\\w+)_(\\w+\\.\\w+\\.\\w+)",
                        values_to = "GDP.Fitted")
  
  
  Linex_Fitted_Data <- data %>%
    
    # Add Linex tests using Primary, Final and Useful energy for rgdpna
    dplyr::mutate("PE_rgdpna_linex" = MacroGrowth::yhat(linex_rgdpna_PE_model)) %>%
    dplyr::mutate("FEG_rgdpna_linex" = MacroGrowth::yhat(linex_rgdpna_FEG_model)) %>%
    dplyr::mutate("UEG_rgdpna_linex" = MacroGrowth::yhat(linex_rgdpna_UEG_model)) %>%
    
    # Add Linex tests using Primary, Final and Useful energy for rgdpe
    dplyr::mutate("PE_rgdpe_linex" = MacroGrowth::yhat(linex_rgdpe_PE_model)) %>%
    dplyr::mutate("FEG_rgdpe_linex" = MacroGrowth::yhat(linex_rgdpe_FEG_model)) %>%
    dplyr::mutate("UEG_rgdpe_linex" = MacroGrowth::yhat(linex_rgdpe_UEG_model)) %>%
    
    # Add Linex tests using Primary, Final and Useful energy for rgdpo
    dplyr::mutate("PE_rgdpo_linex" = MacroGrowth::yhat(linex_rgdpo_PE_model)) %>%
    dplyr::mutate("FEG_rgdpo_linex" = MacroGrowth::yhat(linex_rgdpo_FEG_model)) %>%
    dplyr::mutate("UEG_rgdpo_linex" = MacroGrowth::yhat(linex_rgdpo_UEG_model)) %>%
    
    # Removes columns that are no longer needed
    dplyr::select(-rgdpna, -rgdpe, -rgdpo, 
                  -L, -Ladj, -K, -Kserv, 
                  -PE, -FEG, -UEG, -PX, -FXG, -UXG) %>%
    
    # Pivots CES and Linex data longer
    tidyr::pivot_longer(cols = PE_rgdpna_linex:UEG_rgdpo_linex,
                        names_to = c("Stage", "GDP_Metric", "APF"),
                        names_pattern = "(\\w+)_(\\w+)_(\\w+)",
                        values_to = "GDP.Fitted")
  
  # Collect all data into a single data frame
  All_APF_GDP_Data <- Linex_Fitted_Data %>%
    base::rbind(CES_Fitted_Data) %>%
    base::rbind(SF_Fitted_Data) %>%
    base::rbind(CD_Fitted_Data) %>%
    base::merge(Actual_GDP_Data, by = c("Country", "Year", "iYear", "GDP_Metric")) %>%
    # Replace UEG, FEG and PE with Useful, Final and Primary
    dplyr::mutate(Stage = str_replace(Stage, "PE", "Primary")) %>%
    dplyr::mutate(Stage = str_replace(Stage, "FEG", "Final")) %>%
    dplyr::mutate(Stage = str_replace(Stage, "UEG", "Useful"))
  
  return(All_APF_GDP_Data)
  
}

# foo <- apf_fit(country = "ESP", .df = EA_data_final)


all_apf_fit <- function(countries, data) {
  
  all_apf_fit_data <- data.frame()
  
  for (country in countries) {

    result <- apf_fit(country = country, .df = data)

    all_apf_fit_data <- rbind(all_apf_fit_data, result)

  }
  
  return(all_apf_fit_data)
  
}

APF_Data <- all_apf_fit(countries = countries, data = EA_data_final)



