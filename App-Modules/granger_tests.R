# Script encoding: UTF-8
# R version: 3.6.3
# Author: Emmanuel Aramendia

# This script provides the code for Granger causality testing as performed in the paper
# Title: Moving from final to useful stage in energy-economy analysis: a critical assessment
# doi: 10.1016/j.apenergy.2020.116194

# The script is decomposed as follows:
# (1) Importing libraries
# (2) Importing data (see file ...)
# (3) Computing 1-year growth rates
# (4) Testing time series for stationarity
# (5) Running causality tests
#     i. Setting up the result data frame
#     ii. Initialising n
#     iii. Choosing energy and gdp variables
#     iv. Tests with grangertest function
#     v. Tests with causality function
#     vi. Tests with Toda-Yamamoto procedure
# (6) Exporting data to csv if needed.


#### (1) Importing needed libraries ####
# Add versions.

# Tidyverse packages
library(dplyr)# script working with version 1.0.2
library(readr)# script working with version 1.3.1

# Other packages
library(lmtest) # script working with version 0.9.37
library(tseries) # script working with version 0.10.47
library(vars) # script working with version 1.5.3
library(aod) # script working with version 1.3.1

#### (2) Importing data ####
# raw_ES_data <- readr::read_csv(file = "",# Fill in the path
#                     col_types = cols(
#                       .default = readr::col_double()
#                     ))
raw_ES_data <- EA_data_final


#### (3) Computing 1-year growth rates ####
ES_data <- raw_ES_data %>%
  dplyr::mutate(g_rgdpe = (rgdpe - dplyr::lag(rgdpe))/dplyr::lag(rgdpe) * 100,
         g_rgdpo = (rgdpo - dplyr::lag(rgdpo))/dplyr::lag(rgdpo) * 100,
         g_rgdpna = (rgdpna - dplyr::lag(rgdpna))/dplyr::lag(rgdpna) * 100,
         g_PE = (PE - dplyr::lag(PE))/dplyr::lag(PE) * 100,
         g_FEG = (FEG - dplyr::lag(FEG))/dplyr::lag(FEG) * 100,
         g_UEG = (UEG - dplyr::lag(UEG))/dplyr::lag(UEG) * 100,
         g_UXG = (UXG - dplyr::lag(UXG))/dplyr::lag(UXG) * 100)


#### (4) Testing for stationarity ####

test <- ES_data[-1,] # The data frame that is used subsequently for testing for stationarity. We get rid of the first row, because growth rates are full of NAs.

# Note on tests:
# - The Augmented Dickey-Fuller (ADF) test needs being rejected; so p-value needs to be < 0.1
# - The Phillips-Perron (PP) test needs being rejected; so p-value needs to be < 0.1
# - The Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test needs being accepted; so p-value needs to be > 0.1

# (4.i) Testing final energy growth rate g_FEG
tseries::adf.test(test$g_FEG)
tseries::pp.test(test$g_FEG)
tseries::kpss.test(test$g_FEG)

tseries::adf.test(diff(test$g_FEG,1))
tseries::pp.test(diff(test$g_FEG,1))
tseries::kpss.test(diff(test$g_FEG,1))
# Once differentiated, all tests are passing.


# (4.ii) Testing useful energy growth rate g_UEG
tseries::adf.test(test$g_UEG)
tseries::pp.test(test$g_UEG)
tseries::kpss.test(test$g_UEG)

tseries::adf.test(diff(test$g_UEG,1))
tseries::pp.test(diff(test$g_UEG,1))
tseries::kpss.test(diff(test$g_UEG,1))
# Once differentiated, all tests are passing.


# (4.iii) Testing useful exergy growth rate g_UXG
tseries::adf.test(test$g_UXG)
tseries::pp.test(test$g_UXG)
tseries::kpss.test(test$g_UXG)

tseries::adf.test(diff(test$g_UXG,1))
tseries::pp.test(diff(test$g_UXG,1))
tseries::kpss.test(diff(test$g_UXG,1))
# Once differentiated, all tests are passing.


# (4.vi) Testing gdp (as rgdpe) growth rate g_rgdpe
tseries::adf.test(test$g_rgdpe)
tseries::pp.test(test$g_rgdpe)
tseries::kpss.test(test$g_rgdpe)

tseries::adf.test(diff(test$g_rgdpe,1))
tseries::pp.test(diff(test$g_rgdpe,1))
tseries::kpss.test(diff(test$g_rgdpe,1))
# Once differentiated, all tests are passing.


# (4.v) Testing gdp (as rgdpo) growth rate g_rgdpo
tseries::adf.test(test$g_rgdpo)
tseries::pp.test(test$g_rgdpo)
tseries::kpss.test(test$g_rgdpo)

tseries::adf.test(diff(test$g_rgdpo,1))
tseries::pp.test(diff(test$g_rgdpo,1))
tseries::kpss.test(diff(test$g_rgdpo,1))
# Once differentiated, all tests are passing.


# (4.vi) Testing gdp (as rgdpna) growth rate g_rgdpna
tseries::adf.test(test$g_rgdpna)
tseries::pp.test(test$g_rgdpna)
tseries::kpss.test(test$g_rgdpna)

tseries::adf.test(diff(test$g_rgdpna,1))
tseries::pp.test(diff(test$g_rgdpna,1))
tseries::kpss.test(diff(test$g_rgdpna,1))
# Once differentiated, all tests are passing.


#### (5) Running causality tests ####

# (5.i) Setting up results data frame

results <- data.frame(
  test_id = integer(0),
  energy = character(0),# what energy variable is used
  gdp = character(0),# what gdp variable is used
  granger_test = character(0),# what causality test procedure is used
  lags = integer(0),# how many lags are included in the VAR model
  ncriteria = integer(0),# how many criteria support the use of the number of lags (minimum = 1; maximum = 4)
  year_1 = integer(0),# beginning year for the test
  year_2 = integer(0),# end year for the test
  time_length = integer(0),# number of years included in the test
  validity = integer(0),
  p_e_to_gdp = numeric(0),# p-value for causality running from energy consumption growth to gdp growth
  p_gdp_to_e = numeric(0) # p-value for causality running from gdp growth to energy consumption growth
)


# (5.ii) Initialising n, which is the ID of the test
n <- 1


# (5.iii) Choosing the energy and gdp variables for which we want to run tests
energy.list <- list("g_FEG", "g_UEG", "g_UXG")
gdp.list <- list("g_rgdpe", "g_rgdpo", "g_rgdpna")


# 5.(iv) Running the tests using the grangertest (lmtest package) function
for (ENERGY in energy.list){
  for (GDP in gdp.list){
    
    dy <- data.frame(
      denergy = diff(ES_data[-1,][[ENERGY]],1), # Taking first - empty - row out and differentiating
      dgdp = diff(ES_data[-1,][[GDP]],1)
    )
    
    # The two following for loops ensure that we are covering every single time period of length >= 30 years, so every time period with >= 30 data points.
    for (l in 30:55){# length of time period
      for (i in 1:(55-l+1)){# varying beginning year within the given length
        
        var_selection <- vars::VARselect(dy[i:(i+l-1),],lag=8,type="both") # Good indexes
        
        # Each of the four criterion returns a (potentially) different lag number
        lag_values <- list(var_selection$selection[[1]], 
                           var_selection$selection[[2]],
                           var_selection$selection[[3]],
                           var_selection$selection[[4]])
        
        # Now we test that for a given number of lags returned by a criterion, the model is stable and non serially correlated
        for (lag in unique(lag_values)){# Unique to avoid testing twice the same model
          
          number_criteria <- (lag_values[1]==lag)*1 + (lag_values[2]==lag)*1 + (lag_values[3]==lag)*1 + (lag_values[4]==lag)*1
          
          var <- vars::VAR(dy[i:(i+l-1),], p = lag, type = "const")
          validity_check <- 1# by default, the model is deemed valid
          
          ser_corr_check <- vars::serial.test(var)[2][[1]][3]$p.value
          stab_check <- max(abs(vars::roots(var)))
          
          # If one of the two conditions (stability and non serial correlation) is not verified, then the model is classified as non valid
          if ((ser_corr_check < 0.1) || (stab_check > 1)){
            validity_check <- 0
          }
          
          # Granger tests + stocking results
          gdp_to_e <- lmtest::grangertest(dy$denergy[i:(i+l-1)] ~ dy$dgdp[i:(i+l-1)], order = lag)
          e_to_gdp <- lmtest::grangertest(dy$dgdp[i:(i+l-1)] ~ dy$denergy[i:(i+l-1)], order = lag)
          
          to_add <- data.frame(
            test_id = n,
            energy = ENERGY,
            gdp = GDP,
            granger_test = "grangertest",
            lags = lag,
            ncriteria = number_criteria,
            year_1 = 1960+i,
            year_2 = 1960+i+l-1,
            time_length = l,
            validity = validity_check,
            p_e_to_gdp = e_to_gdp[4][[1]][2],
            p_gdp_to_e = gdp_to_e[4][[1]][2]
          )
          
          results <- rbind(results, to_add)
          
          n <- n+1
        }
      }
    }
    
  }
}


# 5.(v) Running the tests using the causality (from vars package) function
for (ENERGY in energy.list){
  for (GDP in gdp.list){
    
    dy <- data.frame(
      denergy = diff(ES_data[-1,][[ENERGY]],1), # Taking first - empty - row out and differentiating
      dgdp = diff(ES_data[-1,][[GDP]],1)
    )
    
    # The two following for loops ensure that we are covering every single time period of length >= 30 years, so every time period with >= 30 data points.
    for (l in 30:55){# length of time period
      for (i in 1:(55-l+1)){ # varying beginning year within the given length
        
        var_selection <- vars::VARselect(dy[i:(i+l-1),],lag=8,type="both") # Good indexes
        
        # Each of the four criterion returns a (potentially) different lag number
        lag_values <- list(var_selection$selection[[1]], 
                           var_selection$selection[[2]],
                           var_selection$selection[[3]],
                           var_selection$selection[[4]])
        
        # Now we test that for a given number of lags returned by a criterion, the model is stable and non serially correlated
        for (lag in unique(lag_values)){# Unique to avoid testing twice the same model
          
          number_criteria <- (lag_values[1]==lag)*1 + (lag_values[2]==lag)*1 + (lag_values[3]==lag)*1 + (lag_values[4]==lag)*1
          
          var <- vars::VAR(dy[i:(i+l-1),], p = lag, type = "const")
          validity_check <- 1# by default, the model is deemed valid
          
          ser_corr_check <- vars::serial.test(var)[2][[1]][3]$p.value
          stab_check <- max(abs(vars::roots(var)))
          
          # If one of the two conditions (stability and non serial correlation) is not verified, then the model is classified as non valid
          if ((ser_corr_check < 0.1) || (stab_check > 1)){
            validity_check <- 0
          }
          
          # Granger tests + stocking results
          p_gdp_causing_energy <- vars::causality(var, cause = "dgdp")$Granger
          p_energy_causing_gdp <- vars::causality(var, cause = "denergy")$Granger
          
          to_add <- data.frame(
            test_id = n,
            energy = ENERGY,
            gdp = GDP,
            granger_test = "causality",
            lags = lag,
            ncriteria = number_criteria,
            year_1 = 1960+i,
            year_2 = 1960+i+l-1,
            time_length = l,
            validity = validity_check,
            p_e_to_gdp = p_energy_causing_gdp$p.value,
            p_gdp_to_e = p_gdp_causing_energy$p.value
          )
          
          results <- rbind(results, to_add)
          
          n <- n+1
        }
      }
    }
    
  }
}


# 5.(vi) Running the tests using the Toda-Yamamoto (TY) procedure
# We follow the TY procedure implementation as described in https://datazen.info/toda-yamamoto-implementation-in-r/
for (ENERGY in energy.list){
  for (GDP in gdp.list){
    
    y <- data.frame(
      energy = ES_data[-1,][[ENERGY]],# Taking first - empty - row out
      gdp = ES_data[-1,][[GDP]]
    )
    
    # The two following for loops ensure that we are covering every single time period of length >= 30 years, so every time period with >= 30 data points.
    for (l in 30:56){# length of time period
      for (i in 1:(56-l+1)){# varying beginning year within the given length
        
        var_selection <- vars::VARselect(y[i:(i+l-1),],lag=8,type="both") # Good indexes
        
        # Each of the four criterion returns a (potentially) different lag number
        lag_values <- list(var_selection$selection[[1]], 
                           var_selection$selection[[2]],
                           var_selection$selection[[3]],
                           var_selection$selection[[4]])
        
        # Now we test that for a given number of lags returned by a criterion, the model is stable and non serially correlated
        for (lag in unique(lag_values)){# Unique to avoid testing twice the same model
          
          number_criteria <- (lag_values[1]==lag)*1 + (lag_values[2]==lag)*1 + (lag_values[3]==lag)*1 + (lag_values[4]==lag)*1
          
          var_prelim <- vars::VAR(y[i:(i+l-1),], p = lag, type = "const")
          
          validity_check <- 1# by default, the model is deemed valid
          
          ser_corr_check <- vars::serial.test(var_prelim)$serial$p.value
          stab_check <- max(abs(vars::roots(var_prelim)))
          
          # If one of the two conditions (stability and non serial correlation) is not verified, then the model is classified as non valid
          if (ser_corr_check < 0.1 || stab_check > 1){
            validity_check <- 0
          }
          
          var <- vars::VAR(y[i:(i+l-1),], p = (lag+1), type = "const") # As all these time series are I(1)
          
          # Granger tests + stocking results
          p_gdp_causing_energy <- aod::wald.test(b=coef(var$varresult[[1]]), Sigma=vcov(var$varresult[[1]]), Terms = seq(2,2*lag,2))$result$chi2[3]
          p_energy_causing_gdp <- aod::wald.test(b=coef(var$varresult[[2]]), Sigma=vcov(var$varresult[[2]]), Terms= seq(1,2*lag-1,2))$result$chi2[3]
          
          to_add <- data.frame(
            test_id = n,
            energy = ENERGY,
            gdp = GDP,
            granger_test = "toda-yamamoto",
            lags = lag,
            ncriteria = number_criteria,
            year_1 = 1960+i,
            year_2 = 1960+i+l-1,
            time_length = l,
            validity = validity_check,
            p_e_to_gdp = p_energy_causing_gdp,
            p_gdp_to_e = p_gdp_causing_energy
          )
          
          results <- rbind(results, to_add)
          
          n <- n+1
        }
      }
    }
    
  }
}


#### (6) Exporting results to csv if needed ####
# readr::write_csv(x = results,
#                  path = "",# fill in path.
#                  col_names = TRUE)
