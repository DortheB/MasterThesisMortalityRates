rm(list=ls())

# # Turn the demography data objects into smoothed demography data objects
# library("demography")
# library("dplyr")
# library("plotly")
library("forecast")
library("moments")

# Load demography data objects:
load(file = "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/MortalityModels/ModelResultsLCRH.RData")

# Define function that elivers relevant test statistics
checkForStationarity <- function(df){
  # Fulgt denne metode: http://www.statosphere.com.au/check-time-series-stationary-r/
  
  # df <- modelResults_GR0_m$kappa_data$LC
  
  # plot(df)
  # # Plot ACF and PACF
  # acf_plot <- Acf(df)
  # pacf <- Pacf(df)
  
  # Find the best ARIMA model
  best_arima <- auto.arima(df)
  
  # Take first difference
  df1 <- stats::lag(df, k=1)
  
  # Statistical test for stationarity
  Box_ljung <- Box.test(df1)
  p_value_box <- base::round(Box_ljung$p.value, 2)  # p value less than 0.05 : stationary (dette burde den vel ikke være, da der er stærk trend og derfor skal der tages en difference først?)
  adf <- tseries::adf.test(df1)
  p_value_adf <- base::round(adf$p.value, 2) #  small p-values suggest the data is stationary and doesn’t need to be differenced stationarity.
  kpss <- tseries::kpss.test(df1)
  p_value_kpss <- base::round(kpss$p.value,2) # small p-values suggest that the series is not stationary and a differencing is required
  shapiro <- shapiro.test(df)
  p_value_shapiro <-  base::round(shapiro$p.value, 2) #  if the p-value is less than the chosen alpha level, then the null hypothesis is rejected and there is evidence that the data tested are not from a normally distributed population, https://en.wikipedia.org/wiki/Shapiro%E2%80%93Wilk_test
  jarque <- tseries::jarque.bera.test(df)
  p_value_jarque <-  base::round(jarque$p.value, 2) # For example, a tiny p-value and a large chi-square value from this test means that you can reject the null hypothesis that the data is normally distributed, http://www.statisticshowto.com/jarque-bera-test/
  skewness <- agostino.test(df)
  p_value_skewness <- base::round(skewness$p.value, 2) # the null hypothesis is that all the values were sampled from a Gaussian distribution, https://www.graphpad.com/support/faqid/959/
  kurtosis <- anscombe.test(df) 
  p_value_kurtosis <- base::round(kurtosis$p.value, 2) #  H0: normality, versus the alternative hypothesis H1: non-normality due to kurtosis, http://www.fintools.com/wp-content/uploads/2012/02/NormalityandIndependenceTesting.pdf
  
  return_list <- list("best" = best_arima, "pval_box" = p_value_box, "pval_adf" = p_value_adf, 
                      "pval_kpss" = p_value_kpss, "pval_shapiro" = p_value_shapiro, "pval_jarque" = p_value_jarque, 
                      "pval_skew" = p_value_skewness, "pval_kurt" = p_value_kurtosis) 
  return(return_list)
}

# Obtain test statistics for all considered models
gr0_m_LC <- checkForStationarity(modelResults_GR0_m$kappa_data$LC)
gr0_m_RH <- checkForStationarity(modelResults_GR0_m$kappa_data$RH)
gr0_w_LC <- checkForStationarity(modelResults_GR0_w$kappa_data$LC)
gr0_w_RH <- checkForStationarity(modelResults_GR0_w$kappa_data$RH)
gr1_m_LC <- checkForStationarity(modelResults_GR1_m$kappa_data$LC)
gr1_m_RH <- checkForStationarity(modelResults_GR1_m$kappa_data$RH)
gr1_w_LC <- checkForStationarity(modelResults_GR1_w$kappa_data$LC)
gr1_w_RH <- checkForStationarity(modelResults_GR1_w$kappa_data$RH)
gr2_m_LC <- checkForStationarity(modelResults_GR2_m$kappa_data$LC)
gr2_m_RH <- checkForStationarity(modelResults_GR2_m$kappa_data$RH) 
gr2_w_LC <- checkForStationarity(modelResults_GR2_w$kappa_data$LC)
gr2_w_RH <- checkForStationarity(modelResults_GR2_w$kappa_data$RH)
gr3_m_LC <- checkForStationarity(modelResults_GR3_m$kappa_data$LC)
gr3_m_RH <- checkForStationarity(modelResults_GR3_m$kappa_data$RH)
gr3_w_LC <- checkForStationarity(modelResults_GR3_w$kappa_data$LC)
gr3_w_RH <- checkForStationarity(modelResults_GR3_w$kappa_data$RH)
gr4_m_LC <- checkForStationarity(modelResults_GR4_m$kappa_data$LC)
gr4_m_RH <- checkForStationarity(modelResults_GR4_m$kappa_data$RH)
gr4_w_LC <- checkForStationarity(modelResults_GR4_w$kappa_data$LC)
gr4_w_RH <- checkForStationarity(modelResults_GR4_w$kappa_data$RH)
gr5_m_LC <- checkForStationarity(modelResults_GR5_m$kappa_data$LC)
gr5_m_RH <- checkForStationarity(modelResults_GR5_m$kappa_data$RH)
gr5_w_LC <- checkForStationarity(modelResults_GR5_w$kappa_data$LC)
gr5_w_RH <- checkForStationarity(modelResults_GR5_w$kappa_data$RH)
gr6_m_LC <- checkForStationarity(modelResults_GR6_m$kappa_data$LC)
gr6_m_RH <- checkForStationarity(modelResults_GR6_m$kappa_data$RH)
gr6_w_LC <- checkForStationarity(modelResults_GR6_w$kappa_data$LC)
gr6_w_RH <- checkForStationarity(modelResults_GR6_w$kappa_data$RH)
gr7_m_LC <- checkForStationarity(modelResults_GR7_m$kappa_data$LC)
gr7_m_RH <- checkForStationarity(modelResults_GR7_m$kappa_data$RH)
gr7_w_LC <- checkForStationarity(modelResults_GR7_w$kappa_data$LC)
gr7_w_RH <- checkForStationarity(modelResults_GR7_w$kappa_data$RH)
gr8_m_LC <- checkForStationarity(modelResults_GR8_m$kappa_data$LC)
gr8_m_RH <- checkForStationarity(modelResults_GR8_m$kappa_data$RH)
gr8_w_LC <- checkForStationarity(modelResults_GR8_w$kappa_data$LC)
gr8_w_RH <- checkForStationarity(modelResults_GR8_w$kappa_data$RH)
gr9_m_LC <- checkForStationarity(modelResults_GR9_m$kappa_data$LC)
gr9_m_RH <- checkForStationarity(modelResults_GR9_m$kappa_data$RH)
gr9_w_LC <- checkForStationarity(modelResults_GR9_w$kappa_data$LC)
gr9_w_RH <- checkForStationarity(modelResults_GR9_w$kappa_data$RH)


# Manually write the best models
gr0_m_LC$best
gr0_m_LC$best <- "ARIMA(2,1,0)"
gr0_m_RH$best
gr0_m_RH$best <- "ARIMA(0,1,1)"
gr0_w_LC$best
gr0_w_LC$best <- "ARIMA(1,1,0)"
gr0_w_RH$best
gr0_w_RH$best <- "ARIMA(0,1,1)"

gr1_m_LC$best
gr1_m_LC$best <- "ARIMA(0,1,0)"
gr1_m_RH$best
gr1_m_RH$best <- "ARIMA(0,2,1)"
gr1_w_LC$best
gr1_w_LC$best <- "ARIMA(0,1,0)"
gr1_w_RH$best
gr1_w_RH$best <- "ARIMA(0,1,0)"

gr2_m_LC$best # Non stationary
gr2_m_LC$best <- "df"
# best_gr2_m_LC <- "ARIMA(2,1,0)"
gr2_m_RH$best # Non stationary
gr2_m_RH$best <- "df"
# best_gr2_m_RH <- "ARIMA(0,1,1)"
gr2_w_LC$best
gr2_w_LC$best <- "ARIMA(0,2,2)"
gr2_w_RH$best
gr2_w_RH$best <- "ARIMA(0,1,1)"

gr3_m_LC$best
gr3_m_LC$best <- "ARIMA(1,1,0)"
gr3_m_RH$best
gr3_m_RH$best <- "ARIMA(1,1,0)"
gr3_w_LC$best
gr3_w_LC$best <- "ARIMA(1,1,0)"
gr3_w_RH$best
gr3_w_RH$best <- "ARIMA(1,2,1)"

gr4_m_LC$best
gr4_m_LC$best <- "ARIMA(1,1,0)"
gr4_m_RH$best
gr4_m_RH$best <- "ARIMA(0,2,2)"
gr4_w_LC$best
gr4_w_LC$best <- "ARIMA(1,1,0)"
gr4_w_RH$best
gr4_w_RH$best <- "ARIMA(0,1,1)"

gr5_m_LC$best
gr5_m_LC$best <- "ARIMA(0,1,1)"
gr5_m_RH$best
gr5_m_RH$best <- "ARIMA(0,1,1)"
gr5_w_LC$best
gr5_w_LC$best <- "ARIMA(0,1,1)"
gr5_w_RH$best
gr5_w_RH$best <- "ARIMA(0,2,1)"

gr6_m_LC$best
gr6_m_LC$best <- "ARIMA(3,1,0)"
gr6_m_RH$best
gr6_m_RH$best <- "ARIMA(2,1,1)"
gr6_w_LC$best
gr6_w_LC$best <- "ARIMA(1,1,0)"
gr6_w_RH$best
gr6_w_RH$best <- "ARIMA(2,2,0)"

gr7_m_LC$best
gr7_m_LC$best <- "ARIMA(2,2,0)"
gr7_m_RH$best
gr7_m_RH$best <- "ARIMA(0,1,0)"
gr7_w_LC$best
gr7_w_LC$best <- "ARIMA(0,1,1)"
gr7_w_RH$best
gr7_w_RH$best <- "ARIMA(1,1,0)"

gr8_m_LC$best
gr8_m_LC$best <- "ARIMA(1,1,0)"
gr8_m_RH$best
gr8_m_RH$best <- "ARIMA(0,0,0)"
gr8_w_LC$best
gr8_w_LC$best <- "ARIMA(1,1,0)"
gr8_w_RH$best
gr8_w_RH$best <- "ARIMA(0,2,1)"

gr9_m_LC$best
gr9_m_LC$best <- "ARIMA(0,1,1)"
gr9_m_RH$best
gr9_m_RH$best <- "ARIMA(2,2,2)"
gr9_w_LC$best
gr9_w_LC$best <- "ARIMA(0,1,0)"
gr9_w_RH$best
gr9_w_RH$best <- "ARIMA(1,1,0)"

# Combine the results into one matrix
ARIMA_tests_LC <- c(unname(unlist(gr0_m_LC)), unname(unlist(gr1_m_LC)), #unname(unlist(gr2_m_LC), 
                    unname(unlist(gr3_m_LC)),
                    unname(unlist(gr4_m_LC)), unname(unlist(gr5_m_LC)), unname(unlist(gr6_m_LC)), unname(unlist(gr7_m_LC)),
                    unname(unlist(gr8_m_LC)), unname(unlist(gr9_m_LC)),
                    unname(unlist(gr0_w_LC)), unname(unlist(gr1_w_LC)), unname(unlist(gr2_w_LC)), unname(unlist(gr3_w_LC)),
                    unname(unlist(gr4_w_LC)), unname(unlist(gr5_w_LC)), unname(unlist(gr6_w_LC)), unname(unlist(gr7_w_LC)),
                    unname(unlist(gr8_w_LC)), unname(unlist(gr9_w_LC)))
ARIMA_tests_RH <- c(unname(unlist(gr0_m_RH)), unname(unlist(gr1_m_RH)), #unname(unlist(gr2_m_RH), 
                    unname(unlist(gr3_m_RH)),
                    unname(unlist(gr4_m_RH)), unname(unlist(gr5_m_RH)), unname(unlist(gr6_m_RH)), unname(unlist(gr7_m_RH)),
                    unname(unlist(gr8_m_RH)), unname(unlist(gr9_m_RH)),
                    unname(unlist(gr0_w_RH)), unname(unlist(gr1_w_RH)), unname(unlist(gr2_w_RH)), unname(unlist(gr3_w_RH)),
                    unname(unlist(gr4_w_RH)), unname(unlist(gr5_w_RH)), unname(unlist(gr6_w_RH)), unname(unlist(gr7_w_RH)),
                    unname(unlist(gr8_w_RH)), unname(unlist(gr9_w_RH)))
ARIMA_LC_matrix <- matrix(ARIMA_tests_LC, ncol = 8, nrow = 19, byrow=TRUE)
ARIMA_RH_matrix <- matrix(ARIMA_tests_RH, ncol = 8, nrow = 19, byrow=TRUE)
rownames(ARIMA_LC_matrix) <- c("Gr 1, m", "Gr 2, m", #"Gr 3, m", 
                            "Gr 4, m",
                            "Gr 5, m", "Gr 6, m", "Gr 7, m", "Gr 8, m", 
                            "Gr 9, m", "Gr 10, m", 
                            "Gr 1, w", "Gr 2, w", "Gr 3, w", "Gr 4, w",
                            "Gr 5, w", "Gr 6, w", "Gr 7, w", "Gr 8, w", 
                            "Gr 9, w", "Gr 10, w")
rownames(ARIMA_RH_matrix) <- c("Gr 1, m", "Gr 2, m", #"Gr 3, m", 
                               "Gr 4, m",
                               "Gr 5, m", "Gr 6, m", "Gr 7, m", "Gr 8, m", 
                               "Gr 9, m", "Gr 10, m", 
                               "Gr 1, w", "Gr 2, w", "Gr 3, w", "Gr 4, w",
                               "Gr 5, w", "Gr 6, w", "Gr 7, w", "Gr 8, w", 
                               "Gr 9, w", "Gr 10, w")
# gr0_m_LC)
colnames(ARIMA_LC_matrix) <- c("Best ARIMA", "Ljung-Box", "ADF", "KPSS", "Shapiro-Wilks", "Jarque-Bera", "d'Agostino", "anscombe")
colnames(ARIMA_RH_matrix) <- c("Best ARIMA", "Ljung-Box", "ADF", "KPSS", "Shapiro-Wilks", "Jarque-Bera", "d'Agostino", "anscombe")

print(xtable::xtable(ARIMA_LC_matrix, type = "latex"), file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Tables/ARIMA_LC_table.tex")
print(xtable::xtable(ARIMA_RH_matrix, type = "latex"), file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Tables/ARIMA_RH_table.tex")
