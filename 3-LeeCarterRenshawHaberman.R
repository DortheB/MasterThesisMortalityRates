# File that fit LC model and Renshaw Haberman model the ordinary way - and delivers:
# parameters, in sample mortality rates and forecasted mortality rates

# 1: Clean environment and load packages ----

rm(list=ls())

library("StMoMo")
library("plotly")
library("dplyr")
library("plotly")
library("demography")
library("xtable")



# 
# # User specified functions ----
# source("C:/Users/Dorthe B/OneDrive/Speciale/R_Code/MortalityModels/MortalityModelsHelperFunctions.R")
# 
# 
# # Load data and turn into StMoMo data objects ----
# load("C:/Users/Dorthe B/OneDrive/Speciale/R_Code/MortalityModels/SmoothedDemogData_spline.RData")
# # load("C:/Users/Dorthe B/OneDrive/Speciale/R_Code/MortalityModels/SmoothedDemogData.RData")
# 
# # Convert into stmomo object: Husk series => rigtige køn er valgt!
# data_GR0_m <- StMoMoData(data_GR0_demog_smoothed, series = "men") # create stmomo object from demogdata object
# data_GR0_w <- StMoMoData(data_GR0_demog_smoothed, series = "women") # create stmomo object from demogdata object
# data_GR1_m <- StMoMoData(data_GR1_demog_smoothed, series = "men") # create stmomo object from demogdata object
# data_GR1_w <- StMoMoData(data_GR1_demog_smoothed, series = "women") # create stmomo object from demogdata object
# data_GR2_m <- StMoMoData(data_GR2_demog_smoothed, series = "men") # create stmomo object from demogdata object
# data_GR2_w <- StMoMoData(data_GR2_demog_smoothed, series = "women") # create stmomo object from demogdata object
# data_GR3_m <- StMoMoData(data_GR3_demog_smoothed, series = "men") # create stmomo object from demogdata object
# data_GR3_w <- StMoMoData(data_GR3_demog_smoothed, series = "women") # create stmomo object from demogdata object
# data_GR4_m <- StMoMoData(data_GR4_demog_smoothed, series = "men") # create stmomo object from demogdata object
# data_GR4_w <- StMoMoData(data_GR4_demog_smoothed, series = "women") # create stmomo object from demogdata object
# data_GR5_m <- StMoMoData(data_GR5_demog_smoothed, series = "men") # create stmomo object from demogdata object
# data_GR5_w <- StMoMoData(data_GR5_demog_smoothed, series = "women") # create stmomo object from demogdata object
# data_GR6_m <- StMoMoData(data_GR6_demog_smoothed, series = "men") # create stmomo object from demogdata object
# data_GR6_w <- StMoMoData(data_GR6_demog_smoothed, series = "women") # create stmomo object from demogdata object
# data_GR7_m <- StMoMoData(data_GR7_demog_smoothed, series = "men") # create stmomo object from demogdata object
# data_GR7_w <- StMoMoData(data_GR7_demog_smoothed, series = "women") # create stmomo object from demogdata object
# data_GR8_m <- StMoMoData(data_GR8_demog_smoothed, series = "men") # create stmomo object from demogdata object
# data_GR8_w <- StMoMoData(data_GR8_demog_smoothed, series = "women") # create stmomo object from demogdata object
# data_GR9_m <- StMoMoData(data_GR9_demog_smoothed, series = "men") # create stmomo object from demogdata object
# data_GR9_w <- StMoMoData(data_GR9_demog_smoothed, series = "women") # create stmomo object from demogdata object
# data_GR_total_m <- StMoMoData(data_GR_total_demog_smoothed, series = "men") # create stmomo object from demogdata object
# data_GR_total_w <- StMoMoData(data_GR_total_demog_smoothed, series = "women") # create stmomo object from demogdata object
# 
# rm(list = c("data_GR0_demog_smoothed", 'data_GR1_demog_smoothed', 'data_GR2_demog_smoothed', 
#             'data_GR3_demog_smoothed', 'data_GR4_demog_smoothed', 'data_GR5_demog_smoothed',
#             'data_GR6_demog_smoothed', 'data_GR7_demog_smoothed', 'data_GR8_demog_smoothed',
#             'data_GR9_demog_smoothed', 'data_GR_total_demog_smoothed'))
# 
# # Fit the Lee-Carter model and Renshaw-Haberman model to all data sets (could be done in demography instead) ----
# 
# 
# # Function to give in sample fits for the two models
# getInSampleModelValues <- function(df, inSample_LC, inSample_RH, considered_age_group){
#   
#   # df <- df
#   # considered_age_group <- "60"
#   # inSample_LC <- inSample_LC
#   # inSampleRH <-inSample_RH
#   
#   inSampleOneAge_LC <- changeToDataframe(inSample_LC[which(inSample_LC$`row.names(df)` == considered_age_group) , ], "time")
#   inSampleOneAge_LC <- inSampleOneAge_LC[ -1 , ]
#   inSampleOneAge_LC$`row.names(df)` <- substr(inSampleOneAge_LC$`row.names(df)`, start = 2, stop = 5)
#   inSampleOneAge_LC[ , ] <- apply(inSampleOneAge_LC[ , ], 2,  function(x) as.numeric(as.character(x)))
#   inSampleOneAge_RH <- changeToDataframe(inSample_RH[which(inSample_RH$`row.names(df)` == considered_age_group) , ], "time")
#   inSampleOneAge_RH <- inSampleOneAge_RH[ -1 , ]
#   inSampleOneAge_RH$`row.names(df)` <- substr(inSampleOneAge_RH$`row.names(df)`, start = 2, stop = 5)
#   inSampleOneAge_RH[ , ] <- apply(inSampleOneAge_RH[ , ], 2,  function(x) as.numeric(as.character(x)))
#   # Combine into one df
#   inSample_data <- full_join(inSampleOneAge_LC, inSampleOneAge_RH, by = "row.names(df)") # first LC then RH
#   
#   return(inSample_data)
# }
# 
# # Function to return model parameters for LC and Renshaw-Haberman model (StMoMo-pacage)
# obtainModelValues <- function(df){
#   
#   # df <- data_GR2_m
#   
#   # LC model
#   print("About to fit LC")
#   LCfit <-  fit(lc(), data = df, ages.fit = ages)
#   print("Have fitted LC")
#   alpha_LC <- changeToDataframe(LCfit$ax, "age")
#   beta_LC <- changeToDataframe(LCfit$bx, "age")  # LC modellens beta paramter
#   kappa_LC  <- changeToDataframe(LCfit$kt, "time")
#   
#   # Cohort model
#   tol <- 1e-02 # 01 for gr1_m
#   print("About to fit RH")
#   RHfit <- fit(rh(), data = df, ages.fit = ages, tolerance = tol) #, start.ax = LCfit$ax, start.bx = LCfit$bx, start.kt = LCfit$kt) # start => speed up time
#   print("Have fitted RH")
#   alpha_RH <- changeToDataframe(RHfit$ax, "age")
#   beta_RH <- changeToDataframe(RHfit$bx, "age") # kaldes beta^1 i plottet over alle
#   kappa_RH <- changeToDataframe(RHfit$kt, "time")
#   beta_gamma_RH <- changeToDataframe(RHfit$b0x, "age") # kaldes beta^0 i plottet over alle, tror det er gamma
#   gamma_RH <- changeToDataframe(RHfit$gc, "cohort") # Den kaldes cohort i plottet med alle
#   
#   # Combine estimated parameters into one data frame
#   alpha_data <- full_join(alpha_LC, alpha_RH, by = "row.names(df)")
#   colnames(alpha_data) <- c("Age", "LC", "RH")
#   beta_data <- full_join(beta_LC, beta_RH, by = "row.names(df)")
#   colnames(beta_data) <- c("Age", "LC", "RH")
#   kappa_data <- full_join(kappa_LC, kappa_RH, by = "row.names(df)")
#   colnames(kappa_data) <- c("Age", "LC", "RH")
#   beta_gamma_data <- beta_gamma_RH
#   colnames(beta_gamma_data) <- c("Age", "RH")
#   gamma_data <- gamma_RH
#   colnames(gamma_data) <- c("Cohort", "RH")
#   
#   # In sample values
#   forecasts_LC <- forecast::forecast(LCfit)
#   inSample_LC <- changeToDataframe(forecasts_LC$fitted, "age") # Empirical results on the considered period
#   forecasts_RH <- forecast(RHfit)
#   inSample_RH <- changeToDataframe(forecasts_RH$fitted, "age") # Empirical results on the considered period
#   # Format data and combine into one data frame
#   inSample60_data <- getInSampleModelValues(df, inSample_LC, inSample_RH, "60") #
#   inSample70_data <- getInSampleModelValues(df, inSample_LC, inSample_RH, "70")
#   inSample80_data <- getInSampleModelValues(df, inSample_LC, inSample_RH, "80")
#   inSample90_data <- getInSampleModelValues(df, inSample_LC, inSample_RH, "90")
#   
#   return_list <- list("alpha_data" = alpha_data, "beta_data" = beta_data, 
#                       "kappa_data" = kappa_data, "beta_gamma_data" = beta_gamma_data, 
#                       "gamma_data" = gamma_data, "inSample60_data" = inSample60_data, 
#                       "inSample60_data", inSample60_data, "inSample70_data" = inSample70_data, 
#                       "inSample80_data" = inSample80_data, "inSample90_data" = inSample90_data,
#                       "RHfit" = RHfit, "LCfit" = LCfit)
#   
#   rm(inSample_LC, inSample_RH)
#   
#   return(return_list)
# }
# 
# # Fit models: obtain modelparameters and results
# modelResults_GR0_m <- obtainModelValues(data_GR0_m)
# modelResults_GR0_w <- obtainModelValues(data_GR0_w)
# modelResults_GR1_m <- obtainModelValues(data_GR1_m)
# modelResults_GR1_w <- obtainModelValues(data_GR1_w)
# modelResults_GR2_m <- obtainModelValues(data_GR2_m)
# modelResults_GR2_w <- obtainModelValues(data_GR2_w)
# modelResults_GR3_m <- obtainModelValues(data_GR3_m)
# modelResults_GR3_w <- obtainModelValues(data_GR3_w)
# modelResults_GR4_m <- obtainModelValues(data_GR4_m)
# modelResults_GR4_w <- obtainModelValues(data_GR4_w)
# modelResults_GR5_m <- obtainModelValues(data_GR5_m)
# modelResults_GR5_w <- obtainModelValues(data_GR5_w)
# modelResults_GR6_m <- obtainModelValues(data_GR6_m)
# modelResults_GR6_w <- obtainModelValues(data_GR6_w)
# modelResults_GR7_m <- obtainModelValues(data_GR7_m)
# modelResults_GR7_w <- obtainModelValues(data_GR7_w)
# modelResults_GR8_m <- obtainModelValues(data_GR8_m)
# modelResults_GR8_w <- obtainModelValues(data_GR8_w) 
# modelResults_GR9_m <- obtainModelValues(data_GR9_m) 
# modelResults_GR9_w <- obtainModelValues(data_GR9_w) 
# # modelResults_GR_total_m <- obtainModelValues(data_GR_total_m)
# # modelResults_GR_total_w <- obtainModelValues(data_GR_total_w)
# 
# # Clean and save ----
# rm(list=setdiff(ls(), c("modelResults_GR0_m", "modelResults_GR0_w", "modelResults_GR1_m", "modelResults_GR1_w",
#                         "modelResults_GR2_m", "modelResults_GR2_w", "modelResults_GR3_m", "modelResults_GR3_w",
#                         "modelResults_GR4_m", "modelResults_GR4_w", "modelResults_GR5_m", "modelResults_GR5_w",
#                         "modelResults_GR6_m", "modelResults_GR6_w", "modelResults_GR7_m", "modelResults_GR7_w",
#                         "modelResults_GR8_m", "modelResults_GR8_w", "modelResults_GR9_m", "modelResults_GR9_w",
#                         "modelResults_GR_total_m", "modelResults_GR_total_w", 
#                         "data_GR0_m", "data_GR0_w", "data_GR1_m", "data_GR1_w", 
#                         "data_GR2_m", "data_GR2_w", "data_GR3_m", "data_GR3_w",
#                         "data_GR4_m", "data_GR4_w", "data_GR5_m", "data_GR5_w",
#                         "data_GR6_m", "data_GR6_w", "data_GR7_m", "data_GR7_w", 
#                         "data_GR8_m", "data_GR8_w", "data_GR9_m", "data_GR9_w")))
# 
# # save.image(file = "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/MortalityModels/ModelResultsLCRH.RData")
# 
# 
# 

# LOADD DATA ----
# Burde ikke være problemer med nogen nu!
load(file = "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/MortalityModels/ModelResultsLCRH.RData")
load(file = "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/MortalityModels/ModelResults_bayesian.RData")

# General settings ----
x_legend_age <- list(title = "Age")
y_legend <- list(title = "Mortality rate")
y_log_legend <- list(title = "Mortality rate (log scale)")
x_legend_time <- list(title = "Year")
dashes <- c("line", "dot", "dash", "line", "dot", "dash", "line", "dot", "dash", "line", "dot", "dash")
colors <- c("rgb(239, 138, 0)", "rgb(248, 61, 0)", "rgb(153, 0, 102)", "rgb(25, 0, 229)", "rgb(0, 0, 0)") #
ages <- 55:94
# Plotly
Sys.setenv("plotly_username"="DortheB") 
Sys.setenv("plotly_api_key"="U6RdI9TaQ3XMOXNrRonT")
Sys.setenv("plotly_username"="DortheB1") 
Sys.setenv("plotly_api_key"="jxl31GEwe3p8FlyPhSAF")




# PLOT PARAMETERS -----

# Plot functions
plot3Parameter <- function(df, y_legend,x_legend){
  
  # df <- df_alpha
  # y_legend <- "Alpha"
  # x_legend <- "Age"
  
  # Ensure numeric
  df[,1] <- as.numeric(paste(df[,1]))
  colnames(df)[1] <- c("X")
  
  plot_par <- plot_ly(df, x = ~X) %>%
    add_trace(y = ~LC, name = 'Lee-Carter', mode = 'lines', type = 'scatter', line = list(color = colors[1], width = 2, dash = "line"), showlegend = FALSE) %>%
    add_trace(y = ~RH, name = 'Cohort Ord', mode = 'lines', type = 'scatter', line = list(color = colors[2], width = 2, dash = "line"), showlegend = FALSE) %>%
    add_trace(y = ~RH2, name = 'Cohort Bay', mode = 'lines', type = 'scatter', line = list(color = colors[4], width = 2, dash = "line"), showlegend = FALSE) %>%
    #add_trace(y = ~RH2, name = 'Cohort Bay', mode = 'markers', type = 'scatter', marker = list(color = 'black', symbol = 'cross'), showlegend = TRUE) #%>%
    layout(xaxis = x_legend_age <- list(title = x_legend), yaxis = x_legend_age <- list(title = y_legend))
  return(plot_par)
}
plot2Parameter <- function(df, y_legend,x_legend){
  
  # df <- df_gamma
  # y_legend <- "Gamma"
  # x_legend <- "YearOfBirth"
  
  # Ensure numeric
  df[,1] <- as.numeric(paste(df[,1]))
  colnames(df)[1] <- c("X")
  
  plot_par <- plot_ly(df, x = ~X) %>%
    add_trace(y = ~RH, name = 'Cohort Ord', mode = 'lines', type = 'scatter', line = list(color = colors[2], width = 2, dash = "line"), showlegend = FALSE) %>%
    add_trace(y = ~RH2, name = 'Cohort Bay', mode = 'lines', type = 'scatter', line = list(color = colors[4], width = 2, dash = "line"), showlegend = FALSE) %>%
    #add_trace(y = ~RH2, name = 'Cohort Bay', mode = 'markers', type = 'scatter', marker = list(color = 'black', symbol = 'cross'), showlegend = TRUE) #%>%
    layout(xaxis = x_legend_age <- list(title = x_legend), yaxis = x_legend_age <- list(title = y_legend))
  return(plot_par)
}

# Tidligere er LC, RH og actuals joinet. Dette bør vores state space models også blive
# inSample_data <- full_join(inSample_actuals, inSampleOneAge_LC, by = "row.names(df)") # SKAL VRE ACTUALS


# GR1, M

df <- modelResults_GR0_w
df_alpha <- df$alpha_data
df_beta <- df$beta_data
df_betag <- df$beta_gamma_data
df_kappa <- df$kappa_data
df_gamma <- df$gamma_data

# Add bayesian results
df_alpha$RH2 <- modelResults_GR0_w_rh$alpha_data[,3]
df_beta$RH2 <- modelResults_GR0_w_rh$beta_data[,3]
df_betag$RH2 <- modelResults_GR0_w_rh$beta_gamma_data[,2]
df_kappa$RH2 <- modelResults_GR0_w_rh$kappa_data[,2] # Findes ikek i LC
df_gamma$RH2 <- modelResults_GR0_w_rh$gamma_data[,2] # Findes ikke i LC

# For at kunne sætte de rigitge navne på, gemmes de hver især.
alpha_plot <- plot3Parameter(df_alpha, "", "Age")
beta_plot <- plot3Parameter(df_beta, "", "Age")
kappa_plot <- plot3Parameter(df_kappa, "", "Year")
df_betag$RH2 <- 0.03
betag_plot <- plot2Parameter(df_betag, "", "Year")
gamma_plot <- plot2Parameter(df_gamma, "", "Year-of-birth")

# plotly_IMAGE(alpha_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/alpha_w1.png")
# plotly_IMAGE(beta_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/beta_w1.png")
# plotly_IMAGE(kappa_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/kappa_w1.png")
plotly_IMAGE(betag_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/betag_w1.png")
# plotly_IMAGE(gamma_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/gamma_w1.png")





# GR2, M

df <- modelResults_GR1_w
df_alpha <- df$alpha_data
df_beta <- df$beta_data
df_betag <- df$beta_gamma_data
df_kappa <- df$kappa_data
df_gamma <- df$gamma_data

# Add bayesian results
df_alpha$RH2 <- modelResults_GR1_w_rh$alpha_data[,3]
df_beta$RH2 <- modelResults_GR1_w_rh$beta_data[,3]
df_betag$RH2 <- modelResults_GR1_w_rh$beta_gamma_data[,2]
df_kappa$RH2 <- modelResults_GR1_w_rh$kappa_data[,2] # Findes ikek i LC
df_gamma$RH2 <- modelResults_GR1_w_rh$gamma_data[,2] # Findes ikke i LC

# For at kunne sætte de rigitge navne på, gemmes de hver især.
alpha_plot <- plot3Parameter(df_alpha, "", "Age")
beta_plot <- plot3Parameter(df_beta, "", "Age")
kappa_plot <- plot3Parameter(df_kappa, "", "Year")
betag_plot <- plot2Parameter(df_betag, "", "Year")
gamma_plot <- plot2Parameter(df_gamma, "", "Year-of-birth")

# plotly_IMAGE(alpha_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/alpha_w2.png")
# plotly_IMAGE(beta_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/beta_w2.png")
# plotly_IMAGE(kappa_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/kappa_w2.png")
# plotly_IMAGE(betag_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/betag_w2.png")
# plotly_IMAGE(gamma_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/gamma_w2.png")
# 

# GR3, M

df <- modelResults_GR2_w
df_alpha <- df$alpha_data
df_beta <- df$beta_data
df_betag <- df$beta_gamma_data
df_kappa <- df$kappa_data
df_gamma <- df$gamma_data

# Add bayesian results
df_alpha$RH2 <- modelResults_GR2_w_rh$alpha_data[,3]
df_beta$RH2 <- modelResults_GR2_w_rh$beta_data[,3]
df_betag$RH2 <- modelResults_GR2_w_rh$beta_gamma_data[,2]
df_kappa$RH2 <- modelResults_GR2_w_rh$kappa_data[,2] # Findes ikek i LC
df_gamma$RH2 <- modelResults_GR2_w_rh$gamma_data[,2] # Findes ikke i LC

# For at kunne sætte de rigitge navne på, gemmes de hver især.
alpha_plot <- plot3Parameter(df_alpha, "", "Age")
beta_plot <- plot3Parameter(df_beta, "", "Age")
kappa_plot <- plot3Parameter(df_kappa, "", "Year")
betag_plot <- plot2Parameter(df_betag, "", "Year")
gamma_plot <- plot2Parameter(df_gamma, "", "Year-of-birth")

# plotly_IMAGE(alpha_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/alpha_w3.png")
# plotly_IMAGE(beta_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/beta_w3.png")
# plotly_IMAGE(kappa_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/kappa_w3.png")
# plotly_IMAGE(betag_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/betag_w3.png")
# plotly_IMAGE(gamma_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/gamma_w3.png")


# GR4, M

df <- modelResults_GR3_w
df_alpha <- df$alpha_data
df_beta <- df$beta_data
df_betag <- df$beta_gamma_data
df_kappa <- df$kappa_data
df_gamma <- df$gamma_data

# Add bayesian results
df_alpha$RH2 <- modelResults_GR3_w_rh$alpha_data[,3]
df_beta$RH2 <- modelResults_GR3_w_rh$beta_data[,3]
df_betag$RH2 <- modelResults_GR3_w_rh$beta_gamma_data[,2]
df_kappa$RH2 <- modelResults_GR3_w_rh$kappa_data[,2] # Findes ikek i LC
df_gamma$RH2 <- modelResults_GR3_w_rh$gamma_data[,2] # Findes ikke i LC

# For at kunne sætte de rigitge navne på, gemmes de hver især.
alpha_plot <- plot3Parameter(df_alpha, "", "Age")
beta_plot <- plot3Parameter(df_beta, "", "Age")
kappa_plot <- plot3Parameter(df_kappa, "", "Year")
betag_plot <- plot2Parameter(df_betag, "", "Year")
gamma_plot <- plot2Parameter(df_gamma, "", "Year-of-birth")

# plotly_IMAGE(alpha_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/alpha_w4.png")
# plotly_IMAGE(beta_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/beta_w4.png")
# plotly_IMAGE(kappa_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/kappa_w4.png")
# plotly_IMAGE(betag_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/betag_w4.png")
# plotly_IMAGE(gamma_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/gamma_w4.png")


# GR5, M

df <- modelResults_GR4_w
df_alpha <- df$alpha_data
df_beta <- df$beta_data
df_betag <- df$beta_gamma_data
df_kappa <- df$kappa_data
df_gamma <- df$gamma_data

# Add bayesian results
df_alpha$RH2 <- modelResults_GR4_w_rh$alpha_data[,3]
df_beta$RH2 <- modelResults_GR4_w_rh$beta_data[,3]
df_betag$RH2 <- modelResults_GR4_w_rh$beta_gamma_data[,2]
df_kappa$RH2 <- modelResults_GR4_w_rh$kappa_data[,2] # Findes ikek i LC
df_gamma$RH2 <- modelResults_GR4_w_rh$gamma_data[,2] # Findes ikke i LC

# For at kunne sætte de rigitge navne på, gemmes de hver især.
alpha_plot <- plot3Parameter(df_alpha, "", "Age")
beta_plot <- plot3Parameter(df_beta, "", "Age")
kappa_plot <- plot3Parameter(df_kappa, "", "Year")
betag_plot <- plot2Parameter(df_betag, "", "Year")
gamma_plot <- plot2Parameter(df_gamma, "", "Year-of-birth")

# plotly_IMAGE(alpha_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/alpha_w5.png")
# plotly_IMAGE(beta_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/beta_w5.png")
# plotly_IMAGE(kappa_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/kappa_w5.png")
# plotly_IMAGE(betag_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/betag_w5.png")
# plotly_IMAGE(gamma_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/gamma_w5.png")


# GR6, M

df <- modelResults_GR5_w
df_alpha <- df$alpha_data
df_beta <- df$beta_data
df_betag <- df$beta_gamma_data
df_kappa <- df$kappa_data
df_gamma <- df$gamma_data

# Add bayesian results
df_alpha$RH2 <- modelResults_GR5_w_rh$alpha_data[,3]
df_beta$RH2 <- modelResults_GR5_w_rh$beta_data[,3]
df_betag$RH2 <- modelResults_GR5_w_rh$beta_gamma_data[,2]
df_kappa$RH2 <- modelResults_GR5_w_rh$kappa_data[,2] # Findes ikek i LC
df_gamma$RH2 <- modelResults_GR5_w_rh$gamma_data[,2] # Findes ikke i LC

# For at kunne sætte de rigitge navne på, gemmes de hver især.
alpha_plot <- plot3Parameter(df_alpha, "", "Age")
beta_plot <- plot3Parameter(df_beta, "", "Age")
kappa_plot <- plot3Parameter(df_kappa, "", "Year")
betag_plot <- plot2Parameter(df_betag, "", "Year")
gamma_plot <- plot2Parameter(df_gamma, "", "Year-of-birth")

# plotly_IMAGE(alpha_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/alpha_w6.png")
# plotly_IMAGE(beta_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/beta_w6.png")
# plotly_IMAGE(kappa_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/kappa_w6.png")
# plotly_IMAGE(betag_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/betag_w6.png")
# plotly_IMAGE(gamma_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/gamma_w6.png")


# GR7, M

df <- modelResults_GR6_w
df_alpha <- df$alpha_data
df_beta <- df$beta_data
df_betag <- df$beta_gamma_data
df_kappa <- df$kappa_data
df_gamma <- df$gamma_data

# Add bayesian results
df_alpha$RH2 <- modelResults_GR6_w_rh$alpha_data[,3]
df_beta$RH2 <- modelResults_GR6_w_rh$beta_data[,3]
df_betag$RH2 <- modelResults_GR6_w_rh$beta_gamma_data[,2]
df_kappa$RH2 <- modelResults_GR6_w_rh$kappa_data[,2] # Findes ikek i LC
df_gamma$RH2 <- modelResults_GR6_w_rh$gamma_data[,2] # Findes ikke i LC

# For at kunne sætte de rigitge navne på, gemmes de hver især.
alpha_plot <- plot3Parameter(df_alpha, "", "Age")
beta_plot <- plot3Parameter(df_beta, "", "Age")
kappa_plot <- plot3Parameter(df_kappa, "", "Year")
betag_plot <- plot2Parameter(df_betag, "", "Year")
gamma_plot <- plot2Parameter(df_gamma, "", "Year-of-birth")

# plotly_IMAGE(alpha_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/alpha_w7.png")
# plotly_IMAGE(beta_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/beta_w7.png")
# plotly_IMAGE(kappa_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/kappa_w7.png")
# plotly_IMAGE(betag_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/betag_w7.png")
# plotly_IMAGE(gamma_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/gamma_w7.png")
# 

# GR8, M

df <- modelResults_GR7_w
df_alpha <- df$alpha_data
df_beta <- df$beta_data
df_betag <- df$beta_gamma_data
df_kappa <- df$kappa_data
df_gamma <- df$gamma_data

# Add bayesian results
df_alpha$RH2 <- modelResults_GR7_w_rh$alpha_data[,3]
df_beta$RH2 <- modelResults_GR7_w_rh$beta_data[,3]
df_betag$RH2 <- modelResults_GR7_w_rh$beta_gamma_data[,2]
df_kappa$RH2 <- modelResults_GR7_w_rh$kappa_data[,2] # Findes ikek i LC
df_gamma$RH2 <- modelResults_GR7_w_rh$gamma_data[,2] # Findes ikke i LC

# For at kunne sætte de rigitge navne på, gemmes de hver især.
alpha_plot <- plot3Parameter(df_alpha, "", "Age")
beta_plot <- plot3Parameter(df_beta, "", "Age")
kappa_plot <- plot3Parameter(df_kappa, "", "Year")
betag_plot <- plot2Parameter(df_betag, "", "Year")
gamma_plot <- plot2Parameter(df_gamma, "", "Year-of-birth")

# plotly_IMAGE(alpha_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/alpha_w8.png")
# plotly_IMAGE(beta_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/beta_w8.png")
# plotly_IMAGE(kappa_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/kappa_w8.png")
# plotly_IMAGE(betag_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/betag_w8.png")
# plotly_IMAGE(gamma_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/gamma_w8.png")


# GR9, M

df <- modelResults_GR8_w
df_alpha <- df$alpha_data
df_beta <- df$beta_data
df_betag <- df$beta_gamma_data
df_kappa <- df$kappa_data
df_gamma <- df$gamma_data

# Add bayesian results
df_alpha$RH2 <- modelResults_GR8_w_rh$alpha_data[,3]
df_beta$RH2 <- modelResults_GR8_w_rh$beta_data[,3]
df_betag$RH2 <- modelResults_GR8_w_rh$beta_gamma_data[,2]
df_kappa$RH2 <- modelResults_GR8_w_rh$kappa_data[,2] # Findes ikek i LC
df_gamma$RH2 <- modelResults_GR8_w_rh$gamma_data[,2] # Findes ikke i LC

# For at kunne sætte de rigitge navne på, gemmes de hver især.
alpha_plot <- plot3Parameter(df_alpha, "", "Age")
beta_plot <- plot3Parameter(df_beta, "", "Age")
kappa_plot <- plot3Parameter(df_kappa, "", "Year")
betag_plot <- plot2Parameter(df_betag, "", "Year")
gamma_plot <- plot2Parameter(df_gamma, "", "Year-of-birth")

# plotly_IMAGE(alpha_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/alpha_w9.png")
# plotly_IMAGE(beta_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/beta_w9.png")
# plotly_IMAGE(kappa_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/kappa_w9.png")
# plotly_IMAGE(betag_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/betag_w9.png")
# plotly_IMAGE(gamma_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/gamma_w9.png")


# GR10, M

df <- modelResults_GR9_w
df_alpha <- df$alpha_data
df_beta <- df$beta_data
df_betag <- df$beta_gamma_data
df_kappa <- df$kappa_data
df_gamma <- df$gamma_data

# Add bayesian results
df_alpha$RH2 <- modelResults_GR9_w_rh$alpha_data[,3]
df_beta$RH2 <- modelResults_GR9_w_rh$beta_data[,3]
df_betag$RH2 <- modelResults_GR9_w_rh$beta_gamma_data[,2]
df_kappa$RH2 <- modelResults_GR9_w_rh$kappa_data[,2] # Findes ikek i LC
df_gamma$RH2 <- modelResults_GR9_w_rh$gamma_data[,2] # Findes ikke i LC

# For at kunne sætte de rigitge navne på, gemmes de hver især.
alpha_plot <- plot3Parameter(df_alpha, "", "Age")
beta_plot <- plot3Parameter(df_beta, "", "Age")
kappa_plot <- plot3Parameter(df_kappa, "", "Year")
betag_plot <- plot2Parameter(df_betag, "", "Year")
gamma_plot <- plot2Parameter(df_gamma, "", "Year-of-birth")

# plotly_IMAGE(alpha_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/alpha_w10.png")
# plotly_IMAGE(beta_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/beta_w10.png")
# plotly_IMAGE(kappa_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/kappa_w10.png")
# plotly_IMAGE(betag_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/betag_w10.png")
# plotly_IMAGE(gamma_plot, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/gamma_w10.png")





# PLOT IN SAMPLE FIT ----

# load("C:/Users/Dorthe B/OneDrive/Speciale/R_Code/MortalityModels/SmoothedDemogData.RData")
# # Load data and turn into StMoMo data objects ----
load("C:/Users/Dorthe B/OneDrive/Speciale/R_Code/MortalityModels/SmoothedDemogData_spline.RData")
# load("C:/Users/Dorthe B/OneDrive/Speciale/R_Code/MortalityModels/SmoothedDemogData.RData")

# Turn the estimated parameters into dfs 
changeToDataframe <- function(matrix, functionOf){
  
  # matrix <- LCfit$kt
  # functionOf <- "time"
  
  if(functionOf == "time"){
    isFunctionOfTime <- TRUE
    isFunctionOfAge <- FALSE
  } else {
    isFunctionOfTime <- FALSE
    isFunctionOfAge <- TRUE
  }
  
  if(isFunctionOfAge){
    df <- data.frame(matrix)
    df <- cbind(row.names(df), df)
  }
  if(isFunctionOfTime){
    df <- data.frame(t(matrix))
    df <- cbind(row.names(df), df)
  }
  
  return(df)
}

getActuals <- function(df){
  
  # df <- data_GR0_demog_smoothed$rate$men
  
  inSample_actuals <- changeToDataframe(df, "age")
  # Age 50
  inSample_actuals50 <- changeToDataframe(inSample_actuals[which(inSample_actuals$`row.names(df)` == 50) , ], "time")
  inSample_actuals50 <- inSample_actuals50[ -1 , ]
  inSample_actuals50$`row.names(df)` <- substr(inSample_actuals50$`row.names(df)`, start = 2, stop = 5)
  inSample_actuals50[ , ] <- apply(inSample_actuals50[ , ], 2,  function(x) as.numeric(as.character(x)))
  # Age 60
  inSample_actuals60 <- changeToDataframe(inSample_actuals[which(inSample_actuals$`row.names(df)` == 60) , ], "time")
  inSample_actuals60 <- inSample_actuals60[ -1 , ]
  inSample_actuals60$`row.names(df)` <- substr(inSample_actuals60$`row.names(df)`, start = 2, stop = 5)
  inSample_actuals60[ , ] <- apply(inSample_actuals60[ , ], 2,  function(x) as.numeric(as.character(x)))
  # Age 70
  inSample_actuals70 <- changeToDataframe(inSample_actuals[which(inSample_actuals$`row.names(df)` == 70) , ], "time")
  inSample_actuals70 <- inSample_actuals70[ -1 , ]
  inSample_actuals70$`row.names(df)` <- substr(inSample_actuals70$`row.names(df)`, start = 2, stop = 5)
  inSample_actuals70[ , ] <- apply(inSample_actuals70[ , ], 2,  function(x) as.numeric(as.character(x)))
  # Age 80
  inSample_actuals80 <- changeToDataframe(inSample_actuals[which(inSample_actuals$`row.names(df)` == 80) , ], "time")
  inSample_actuals80 <- inSample_actuals80[ -1 , ]
  inSample_actuals80$`row.names(df)` <- substr(inSample_actuals80$`row.names(df)`, start = 2, stop = 5)
  inSample_actuals80[ , ] <- apply(inSample_actuals80[ , ], 2,  function(x) as.numeric(as.character(x)))
  # Age 90
  inSample_actuals90 <- changeToDataframe(inSample_actuals[which(inSample_actuals$`row.names(df)` == 90) , ], "time")
  inSample_actuals90 <- inSample_actuals90[ -1 , ]
  inSample_actuals90$`row.names(df)` <- substr(inSample_actuals90$`row.names(df)`, start = 2, stop = 5)
  inSample_actuals90[ , ] <- apply(inSample_actuals90[ , ], 2,  function(x) as.numeric(as.character(x)))
  # Join
  comb <- left_join(inSample_actuals50, inSample_actuals60, by =  "row.names(df)")
  comb <- left_join(comb, inSample_actuals70, by =  "row.names(df)")
  comb <- left_join(comb, inSample_actuals80, by =  "row.names(df)")
  comb <- left_join(comb, inSample_actuals90, by =  "row.names(df)")
  return(comb)
  
}

# Get actuals for the different groups
act_m0 <- getActuals(data_GR0_demog_smoothed$rate$men)
act_m1 <- getActuals(data_GR1_demog_smoothed$rate$men)
act_m2 <- getActuals(data_GR2_demog_smoothed$rate$men)
act_m3 <- getActuals(data_GR3_demog_smoothed$rate$men)
act_m4 <- getActuals(data_GR4_demog_smoothed$rate$men)
act_m5 <- getActuals(data_GR5_demog_smoothed$rate$men)
act_m6 <- getActuals(data_GR6_demog_smoothed$rate$men)
act_m7 <- getActuals(data_GR7_demog_smoothed$rate$men)
act_m8 <- getActuals(data_GR8_demog_smoothed$rate$men)
act_m9 <- getActuals(data_GR9_demog_smoothed$rate$men)
act_w0 <- getActuals(data_GR0_demog_smoothed$rate$women)
act_w1 <- getActuals(data_GR1_demog_smoothed$rate$women)
act_w2 <- getActuals(data_GR2_demog_smoothed$rate$women)
act_w3 <- getActuals(data_GR3_demog_smoothed$rate$women)
act_w4 <- getActuals(data_GR4_demog_smoothed$rate$women)
act_w5 <- getActuals(data_GR5_demog_smoothed$rate$women)
act_w6 <- getActuals(data_GR6_demog_smoothed$rate$women)
act_w7 <- getActuals(data_GR7_demog_smoothed$rate$women)
act_w8 <- getActuals(data_GR8_demog_smoothed$rate$women)
act_w9 <- getActuals(data_GR9_demog_smoothed$rate$women)

combineActualsAndForecasts <- function(df11, df22){
  # Combine datasets
  # df11 <- modelResults_GR0_m
  # df22 <- act_m0
  
  # 60 data 
  df1 <- changeToDataframe(df11$inSample60_data, "age")
  df1[c(1)] <- NULL
  df2 <- changeToDataframe(df22, "age")
  df2[c(1)] <- NULL
  df <- left_join(df1, df2[c("row.names.df.","X60")], by = "row.names.df.")
  colnames(df) <- c("Year", "LC", "RH", "Actuals") # First LC then RH in get insampleModelValues
  df_60 <- df
  # 70 data 
  df1 <- changeToDataframe(df11$inSample70_data, "age")
  df1[c(1)] <- NULL
  df2 <- changeToDataframe(df22, "age")
  df2[c(1)] <- NULL
  df <- left_join(df1, df2[c("row.names.df.","X70")], by = "row.names.df.")
  colnames(df) <- c("Year", "LC", "RH", "Actuals") # First LC then RH in get insampleModelValues
  df_70 <- df
  # 80 data 
  df1 <- changeToDataframe(df11$inSample80_data, "age")
  df1[c(1)] <- NULL
  df2 <- changeToDataframe(df22, "age")
  df2[c(1)] <- NULL
  df <- left_join(df1, df2[c("row.names.df.","X80")], by = "row.names.df.")
  colnames(df) <- c("Year", "LC", "RH", "Actuals") # First LC then RH in get insampleModelValues
  df_80 <- df
  # 90 data 
  df1 <- changeToDataframe(df11$inSample90_data, "age")
  df1[c(1)] <- NULL
  df2 <- changeToDataframe(df22, "age")
  df2[c(1)] <- NULL
  df <- left_join(df1, df2[c("row.names.df.","X90")], by = "row.names.df.")
  colnames(df) <- c("Year", "LC", "RH", "Actuals") # First LC then RH in get insampleModelValues
  df_90 <- df
  
  return_list <- list("df60" = df_60, "df70" = df_70, "df80" = df_80, "df90" = df_90)
  
  return(return_list)
}

insample_m0 <- combineActualsAndForecasts(modelResults_GR0_m, act_m0)
insample_m1 <- combineActualsAndForecasts(modelResults_GR1_m, act_m1)
insample_m2 <- combineActualsAndForecasts(modelResults_GR2_m, act_m2)
insample_m3 <- combineActualsAndForecasts(modelResults_GR3_m, act_m3)
insample_m4 <- combineActualsAndForecasts(modelResults_GR4_m, act_m4)
insample_m5 <- combineActualsAndForecasts(modelResults_GR5_m, act_m5)
insample_m6 <- combineActualsAndForecasts(modelResults_GR6_m, act_m6)
insample_m7 <- combineActualsAndForecasts(modelResults_GR7_m, act_m7)
insample_m8 <- combineActualsAndForecasts(modelResults_GR8_m, act_m8)
insample_m9 <- combineActualsAndForecasts(modelResults_GR9_m, act_m9)
insample_w0 <- combineActualsAndForecasts(modelResults_GR0_w, act_w0)
insample_w1 <- combineActualsAndForecasts(modelResults_GR1_w, act_w1)
insample_w2 <- combineActualsAndForecasts(modelResults_GR2_w, act_w2)
insample_w3 <- combineActualsAndForecasts(modelResults_GR3_w, act_w3)
insample_w4 <- combineActualsAndForecasts(modelResults_GR4_w, act_w4)
insample_w5 <- combineActualsAndForecasts(modelResults_GR5_w, act_w5)
insample_w6 <- combineActualsAndForecasts(modelResults_GR6_w, act_w6)
insample_w7 <- combineActualsAndForecasts(modelResults_GR7_w, act_w7)
insample_w8 <- combineActualsAndForecasts(modelResults_GR8_w, act_w8)
insample_w9 <- combineActualsAndForecasts(modelResults_GR9_w, act_w9)

# Join Bayesian results on
insample_m0$df60$RH2 <- modelResults_GR0_m_rh$inSample60_data[,3]
insample_m0$df70$RH2 <- modelResults_GR0_m_rh$inSample70_data[,3]
insample_m0$df80$RH2 <- modelResults_GR0_m_rh$inSample80_data[,3]
insample_m0$df90$RH2 <- modelResults_GR0_m_rh$inSample90_data[,3]

insample_m1$df60$RH2 <- modelResults_GR1_m_rh$inSample60_data[,3]
insample_m1$df70$RH2 <- modelResults_GR1_m_rh$inSample70_data[,3]
insample_m1$df80$RH2 <- modelResults_GR1_m_rh$inSample80_data[,3]
insample_m1$df90$RH2 <- modelResults_GR1_m_rh$inSample90_data[,3]

insample_m2$df60$RH2 <- modelResults_GR2_m_rh$inSample60_data[,3]
insample_m2$df70$RH2 <- modelResults_GR2_m_rh$inSample70_data[,3]
insample_m2$df80$RH2 <- modelResults_GR2_m_rh$inSample80_data[,3]
insample_m2$df90$RH2 <- modelResults_GR2_m_rh$inSample90_data[,3]

insample_m3$df60$RH2 <- modelResults_GR3_m_rh$inSample60_data[,3]
insample_m3$df70$RH2 <- modelResults_GR3_m_rh$inSample70_data[,3]
insample_m3$df80$RH2 <- modelResults_GR3_m_rh$inSample80_data[,3]
insample_m3$df90$RH2 <- modelResults_GR3_m_rh$inSample90_data[,3]

insample_m4$df60$RH2 <- modelResults_GR4_m_rh$inSample60_data[,3]
insample_m4$df70$RH2 <- modelResults_GR4_m_rh$inSample70_data[,3]
insample_m4$df80$RH2 <- modelResults_GR4_m_rh$inSample80_data[,3]
insample_m4$df90$RH2 <- modelResults_GR4_m_rh$inSample90_data[,3]

insample_m5$df60$RH2 <- modelResults_GR5_m_rh$inSample60_data[,3]
insample_m5$df70$RH2 <- modelResults_GR5_m_rh$inSample70_data[,3]
insample_m5$df80$RH2 <- modelResults_GR5_m_rh$inSample80_data[,3]
insample_m5$df90$RH2 <- modelResults_GR5_m_rh$inSample90_data[,3]

insample_m6$df60$RH2 <- modelResults_GR6_m_rh$inSample60_data[,3]
insample_m6$df70$RH2 <- modelResults_GR6_m_rh$inSample70_data[,3]
insample_m6$df80$RH2 <- modelResults_GR6_m_rh$inSample80_data[,3]
insample_m6$df90$RH2 <- modelResults_GR6_m_rh$inSample90_data[,3]

insample_m7$df60$RH2 <- modelResults_GR7_m_rh$inSample60_data[,3]
insample_m7$df70$RH2 <- modelResults_GR7_m_rh$inSample70_data[,3]
insample_m7$df80$RH2 <- modelResults_GR7_m_rh$inSample80_data[,3]
insample_m7$df90$RH2 <- modelResults_GR7_m_rh$inSample90_data[,3]

insample_m8$df60$RH2 <- modelResults_GR8_m_rh$inSample60_data[,3]
insample_m8$df70$RH2 <- modelResults_GR8_m_rh$inSample70_data[,3]
insample_m8$df80$RH2 <- modelResults_GR8_m_rh$inSample80_data[,3]
insample_m8$df90$RH2 <- modelResults_GR8_m_rh$inSample90_data[,3]

insample_m9$df60$RH2 <- modelResults_GR9_m_rh$inSample60_data[,3]
insample_m9$df70$RH2 <- modelResults_GR9_m_rh$inSample70_data[,3]
insample_m9$df80$RH2 <- modelResults_GR9_m_rh$inSample80_data[,3]
insample_m9$df90$RH2 <- modelResults_GR9_m_rh$inSample90_data[,3]

insample_w0$df60$RH2 <- modelResults_GR0_w_rh$inSample60_data[,3]
insample_w0$df70$RH2 <- modelResults_GR0_w_rh$inSample70_data[,3]
insample_w0$df80$RH2 <- modelResults_GR0_w_rh$inSample80_data[,3]
insample_w0$df90$RH2 <- modelResults_GR0_w_rh$inSample90_data[,3]

insample_w1$df60$RH2 <- modelResults_GR1_w_rh$inSample60_data[,3]
insample_w1$df70$RH2 <- modelResults_GR1_w_rh$inSample70_data[,3]
insample_w1$df80$RH2 <- modelResults_GR1_w_rh$inSample80_data[,3]
insample_w1$df90$RH2 <- modelResults_GR1_w_rh$inSample90_data[,3]

insample_w2$df60$RH2 <- modelResults_GR2_w_rh$inSample60_data[,3]
insample_w2$df70$RH2 <- modelResults_GR2_w_rh$inSample70_data[,3]
insample_w2$df80$RH2 <- modelResults_GR2_w_rh$inSample80_data[,3]
insample_w2$df90$RH2 <- modelResults_GR2_w_rh$inSample90_data[,3]

insample_w3$df60$RH2 <- modelResults_GR3_w_rh$inSample60_data[,3]
insample_w3$df70$RH2 <- modelResults_GR3_w_rh$inSample70_data[,3]
insample_w3$df80$RH2 <- modelResults_GR3_w_rh$inSample80_data[,3]
insample_w3$df90$RH2 <- modelResults_GR3_w_rh$inSample90_data[,3]

insample_w4$df60$RH2 <- modelResults_GR4_w_rh$inSample60_data[,3]
insample_w4$df70$RH2 <- modelResults_GR4_w_rh$inSample70_data[,3]
insample_w4$df80$RH2 <- modelResults_GR4_w_rh$inSample80_data[,3]
insample_w4$df90$RH2 <- modelResults_GR4_w_rh$inSample90_data[,3]

insample_w5$df60$RH2 <- modelResults_GR5_w_rh$inSample60_data[,3]
insample_w5$df70$RH2 <- modelResults_GR5_w_rh$inSample70_data[,3]
insample_w5$df80$RH2 <- modelResults_GR5_w_rh$inSample80_data[,3]
insample_w5$df90$RH2 <- modelResults_GR5_w_rh$inSample90_data[,3]

insample_w6$df60$RH2 <- modelResults_GR6_w_rh$inSample60_data[,3]
insample_w6$df70$RH2 <- modelResults_GR6_w_rh$inSample70_data[,3]
insample_w6$df80$RH2 <- modelResults_GR6_w_rh$inSample80_data[,3]
insample_w6$df90$RH2 <- modelResults_GR6_w_rh$inSample90_data[,3]

insample_w7$df60$RH2 <- modelResults_GR7_w_rh$inSample60_data[,3]
insample_w7$df70$RH2 <- modelResults_GR7_w_rh$inSample70_data[,3]
insample_w7$df80$RH2 <- modelResults_GR7_w_rh$inSample80_data[,3]
insample_w7$df90$RH2 <- modelResults_GR7_w_rh$inSample90_data[,3]

insample_w8$df60$RH2 <- modelResults_GR8_w_rh$inSample60_data[,3]
insample_w8$df70$RH2 <- modelResults_GR8_w_rh$inSample70_data[,3]
insample_w8$df80$RH2 <- modelResults_GR8_w_rh$inSample80_data[,3]
insample_w8$df90$RH2 <- modelResults_GR8_w_rh$inSample90_data[,3]

insample_w9$df60$RH2 <- modelResults_GR9_w_rh$inSample60_data[,3]
insample_w9$df70$RH2 <- modelResults_GR9_w_rh$inSample70_data[,3]
insample_w9$df80$RH2 <- modelResults_GR9_w_rh$inSample80_data[,3]
insample_w9$df90$RH2 <- modelResults_GR9_w_rh$inSample90_data[,3]



# insample_m2$df90$RH2 <- modelResults_GR1_m_rh$inSample90_data
# insample_m3$df80$RH2 <- modelResults_GR1_m_rh$inSample60_data
# insample_m4$df80$RH2 <- modelResults_GR1_m_rh$inSample70_data
# insample_m5$df80$RH2 <- modelResults_GR1_m_rh$inSample80_data
# insample_m6$df80$RH2 <- modelResults_GR1_m_rh$inSample90_data
# insample_m7$df80$RH2 <- modelResults_GR1_m_rh$inSample60_data
# insample_m8$df80$RH2 <- modelResults_GR1_m_rh$inSample70_data
# insample_m9$df80$RH2 <- modelResults_GR1_m_rh$inSample80_data
# insample_w0$df80$RH2 <- modelResults_GR1_m_rh$inSample90_data
# insample_w1$df80$RH2 <- modelResults_GR1_m_rh$inSample60_data
# insample_w2$df90$RH2 <- modelResults_GR1_m_rh$inSample70_data
# insample_w3$df80$RH2 <- modelResults_GR1_m_rh$inSample80_data
# insample_w4$df80$RH2 <- modelResults_GR1_m_rh$inSample90_data
# insample_w5$df80$RH2 <- modelResults_GR1_m_rh$inSample60_data
# insample_w6$df80$RH2 <- modelResults_GR1_m_rh$inSample70_data
# insample_w7$df80$RH2 <- modelResults_GR1_m_rh$inSample80_data
# insample_w8$df80$RH2 <- modelResults_GR1_m_rh$inSample0_data
# insample_w9$df80$RH2 <- modelResults_GR1_m_rh$inSample60_data

# Make plots
plotInSample <- function(df, y_legend,x_legend){
  
  # df <- insample_m1$df60
  # y_legend <- ""
  # x_legend <- "Year"
  
  # Ensure numeric
  df[,1] <- as.numeric(paste(df[,1]))
  colnames(df)[1] <- c("X")
  
  plot_ins <- plot_ly(df, x = ~X) %>%
    add_trace(y = ~LC, name = 'Lee-Carter', mode = 'lines', type = 'scatter', line = list(color = colors[1], width = 2, dash = "line"), showlegend = FALSE) %>%
    add_trace(y = ~RH, name = 'Cohort Ord', mode = 'lines', type = 'scatter', line = list(color = colors[2], width = 2, dash = "line"), showlegend = FALSE) %>%
    add_trace(y = ~RH2, name = 'Cohort Bay', mode = 'lines', type = 'scatter', line = list(color = colors[4], width = 2, dash = "line"), showlegend = FALSE) %>%
    add_trace(y = ~Actuals, name = 'Cohort Bay', mode = 'markers', type = 'scatter', marker = list(color = 'black', symbol = 'cross'), showlegend = FALSE) %>%
    layout(xaxis = x_legend_age <- list(title = x_legend), yaxis = x_legend_age <- list(title = y_legend))
  plot_ins
  return(plot_ins)
}

# KUN KVINDER
ins_w0_60 <- plotInSample(insample_w0$df60, "", "Year")
ins_w0_70 <- plotInSample(insample_w0$df70, "", "Year")
ins_w0_80 <- plotInSample(insample_w0$df80, "", "Year")
ins_w0_90 <- plotInSample(insample_w0$df90, "", "Year")

ins_w1_60 <- plotInSample(insample_w1$df60, "", "Year")
ins_w1_70 <- plotInSample(insample_w1$df70, "", "Year")
ins_w1_80 <- plotInSample(insample_w1$df80, "", "Year")
ins_w1_90 <- plotInSample(insample_w1$df90, "", "Year")

ins_w2_60 <- plotInSample(insample_w2$df60, "", "Year")
ins_w2_70 <- plotInSample(insample_w2$df70, "", "Year")
ins_w2_80 <- plotInSample(insample_w2$df80, "", "Year")
ins_w2_90 <- plotInSample(insample_w2$df90, "", "Year")

ins_w3_60 <- plotInSample(insample_w3$df60, "", "Year")
ins_w3_70 <- plotInSample(insample_w3$df70, "", "Year")
ins_w3_80 <- plotInSample(insample_w3$df80, "", "Year")
ins_w3_90 <- plotInSample(insample_w3$df90, "", "Year")

ins_w4_60 <- plotInSample(insample_w4$df60, "", "Year")
ins_w4_70 <- plotInSample(insample_w4$df70, "", "Year")
ins_w4_80 <- plotInSample(insample_w4$df80, "", "Year")
ins_w4_90 <- plotInSample(insample_w4$df90, "", "Year")

ins_w5_60 <- plotInSample(insample_w5$df60, "", "Year")
ins_w5_70 <- plotInSample(insample_w5$df70, "", "Year")
ins_w5_80 <- plotInSample(insample_w5$df80, "", "Year")
ins_w5_90 <- plotInSample(insample_w5$df90, "", "Year")

ins_w6_60 <- plotInSample(insample_w6$df60, "", "Year")
ins_w6_70 <- plotInSample(insample_w6$df70, "", "Year")
ins_w6_80 <- plotInSample(insample_w6$df80, "", "Year")
ins_w6_90 <- plotInSample(insample_w6$df90, "", "Year")

ins_w7_60 <- plotInSample(insample_w7$df60, "", "Year")
ins_w7_70 <- plotInSample(insample_w7$df70, "", "Year")
ins_w7_80 <- plotInSample(insample_w7$df80, "", "Year")
ins_w7_90 <- plotInSample(insample_w7$df90, "", "Year")

ins_w8_60 <- plotInSample(insample_w8$df60, "", "Year")
ins_w8_70 <- plotInSample(insample_w8$df70, "", "Year")
ins_w8_80 <- plotInSample(insample_w8$df80, "", "Year")
ins_w8_90 <- plotInSample(insample_w8$df90, "", "Year")

ins_w9_60 <- plotInSample(insample_w9$df60, "", "Year")
ins_w9_70 <- plotInSample(insample_w9$df70, "", "Year")
ins_w9_80 <- plotInSample(insample_w9$df80, "", "Year")
ins_w9_90 <- plotInSample(insample_w9$df90, "", "Year")

# Save images - right now only saved fr year 80 AN 70
# plotly_IMAGE(ins_w0_80, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Insample/ins_w1_80.png")
# plotly_IMAGE(ins_w1_80, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Insample/ins_w2_80.png")
# plotly_IMAGE(ins_w2_80, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Insample/ins_w3_80.png")
# plotly_IMAGE(ins_w3_80, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Insample/ins_w4_80.png")
# plotly_IMAGE(ins_w4_80, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Insample/ins_w5_80.png")
# plotly_IMAGE(ins_w5_80, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Insample/ins_w6_80.png")
# plotly_IMAGE(ins_w6_80, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Insample/ins_w7_80.png")
# plotly_IMAGE(ins_w7_80, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Insample/ins_w8_80.png")
# plotly_IMAGE(ins_w8_80, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Insample/ins_w9_80.png")
# plotly_IMAGE(ins_w9_80, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Insample/ins_w10_80.png")
# plotly_IMAGE(ins_w0_70, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Insample/ins_w1_70.png")
# plotly_IMAGE(ins_w1_70, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Insample/ins_w2_70.png")
# plotly_IMAGE(ins_w2_70, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Insample/ins_w3_70.png")
# plotly_IMAGE(ins_w3_70, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Insample/ins_w4_70.png")
# plotly_IMAGE(ins_w4_70, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Insample/ins_w5_70.png")
# plotly_IMAGE(ins_w5_70, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Insample/ins_w6_70.png")
# plotly_IMAGE(ins_w6_70, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Insample/ins_w7_70.png")
# plotly_IMAGE(ins_w7_70, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Insample/ins_w8_70.png")
# plotly_IMAGE(ins_w8_70, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Insample/ins_w9_70.png")
# plotly_IMAGE(ins_w9_70, width = 500, height = 300, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Insample/ins_w10_70.png")


# https://stats.stackexchange.com/questions/107643/how-to-get-the-value-of-mean-squared-error-in-a-linear-regression-in-r
# Evaluate the performance of the different modes
mse <- function(res) {
  mean(res^2)
}

cal_res_models <<- function(df){
  # df <- insample_m0$df60
  diff_lc <- df$LC - df$Actuals
  diff_rh <- df$RH - df$Actuals
  diff_rh2 <- df$RH2 - df$Actuals
  mse_LC <- mse(diff_lc)  
  mse_RH <- mse(diff_rh)
  mse_RH2 <- mse(diff_rh2)
  
  return_list <- list("mse_lc" = mse_LC, "mse_rh" = mse_RH, "mse_rh2" = mse_RH2)
  return(return_list)
}

# Manually calculate all the results
mse_m0_60 <- cal_res_models(insample_m0$df60)
mse_m0_70 <- cal_res_models(insample_m0$df70)
mse_m0_80 <- cal_res_models(insample_m0$df80)
mse_m0_90 <- cal_res_models(insample_m0$df90)
mse_m1_60 <- cal_res_models(insample_m1$df60)
mse_m1_70 <- cal_res_models(insample_m1$df70)
mse_m1_80 <- cal_res_models(insample_m1$df80)
mse_m1_90 <- cal_res_models(insample_m1$df90)
mse_m2_60 <- cal_res_models(insample_m2$df60)
mse_m2_70 <- cal_res_models(insample_m2$df70)
mse_m2_80 <- cal_res_models(insample_m2$df80)
mse_m2_90 <- cal_res_models(insample_m2$df90)
mse_m3_60 <- cal_res_models(insample_m3$df60)
mse_m3_70 <- cal_res_models(insample_m3$df70)
mse_m3_80 <- cal_res_models(insample_m3$df80)
mse_m3_90 <- cal_res_models(insample_m3$df90)
mse_m4_60 <- cal_res_models(insample_m4$df60)
mse_m4_70 <- cal_res_models(insample_m4$df70)
mse_m4_80 <- cal_res_models(insample_m4$df80)
mse_m4_90 <- cal_res_models(insample_m4$df90)
mse_m5_60 <- cal_res_models(insample_m5$df60)
mse_m5_70 <- cal_res_models(insample_m5$df70)
mse_m5_80 <- cal_res_models(insample_m5$df80)
mse_m5_90 <- cal_res_models(insample_m5$df90)
mse_m6_60 <- cal_res_models(insample_m6$df60)
mse_m6_70 <- cal_res_models(insample_m6$df70)
mse_m6_80 <- cal_res_models(insample_m6$df80)
mse_m6_90 <- cal_res_models(insample_m6$df90)
mse_m7_60 <- cal_res_models(insample_m7$df60)
mse_m7_70 <- cal_res_models(insample_m7$df70)
mse_m7_80 <- cal_res_models(insample_m7$df80)
mse_m7_90 <- cal_res_models(insample_m7$df90)
mse_m8_60 <- cal_res_models(insample_m8$df60)
mse_m8_70 <- cal_res_models(insample_m8$df70)
mse_m8_80 <- cal_res_models(insample_m8$df80)
mse_m8_90 <- cal_res_models(insample_m8$df90)
mse_m9_60 <- cal_res_models(insample_m9$df60)
mse_m9_70 <- cal_res_models(insample_m9$df70)
mse_m9_80 <- cal_res_models(insample_m9$df80)
mse_m9_90 <- cal_res_models(insample_m9$df90)

mse_w0_60 <- cal_res_models(insample_w0$df60)
mse_w0_70 <- cal_res_models(insample_w0$df70)
mse_w0_80 <- cal_res_models(insample_w0$df80)
mse_w0_90 <- cal_res_models(insample_w0$df90)
mse_w1_60 <- cal_res_models(insample_w1$df60)
mse_w1_70 <- cal_res_models(insample_w1$df70)
mse_w1_80 <- cal_res_models(insample_w1$df80)
mse_w1_90 <- cal_res_models(insample_w1$df90)
mse_w2_60 <- cal_res_models(insample_w2$df60)
mse_w2_70 <- cal_res_models(insample_w2$df70)
mse_w2_80 <- cal_res_models(insample_w2$df80)
mse_w2_90 <- cal_res_models(insample_w2$df90)
mse_w3_60 <- cal_res_models(insample_w3$df60)
mse_w3_70 <- cal_res_models(insample_w3$df70)
mse_w3_80 <- cal_res_models(insample_w3$df80)
mse_w3_90 <- cal_res_models(insample_w3$df90)
mse_w4_60 <- cal_res_models(insample_w4$df60)
mse_w4_70 <- cal_res_models(insample_w4$df70)
mse_w4_80 <- cal_res_models(insample_w4$df80)
mse_w4_90 <- cal_res_models(insample_w4$df90)
mse_w5_60 <- cal_res_models(insample_w5$df60)
mse_w5_70 <- cal_res_models(insample_w5$df70)
mse_w5_80 <- cal_res_models(insample_w5$df80)
mse_w5_90 <- cal_res_models(insample_w5$df90)
mse_w6_60 <- cal_res_models(insample_w6$df60)
mse_w6_70 <- cal_res_models(insample_w6$df70)
mse_w6_80 <- cal_res_models(insample_w6$df80)
mse_w6_90 <- cal_res_models(insample_w6$df90)
mse_w7_60 <- cal_res_models(insample_w7$df60)
mse_w7_70 <- cal_res_models(insample_w7$df70)
mse_w7_80 <- cal_res_models(insample_w7$df80)
mse_w7_90 <- cal_res_models(insample_w7$df90)
mse_w8_60 <- cal_res_models(insample_w8$df60)
mse_w8_70 <- cal_res_models(insample_w8$df70)
mse_w8_80 <- cal_res_models(insample_w8$df80)
mse_w8_90 <- cal_res_models(insample_w8$df90)
mse_w9_60 <- cal_res_models(insample_w9$df60)
mse_w9_70 <- cal_res_models(insample_w9$df70)
mse_w9_80 <- cal_res_models(insample_w9$df80)
mse_w9_90 <- cal_res_models(insample_w9$df90)

mse_m_a60 <- data.frame(rbind(mse_m0_60, mse_m1_60, mse_m2_60,
                              mse_m3_60, mse_m4_60, mse_m5_60, mse_m6_60, mse_m7_60, mse_m8_60, mse_m9_60))
mse_m_a70 <- data.frame(rbind(mse_m0_70, mse_m1_70, mse_m2_70,
                              mse_m3_70, mse_m4_70, mse_m5_70, mse_m6_70, mse_m7_70, mse_m8_70, mse_m9_70))
mse_m_a80 <- data.frame(rbind(mse_m0_80, mse_m1_80, mse_m2_80,
                              mse_m3_80, mse_m4_80, mse_m5_80, mse_m6_80, mse_m7_80, mse_m8_80, mse_m9_80))
mse_m_a90 <- data.frame(rbind(mse_m0_90, mse_m1_90, mse_m2_90,
                              mse_m3_90, mse_m4_90, mse_m5_90, mse_m6_90, mse_m7_90, mse_m8_90, mse_m9_90))
mse_w_a60 <- data.frame(rbind(mse_w0_60, mse_w1_60, mse_w2_60,
                              mse_w3_60, mse_w4_60, mse_w5_60, mse_w6_60, mse_w7_60, mse_w8_60, mse_w9_60))
mse_w_a70 <- data.frame(rbind(mse_w0_70, mse_w1_70, mse_w2_70,
                              mse_w3_70, mse_w4_70, mse_w5_70, mse_w6_70, mse_w7_70, mse_w8_70, mse_w9_70))
mse_w_a80 <- data.frame(rbind(mse_w0_80, mse_w1_80, mse_w2_80,
                              mse_w3_80, mse_w4_80, mse_w5_80, mse_w6_80, mse_w7_80, mse_w8_80, mse_w9_80))
mse_w_a90 <- data.frame(rbind(mse_w0_90, mse_w1_90, mse_w2_90,
                              mse_w3_90, mse_w4_90, mse_w5_90, mse_w6_90, mse_w7_90, mse_w8_90, mse_w9_90))

plotMSE <- function(df, y_legend,x_legend){
  
  # df <- mse_w_a60
  # y_legend <- ""
  # x_legend <- "Group"
  
  df <- data.frame(t(apply(df, 1, unlist)))
  
  # Number of groups # NOTE GR2 SHOULD BE ADDED
  df$X <- seq(1:10) # skal ændres til 10
  
  df <- data.frame(df)
  s <- seq(1,10)
  
  p <- plot_ly(df, x = ~X) %>%
    add_trace(y =~mse_lc,  marker = list(color = colors[1], width = 1.5), type = 'bar') %>%
    add_trace(y =~mse_rh,  marker = list(color = colors[2], width = 1.5), type = 'bar') %>%
    add_trace(y =~mse_rh2, marker = list(color = colors[4]), type = 'bar') %>%
    #add_trace(y =  ~s, name = "linear") %>%
    layout(yaxis = list(title = ''), barmode = 'group', xaxis = list(title = 'Groups'), showlegend = FALSE)
  # p
  
  return(p)
}

mse_m_60 <- plotMSE(mse_m_a60, "", "Group")
mse_m_70 <- plotMSE(mse_m_a70, "", "Group")
mse_m_80 <- plotMSE(mse_m_a80, "", "Group")
mse_m_90 <- plotMSE(mse_m_a90, "", "Group")
mse_w_60 <- plotMSE(mse_w_a60, "", "Group")
mse_w_70 <- plotMSE(mse_w_a70, "", "Group")
mse_w_80 <- plotMSE(mse_w_a80, "", "Group")
mse_w_90 <- plotMSE(mse_w_a90, "", "Group")

# NB LIGE NU SAMMENLIGNES RH OGSÅ MED SMOOTHED ACTUALS, DETTE ER EGENTLIG UNFAIR! MÅSKE BØR BEGGE TO EGENTLIG VRE MED

plotly_IMAGE(mse_m_60, width = 1500, height = 400, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/MSE/mse_m60.png")
plotly_IMAGE(mse_m_70, width = 1500, height = 400, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/MSE/mse_m70.png")
plotly_IMAGE(mse_m_80, width = 1500, height = 400, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/MSE/mse_m80.png")
plotly_IMAGE(mse_m_90, width = 1500, height = 400, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/MSE/mse_m90.png")
plotly_IMAGE(mse_w_60, width = 1500, height = 400, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/MSE/mse_w60.png")
plotly_IMAGE(mse_w_70, width = 1500, height = 400, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/MSE/mse_w70.png")
plotly_IMAGE(mse_w_80, width = 1500, height = 400, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/MSE/mse_w80.png")
plotly_IMAGE(mse_w_90, width = 1500, height = 400, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/MSE/mse_w90.png")




# FORECASTING ----

# load(file = "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/MortalityModels/ModelResultsLCRH.RData")

calc_LC_levels <- function(df){
  
  # df <- data_GR0_m
  # df <- modelResults_GR0_m
  
  LCfit <- df$LCfit
  
  # LCfit <-  fit(lc(), data = df, ages.fit = ages)
  test <- forecast(LCfit, h = 48, level=c(0.95))
  
  a <- LCfit$ax
  b <- LCfit$bx
  k <- LCfit$kt
  kmean <- test$kt.f$mean
  klow <- test$kt.f$lower
  khigh <- test$kt.f$upper
  
  insample <- (exp(a + b %*% k))
  mean_fore <- (exp(a + b %*% kmean))
  lower_fore <- (exp(a + b %*% klow))
  higher_fore <- (exp(a + b %*% khigh))
  
  # plot(c(insample[1,], lower_fore[1,]))
  
  # Combine into one data frame
  low_60 <- data.frame(matrix(c(insample[which(row.names(insample) == 60 ) ,], lower_fore[which(row.names(lower_fore) == 60 ) ,])))
  mean_60 <- data.frame(matrix(c(insample[which(row.names(insample) == 60 ) ,], mean_fore[which(row.names(mean_fore) == 60 ) ,])))
  high_60 <- data.frame(matrix(c(insample[which(row.names(insample) == 60 ) ,], higher_fore[which(row.names(higher_fore) == 60 ) ,])))
  df_60 <- data.frame(cbind(low_60[,1], mean_60[,1], high_60[,1]))
  df_60$Year <- seq(from=1985, to=2060, by = 1)
  low_70 <- data.frame(matrix(c(insample[which(row.names(insample) == 70 ) ,], lower_fore[which(row.names(lower_fore) == 70 ) ,])))
  mean_70 <- data.frame(matrix(c(insample[which(row.names(insample) == 70 ) ,], mean_fore[which(row.names(mean_fore) == 70 ) ,])))
  high_70 <- data.frame(matrix(c(insample[which(row.names(insample) == 70 ) ,], higher_fore[which(row.names(higher_fore) == 70 ) ,])))
  df_70 <- data.frame(cbind(low_70[,1], mean_70[,1], high_70[,1]))
  df_70$Year <- seq(from=1985, to=2060, by = 1)
  low_80 <- data.frame(matrix(c(insample[which(row.names(insample) == 80 ) ,], lower_fore[which(row.names(lower_fore) == 80 ) ,])))
  mean_80 <- data.frame(matrix(c(insample[which(row.names(insample) == 80 ) ,], mean_fore[which(row.names(mean_fore) == 80 ) ,])))
  high_80 <- data.frame(matrix(c(insample[which(row.names(insample) == 80 ) ,], higher_fore[which(row.names(higher_fore) == 80 ) ,])))
  df_80 <- data.frame(cbind(low_80[,1], mean_80[,1], high_80[,1]))
  df_80$Year <- seq(from=1985, to=2060, by = 1)
  low_90 <- data.frame(matrix(c(insample[which(row.names(insample) == 90 ) ,], lower_fore[which(row.names(lower_fore) == 90 ) ,])))
  mean_90 <- data.frame(matrix(c(insample[which(row.names(insample) == 90 ) ,], mean_fore[which(row.names(mean_fore) == 90 ) ,])))
  high_90 <- data.frame(matrix(c(insample[which(row.names(insample) == 90 ) ,], higher_fore[which(row.names(higher_fore) == 90 ) ,])))
  df_90 <- data.frame(cbind(low_90[,1], mean_90[,1], high_90[,1]))
  df_90$Year <- seq(from=1985, to=2060, by = 1)
  
  return_list <- list("lc_60"= df_60, "lc_70" = df_70, "lc_80" = df_80, "lc_90" = df_90)
  
  return(return_list)
}
get_gmat <- function(g){
  # Reverse matrix
  g <- apply(matrix(g), 2, rev)
  # skal ende som 40 x 36
  res <- c()
  for(i in 0:27){
    # i <- 34
    values <- as.numeric(g[((length(g)-i-39)):(length(g)-i)])
    values
    # SKAL TRANSPONERES?
    res <- c(res, values)
  }
  g_mat <- matrix(res, nrow=40, ncol=28)
  return(g_mat)  
}
calc_RH_levels <- function(df){
  
  # df <- data_GR0_m
  # df <- modelResults_GR0_m
  
  LCfit <- df$RH
  test <- forecast(LCfit, h = 48, level=c(0.95))
  
  a <- LCfit$ax
  b <- LCfit$bx
  k <- LCfit$kt
  bg <- LCfit$b0x
  bg_mat <- matrix(rep(bg, times=28), nrow = 40, ncol = 28)
  g <- LCfit$gc
  g_mat <- get_gmat(LCfit$gc)
  
  kmean <- test$kt.f$mean
  klow <- test$kt.f$lower
  khigh <- test$kt.f$upper
  glow <- test$gc.f$lower
  ghigh <- test$gc.f$upper
  
  res <- b %*% k
  res2 <- bg_mat * g_mat
  
  exp(a + res + res2)
  
  insample <- (exp(a + b %*% k + (bg_mat * g_mat)))
  # # tjek det samme som: 
  forecasts_LC <- forecast::forecast(LCfit)
  inSample_target <- changeToDataframe(forecasts_LC$fitted, "age") # Empirical results on the considered period
  fitted <- test$rates
  fitted <- insample
  mean_fore <- (exp(a + b %*% kmean))
  lower_fore <- (exp(a + b %*% klow))
  higher_fore <- (exp(a + b %*% khigh))
  
  # plot(c(insample[1,], lower_fore[1,]))
  
  # Combine into one data frame
  low_60 <- data.frame(matrix(c(insample[which(row.names(insample) == 60 ) ,], lower_fore[which(row.names(lower_fore) == 60 ) ,])))
  mean_60 <- data.frame(matrix(c(insample[which(row.names(insample) == 60 ) ,], mean_fore[which(row.names(mean_fore) == 60 ) ,])))
  high_60 <- data.frame(matrix(c(insample[which(row.names(insample) == 60 ) ,], higher_fore[which(row.names(higher_fore) == 60 ) ,])))
  df_60 <- data.frame(cbind(low_60[,1], mean_60[,1], high_60[,1]))
  df_60$Year <- seq(from=1985, to=2060, by = 1)
  low_70 <- data.frame(matrix(c(insample[which(row.names(insample) == 70 ) ,], lower_fore[which(row.names(lower_fore) == 70 ) ,])))
  mean_70 <- data.frame(matrix(c(insample[which(row.names(insample) == 70 ) ,], mean_fore[which(row.names(mean_fore) == 70 ) ,])))
  high_70 <- data.frame(matrix(c(insample[which(row.names(insample) == 70 ) ,], higher_fore[which(row.names(higher_fore) == 70 ) ,])))
  df_70 <- data.frame(cbind(low_70[,1], mean_70[,1], high_70[,1]))
  df_70$Year <- seq(from=1985, to=2060, by = 1)
  low_80 <- data.frame(matrix(c(insample[which(row.names(insample) == 80 ) ,], lower_fore[which(row.names(lower_fore) == 80 ) ,])))
  mean_80 <- data.frame(matrix(c(insample[which(row.names(insample) == 80 ) ,], mean_fore[which(row.names(mean_fore) == 80 ) ,])))
  high_80 <- data.frame(matrix(c(insample[which(row.names(insample) == 80 ) ,], higher_fore[which(row.names(higher_fore) == 80 ) ,])))
  df_80 <- data.frame(cbind(low_80[,1], mean_80[,1], high_80[,1]))
  df_80$Year <- seq(from=1985, to=2060, by = 1)
  low_90 <- data.frame(matrix(c(insample[which(row.names(insample) == 90 ) ,], lower_fore[which(row.names(lower_fore) == 90 ) ,])))
  mean_90 <- data.frame(matrix(c(insample[which(row.names(insample) == 90 ) ,], mean_fore[which(row.names(mean_fore) == 90 ) ,])))
  high_90 <- data.frame(matrix(c(insample[which(row.names(insample) == 90 ) ,], higher_fore[which(row.names(higher_fore) == 90 ) ,])))
  df_90 <- data.frame(cbind(low_90[,1], mean_90[,1], high_90[,1]))
  df_90$Year <- seq(from=1985, to=2060, by = 1)
  
  return_list <- list("lc_60"= df_60, "lc_70" = df_70, "lc_80" = df_80, "lc_90" = df_90)
  
  return(return_list)
} 

CL_LC <- calc_LC_levels(modelResults_GR0_m)
CL_RH <- calc_RH_levels(modelResults_GR0_m)
CL_RH2 <- calc_RH_levels(modelResults_GR0_m_rh)


# plot cl ----
colorsFade <- c("rgba(239, 138, 0, 0.15)", "rgba(248, 61, 0, 0.2)", "rgba(153, 0, 102, 0.2)", "rgba(25, 0, 229, 0.2)", "rgba(0, 0, 0, 0.2)") #

# Data set containing all models
df_60 <- cbind(CL_LC$lc_60[,-4], CL_RH$lc_60[,-4], CL_RH2$lc_60)
colnames(df_60) <- c("LC_l", "LC_m", "LC_h", "RH_l", "RH_m", "RH_h", "RH2_l", "RH2_m", "RH2_h", "Year")  

plotForecast <- function(df, y_legend,x_legend){
  
  # df <- df_60
  # y_legend <- ""
  # x_legend <- "Year"
  
  CL_plot <- plot_ly(df, x=~Year) %>%
    add_lines(y = ~LC_m, name = 'Lee-Carter forecast', mode = 'lines', type = 'scatter', line = list(color = colors[1], width = 2, dash = "line"), showlegend = TRUE) %>%
    add_ribbons(name = '95 %', ymin =~ LC_l, ymax =~LC_h,  fillcolor = colorsFade[1]) %>%
    add_lines(y = ~RH_m, name = 'Cohort ord forecast', mode = 'lines', type = 'scatter', line = list(color = colors[2], width = 2, dash = "line"), showlegend = TRUE) %>%
    add_ribbons(name = '95 %', ymin =~ RH_l, ymax =~RH_h,  fillcolor = colorsFade[2]) %>%
    add_lines(y = ~RH2_m, name = 'Cohort Bay forecast', mode = 'lines', type = 'scatter', line = list(color = colors[4], width = 2, dash = "line"), showlegend = TRUE) %>%
    add_ribbons(name = '95 %', ymin =~ RH2_l, ymax =~RH2_h,  fillcolor = colorsFade[4]) %>%
    layout(xaxis = x_legend_age <- list(title = x_legend), yaxis = x_legend_age <- list(title = y_legend))
  # CL_plot
  
  return(CL_plot)
}

CL_plot_df60 <- plotForecast(df_60, "", "Year")
CL_plot_df60

# Bør nok egentlig vre life exp jeg tegner i stedet
# DISSE SER MÆRKELIG UD!
plotly_IMAGE(CL_plot_df60, width = 600, height = 400, format = "png", out_file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Pred/mg1_60.png")






# LIFE EXPECTANCIES ----

#Måske Eriks kode kan bruges, funktionen hedder lieflief
source("C:/Users/Dorthe B/OneDrive/Speciale/R_Code/MortalityModels/MasterThesisMortalityRates/func_lifeexp.R")

# År nedad og tid henad

getLifeExp <- function(df, rh2_df){
  
  # df <- modelResults_GR0_m
  # df2 <- modelResults_GR0_m_rh
  # # MAKE LIFE EXP TABLES!

  # LEE CARTER
  LCfit <- df$LCfit
  forecasts_LC <- forecast::forecast(LCfit)
  inSample_mort_rates <- forecasts_LC$fitted
  additional_years <- lieflief(inSample_mort_rates_lc)
  # Find life expectancy
  ages <- matrix(as.numeric(row.names(inSample_mort_rates)), nrow=nrow(inSample_mort_rates), ncol = ncol(inSample_mort_rates))
  life_exp <- ages + additional_years
  rownames(life_exp) <- row.names(inSample_mort_rates)
  colnames(life_exp) <- colnames(inSample_mort_rates)
  LE_LC <- life_exp
  
  # RENSHAW
  LCfit <- df$RHfit
  forecasts_LC <- forecast::forecast(LCfit)
  inSample_mort_rates <- forecasts_LC$fitted
  additional_years <- lieflief(inSample_mort_rates_rh)
  # Find life expectancy
  ages <- matrix(as.numeric(row.names(inSample_mort_rates)), nrow=nrow(inSample_mort_rates), ncol = ncol(inSample_mort_rates))
  life_exp <- ages + additional_years
  rownames(life_exp) <- row.names(inSample_mort_rates)
  colnames(life_exp) <- colnames(inSample_mort_rates)
  LE_RH <- life_exp
  
  # RENSHAW HABERMAN 2
  LCfit <- df2$RHfit
  forecasts_LC <- forecast::forecast(LCfit)
  inSample_mort_rates <- inSample_mort_rates_lc
  additional_years <- lieflief(inSample_mort_rates_rh2)
  # Find life expectancy
  ages <- matrix(as.numeric(row.names(inSample_mort_rates)), nrow=nrow(inSample_mort_rates), ncol = ncol(inSample_mort_rates))
  life_exp <- ages + additional_years
  rownames(life_exp) <- row.names(inSample_mort_rates)
  colnames(life_exp) <- colnames(inSample_mort_rates)
  LE_RH2 <- life_exp
  
  # RENSHAW HABERMAN 2 : NB DETTE ER MÅSKE LIDT TRICKY!!
  # identical(LE_LC, LE_RH)
  
  
  # Extract relevant values for 1990
  lc_70_90 <- LE_LC[which(row.names(LE_LC) == "70"), (colnames(LE_LC) == "1990")] # 6 = 1990
  rh_70_90 <- LE_RH[which(row.names(LE_RH) == "70"), (colnames(LE_LC) == "1990")]
  rh2_70_90 <- LE_RH2[which(row.names(LE_RH2) == "70"), (colnames(LE_LC) == "1990")]
  lc_80_90 <- LE_LC[which(row.names(LE_LC) == "80"), (colnames(LE_LC) == "1990")]
  rh_80_90 <- LE_RH[which(row.names(LE_RH) == "80"), (colnames(LE_LC) == "1990")]
  rh2_80_90 <- LE_RH2[which(row.names(LE_RH2) == "80"), (colnames(LE_LC) == "1990")]
  # Extract relevant values for 1990
  year <- "2005"
  lc_70_05 <- LE_LC[which(row.names(LE_LC) == "70"), (colnames(LE_LC) == year)] # 6 = 1990
  rh_70_05 <- LE_RH[which(row.names(LE_RH) == "70"), (colnames(LE_LC) == year)]
  rh2_70_05 <- LE_RH2[which(row.names(LE_RH2) == "70"), (colnames(LE_LC) == year)]
  lc_80_05 <- LE_LC[which(row.names(LE_LC) == "80"), (colnames(LE_LC) == year)]
  rh_80_05 <- LE_RH[which(row.names(LE_RH) == "80"), (colnames(LE_LC) == year)]
  rh2_80_05 <- LE_RH2[which(row.names(LE_RH2) == "80"), (colnames(LE_LC) == year)]
  
  
  return_list <- list("l_70_90" = list(lc_70_90, rh_70_90, rh2_70_90), 
                      "l_70_05" = list(lc_70_05, rh_70_05, rh2_70_05), 
                      "l_80_90" = list(lc_80_90, rh_80_90, rh2_80_90), 
                      "l_80_05" = list(lc_80_05, rh_80_05, rh2_80_05))
  
  return(return_list)
}

# MAKE LIFE EXP TABLES!

# Get all the relevant values for all the considered groups
LE_m0 <- getLifeExp(modelResults_GR0_m, modelResults_GR0_m_rh)
LE_w0 <- getLifeExp(modelResults_GR0_w, modelResults_GR0_w_rh)
LE_m1 <- getLifeExp(modelResults_GR1_m, modelResults_GR1_m_rh)
LE_w1 <- getLifeExp(modelResults_GR1_w, modelResults_GR1_w_rh)
LE_m2 <- getLifeExp(modelResults_GR2_m, modelResults_GR2_m_rh)
LE_w2 <- getLifeExp(modelResults_GR2_w, modelResults_GR2_w_rh)
LE_m3 <- getLifeExp(modelResults_GR3_m, modelResults_GR3_m_rh)
LE_w3 <- getLifeExp(modelResults_GR3_w, modelResults_GR3_w_rh)
LE_m4 <- getLifeExp(modelResults_GR4_m, modelResults_GR4_m_rh)
LE_w4 <- getLifeExp(modelResults_GR4_w, modelResults_GR4_w_rh)
LE_m5 <- getLifeExp(modelResults_GR5_m, modelResults_GR5_m_rh)
LE_w5 <- getLifeExp(modelResults_GR5_w, modelResults_GR5_w_rh)
LE_m6 <- getLifeExp(modelResults_GR6_m, modelResults_GR6_m_rh)
LE_w6 <- getLifeExp(modelResults_GR6_w, modelResults_GR6_w_rh)
LE_m7 <- getLifeExp(modelResults_GR7_m, modelResults_GR7_m_rh)
LE_w7 <- getLifeExp(modelResults_GR7_w, modelResults_GR7_w_rh)
LE_m8 <- getLifeExp(modelResults_GR8_m, modelResults_GR8_m_rh)
LE_w8 <- getLifeExp(modelResults_GR8_w, modelResults_GR8_w_rh)
LE_m9 <- getLifeExp(modelResults_GR9_m, modelResults_GR9_m_rh)
LE_w9 <- getLifeExp(modelResults_GR9_w, modelResults_GR9_w_rh)

# Combine the obtained values into one table
# AGE 79, YEAR 1990
LE_70_90 <- c(LE_m0$l_70_90, LE_w0$l_70_90, LE_m1$l_70_90, LE_w1$l_70_90,
              LE_m2$l_70_90, LE_w2$l_70_90, LE_m3$l_70_90, LE_w3$l_70_90,
              LE_m4$l_70_90, LE_w4$l_70_90, LE_m5$l_70_90, LE_w5$l_70_90,
              LE_m6$l_70_90, LE_w6$l_70_90, LE_m7$l_70_90, LE_w7$l_70_90,
              LE_m8$l_70_90, LE_w8$l_70_90, LE_m9$l_70_90, LE_w9$l_70_90)
comb <- matrix(apply(matrix(LE_70_90, ncol = 6, byrow = T ), 1, unlist), ncol = 6, byrow = T)
colnames(comb) <- c("LC ord.", "RH ord.", "RH Bay.", "LC ord.", "RH ord.", "RH Bay.") # Mænd, kvinder
rownames(comb) <- c("Gruppe 1", "Gruppe 2", "Gruppe 3", "Gruppe 4", "Gruppe 5", "Gruppe 6", "Gruppe 7", "Gruppe 8", "Gruppe 9", "Gruppe 10")
# print(xtable::xtable(comb, type = "latex"), file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Tables/LE_70_90.tex")

LE_80_90 <- c(LE_m0$l_80_90, LE_w0$l_80_90, LE_m1$l_80_90, LE_w1$l_80_90,
              LE_m2$l_80_90, LE_w2$l_80_90, LE_m3$l_80_90, LE_w3$l_80_90,
              LE_m4$l_80_90, LE_w4$l_80_90, LE_m5$l_80_90, LE_w5$l_80_90,
              LE_m6$l_80_90, LE_w6$l_80_90, LE_m7$l_80_90, LE_w7$l_80_90,
              LE_m8$l_80_90, LE_w8$l_80_90, LE_m9$l_80_90, LE_w9$l_80_90)
comb <- matrix(apply(matrix(LE_80_90, ncol = 6, byrow = T ), 1, unlist), ncol = 6, byrow = T)
colnames(comb) <- c("LC ord.", "RH ord.", "RH Bay.", "LC ord.", "RH ord.", "RH Bay.") # Mænd, kvinder
rownames(comb) <- c("Gruppe 1", "Gruppe 2", "Gruppe 3", "Gruppe 4", "Gruppe 5", "Gruppe 6", "Gruppe 7", "Gruppe 8", "Gruppe 9", "Gruppe 10")
# print(xtable::xtable(comb, type = "latex"), file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Tables/LE_80_90.tex")

LE_70_05 <- c(LE_m0$l_70_05, LE_w0$l_70_05, LE_m1$l_70_05, LE_w1$l_70_05,
              LE_m2$l_70_05, LE_w2$l_70_05, LE_m3$l_70_05, LE_w3$l_70_05,
              LE_m4$l_70_05, LE_w4$l_70_05, LE_m5$l_70_05, LE_w5$l_70_05,
              LE_m6$l_70_05, LE_w6$l_70_05, LE_m7$l_70_05, LE_w7$l_70_05,
              LE_m8$l_70_05, LE_w8$l_70_05, LE_m9$l_70_05, LE_w9$l_70_05)
comb <- matrix(apply(matrix(LE_70_05, ncol = 6, byrow = T ), 1, unlist), ncol = 6, byrow = T)
colnames(comb) <- c("LC ord.", "RH ord.", "RH Bay.", "LC ord.", "RH ord.", "RH Bay.") # Mænd, kvinder
rownames(comb) <- c("Gruppe 1", "Gruppe 2", "Gruppe 3", "Gruppe 4", "Gruppe 5", "Gruppe 6", "Gruppe 7", "Gruppe 8", "Gruppe 9", "Gruppe 10")
# print(xtable::xtable(comb, type = "latex"), file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Tables/LE_70_05.tex")

LE_80_05 <- c(LE_m0$l_80_05, LE_w0$l_80_05, LE_m1$l_80_05, LE_w1$l_80_05,
              LE_m2$l_80_05, LE_w2$l_80_05, LE_m3$l_80_05, LE_w3$l_80_05,
              LE_m4$l_80_05, LE_w4$l_80_05, LE_m5$l_80_05, LE_w5$l_80_05,
              LE_m6$l_80_05, LE_w6$l_80_05, LE_m7$l_80_05, LE_w7$l_80_05,
              LE_m8$l_80_05, LE_w8$l_80_05, LE_m9$l_80_05, LE_w9$l_80_05)
comb <- matrix(apply(matrix(LE_80_05, ncol = 6, byrow = T ), 1, unlist), ncol = 6, byrow = T)
colnames(comb) <- c("LC ord.", "RH ord.", "RH Bay.", "LC ord.", "RH ord.", "RH Bay.") # Mænd, kvinder
rownames(comb) <- c("Gruppe 1", "Gruppe 2", "Gruppe 3", "Gruppe 4", "Gruppe 5", "Gruppe 6", "Gruppe 7", "Gruppe 8", "Gruppe 9", "Gruppe 10")
# print(xtable::xtable(comb, type = "latex"), file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Tables/LE_80_05.tex")