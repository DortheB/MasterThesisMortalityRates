# Plots the parameters from LC model and Renshaw Haberman model




# 1: Clean environment and load packages ----

rm(list=ls())

library("StMoMo")
library("plotly")
library("dplyr")
library("plotly")
library("demography")




# Load data and turn into StMoMo data objects ----

load("C:/Users/Dorthe B/OneDrive/Speciale/R_Code/MortalityModels/ModelResultsLCRH.RData")




# Plot parameters and save them ----

colors <- c("rgb(100,149,237)", "rgb(100,149,237)", "rgb(255, 195,15)", "rgb(139,0,0)") # sort, blå, gul, rød
x_legend_age <- list(title = "Age")
y_legend <- list(title = "")
x_legend_time <- list(title = "Year")
x_legend_cohort <- list(title = "Cohort")
empty_plot <- plotly_empty(type = 'scatter', mode = 'markers')

# Function that plot parameters
makeParameterplot <- function(listWithDataResults){
  
  # listWithDataResults <- modelResults_GR0_m
  
  # ALPHA PLOT
  plot_alpha <- plot_ly(listWithDataResults$alpha_data, x = ~Age) %>%
    add_trace(y = ~LC, name = 'Lee-Carter', mode = 'lines', type = 'scatter', line = list(color = colors[2], width = 2, dash = "line"), showlegend = TRUE) %>%
    add_trace(y = ~RH, name = 'Cohort', mode = 'lines', type = 'scatter', line = list(color = colors[3], width = 2, dash = "line"), showlegend = TRUE)  %>%
    layout(xaxis = x_legend_age, yaxis = y_legend)
  
  # BETA PLOT
  plot_beta <- plot_ly(listWithDataResults$beta_data, x = ~Age) %>%
    add_trace(y = ~LC, name = 'Lee-Carter', mode = 'lines', type = 'scatter', line = list(color = colors[2], width = 2, dash = "line"), showlegend = FALSE) %>%
    add_trace(y = ~RH, name = 'Cohort', mode = 'lines', type = 'scatter', line = list(color = colors[3], width = 2, dash = "line"), showlegend = FALSE)  %>%
    layout(xaxis = x_legend_age, yaxis = y_legend)
  
  # KAPPA PLOT
  plot_kappa <- plot_ly(listWithDataResults$kappa_data, x = ~Age) %>%
    add_trace(y = ~LC, name = 'Lee-Carter', mode = 'lines', type = 'scatter', line = list(color = colors[2], width = 2, dash = "line"), showlegend = FALSE) %>%
    add_trace(y = ~RH, name = 'Cohort', mode = 'lines', type = 'scatter', line = list(color = colors[3], width = 2, dash = "line"), showlegend = FALSE)  %>%
    layout(xaxis = x_legend_time, yaxis = y_legend)
  
  # BETA GAMMA PLOT
  plot_beta_gamma <- plot_ly(listWithDataResults$beta_gamma_data, x = ~Age) %>%
    add_trace(y = ~RH, name = 'Cohort', mode = 'lines', type = 'scatter', line = list(color = colors[3], width = 2, dash = "line"), showlegend = FALSE)  %>%
    layout(xaxis = x_legend_time, yaxis = y_legend)
  
  # GAMMA PLOT
  plot_gamma <- plot_ly(listWithDataResults$gamma_data, x = ~Cohort) %>%
    add_trace(y = ~RH, name = 'Cohort', mode = 'lines', type = 'scatter', line = list(color = colors[3], width = 2, dash = "line"), showlegend = FALSE)  %>%
    layout(xaxis = x_legend_cohort, yaxis = y_legend)
  
  # Combine into one plot
  parameter_plot1 <- subplot(plot_alpha, empty_plot, nrows = 1, margin = 0.05)
  parameter_plot1 <- parameter_plot1 %>% plotly::layout(annotations = list(list(x = 0.05 , y = 1.05, text = "<b>Alpha</b>", showarrow = F, xref='paper', yref='paper'),
                                                                           list(x = 0.95 , y = 1.05, text = "<b> </b>", showarrow = F, xref='paper', yref='paper')))
  parameter_plot2 <- subplot(plot_beta, plot_kappa, nrows = 1, margin = 0.05)
  parameter_plot2 <- parameter_plot2 %>% plotly::layout(annotations = list(list(x = 0.05 , y = 1.05, text = "<b>Beta</b>", showarrow = F, xref='paper', yref='paper'),
                                                                           list(x = 0.95 , y = 1.05, text = "<b>Kappa</b>", showarrow = F, xref='paper', yref='paper')))
  plot_gamma <- plot_gamma %>% plotly::layout(legend = list(orientation = 'h'), autosize = F, width = 1700/2, height = 3500/3)
  parameter_plot3 <- subplot(plot_beta_gamma, plot_gamma, nrows = 1, margin = 0.05)
  parameter_plot3 <- parameter_plot3 %>% plotly::layout(annotations = list(list(x = 0.05 , y = 1.05, text = "<b>BetaGamma</b>", showarrow = F, xref='paper', yref='paper'),
                                                                           list(x = 0.95 , y = 1.05, text = "<b> Gamma </b>", showarrow = F, xref='paper', yref='paper')))
  # PROBLEM MED, AT DE IKKE KAN INDEHOLDE GRÆSKE BOGSTAVER - OVERVEJ DERFOR, OM DET GENERELT ER SMARTERE AT LAVE OVERSKRIFTER I LATES
  parameter_plot <- subplot(parameter_plot1, parameter_plot2, parameter_plot3, nrows = 3, margin = 0.05)
  
  return(parameter_plot)
}

parplot_GR0_m <- makeParameterplot(listWithDataResults = modelResults_GR0_m)
export(parplot_GR0_m, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/par_GR0_m.png")
parplot_GR0_w <- makeParameterplot(listWithDataResults = modelResults_GR0_w)
export(parplot_GR0_w, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/par_GR0_w.png")
parplot_GR1_m <- makeParameterplot(listWithDataResults = modelResults_GR1_m)
export(parplot_GR1_m, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/par_GR1_m.png")
parplot_GR1_w <- makeParameterplot(listWithDataResults = modelResults_GR1_w)
export(parplot_GR1_w, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/Parameters/par_GR1_w.png")
