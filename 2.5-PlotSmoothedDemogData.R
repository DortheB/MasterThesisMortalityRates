
rm(list=ls())

# Turn the demography data objects into smoothed demography data objects
library("demography")
library("dplyr")
library("plotly")

# Load demography data objects:
load(file = "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/MortalityModels/RawDemogData.RData")




# Functions ----
# Function that collects relevant mortality rates
extractRelevantMortalityRatesFromSmoothedData <- function(df, gender){
  
  # df <- data_GR0_demog
  # gender <- "men"
  
  # Consider med
  if(gender == "men"){
    df <- df$men
  }
  if(gender == "women"){
    df <- df$women
  }
  rows.to.keep <- which(rownames(df) %in% 55:94)
  df <- df[rows.to.keep ,]
  age65 <- df[65-54 , ]
  age80 <- df[80-54 , ]
  time1990 <- df[, 1990-1984]
  time2005 <- df[, 2005-1984]
  
  return_list <- list("age65" = age65, "age80" = age80, 
                      "time1990" = time1990, "time2005" = time2005)
  
  return(return_list)
}
# Function that creates the final plots
createPlotsOneGroup <- function(raw_df, gender){
  
  # raw_df <- data_GR0_demog
  # gender <- "men"
  
  # Smooth the data
  data_smoothed1 <- smooth.demogdata(raw_df, method = "spline")$rate
  data_smoothed2 <- smooth.demogdata(raw_df, method = "cspline")$rate
  data_smoothed3 <- smooth.demogdata(raw_df, method = "loess")$rate
  # data_GR0_demog_smoothed4 <- smooth.demogdata(data_GR_total_demog, method = "mspline")
  raw_m <- extractRelevantMortalityRatesFromSmoothedData(raw_df$rate, gender)
  spline_m <- extractRelevantMortalityRatesFromSmoothedData(data_smoothed1, gender)
  cspline_m <- extractRelevantMortalityRatesFromSmoothedData(data_smoothed2, gender)
  loess_m <- extractRelevantMortalityRatesFromSmoothedData(data_smoothed3, gender)
  # Combine data
  age_data <- data.frame(cbind(raw_m$`age65`, spline_m$`age65`, cspline_m$`age65`,loess_m$`age65`,
                               raw_m$`age80`, spline_m$`age80`, cspline_m$`age80`, loess_m$`age80`))
  age_data <- tibble::rownames_to_column(age_data, "Year")
  time_data <- data.frame(cbind(raw_m$`time1990`, spline_m$`time1990`, cspline_m$`time1990`,loess_m$`time1990`, 
                                raw_m$`time2005`, spline_m$`time2005`, cspline_m$`time2005`, loess_m$`time2005`))
  time_data <- tibble::rownames_to_column(time_data, "Age")
  # Make plot
  if(gender == "women"){
    colors1 <- colors3
    colors2 <- colors4
  }
  plot_all_years <- plot_ly(age_data, x = ~Year) %>%
    add_trace(y = ~X1, name = 'Age 65, actuals', mode = 'markers', type = 'scatter', marker = list(color = colors1[1]), showlegend = TRUE) %>%
    add_trace(y = ~X2, name = 'Age 65, spline', mode = 'lines', type = 'scatter', line = list(color = colors1[2], width = 2, dash = "line"), showlegend = TRUE) %>%
    add_trace(y = ~X3, name = 'Age 65, c-spline', mode = 'lines', type = 'scatter', line = list(color = colors1[3], width = 2, dash = "line"), showlegend = T) %>%
    add_trace(y = ~X4, name = 'Age 65, loess', mode = 'lines', type = 'scatter', line = list(color = colors1[4], width = 2, dash = "line"), showlegend = T) %>%
    add_trace(y = ~X5, name = 'Age 80, actuals', mode = 'markers', type = 'scatter', marker = list(color = colors2[1]), showlegend = T) %>%
    add_trace(y = ~X6, name = 'Age 80, spline', mode = 'lines', type = 'scatter', line = list(color = colors2[2], width = 2, dash = "line"), showlegend = T) %>%
    add_trace(y = ~X7, name = 'Age 80, c-spline', mode = 'lines', type = 'scatter', line = list(color = colors2[3], width = 2, dash = "line"), showlegend = T) %>%
    add_trace(y = ~X8, name = 'Age 80, loess', mode = 'lines', type = 'scatter', line = list(color = colors2[4], width = 2, dash = "line"), showlegend = T) %>%
    layout(xaxis = x_legend_time, yaxis = y_log_legend) %>%
    layout(yaxis = list(type = "log"))
  plot_all_years <- plot_all_years %>% plotly::layout(legend = list(orientation = 'h'), autosize = F, width = 1700/2, height = 3500/3)
  
  plot_all_ages <- plot_ly(time_data, x = ~Age) %>%
    add_trace(y = ~X1, name = '1990, actuals', mode = 'markers', type = 'scatter', marker = list(color = colors1[1]), showlegend = TRUE) %>%
    add_trace(y = ~X2, name = '1990, spline', mode = 'lines', type = 'scatter', line = list(color = colors1[2], width = 2, dash = "line"), showlegend = TRUE) %>%
    add_trace(y = ~X3, name = '1990, c-spline', mode = 'lines', type = 'scatter', line = list(color = colors1[3], width = 2, dash = "line"), showlegend = TRUE) %>%
    add_trace(y = ~X4, name = '2005, loess', mode = 'lines', type = 'scatter', line = list(color = colors1[4], width = 2, dash = "line"), showlegend = TRUE) %>%
    add_trace(y = ~X5, name = '2005, actuals', mode = 'markers', type = 'scatter', marker = list(color = colors2[1]), showlegend = TRUE) %>%
    add_trace(y = ~X6, name = '2005, spline', mode = 'lines', type = 'scatter', line = list(color = colors2[2], width = 2, dash = "line"), showlegend = TRUE) %>%
    add_trace(y = ~X7, name = '2005, c-spline', mode = 'lines', type = 'scatter', line = list(color = colors2[3], width = 2, dash = "line"), showlegend = TRUE) %>%
    add_trace(y = ~X8, name = '2005, loess', mode = 'lines', type = 'scatter', line = list(color = colors2[4], width = 2, dash = "line"), showlegend = TRUE) %>%
    layout(xaxis = x_legend_time, yaxis = y_log_legend) %>%
    layout(yaxis = list(type = "log"))
  plot_all_ages <- plot_all_ages %>% plotly::layout(legend = list(orientation = 'h'), autosize = F, width = 1700/2, height = 3500/3)
  
  return_list <- list("plot_years" = plot_all_years, "plot_ages" = plot_all_ages)
  
  return(return_list)
  
}




# General settings ----
colors1 <- c("rgb(0,100,0)", "rgb(202,255,12)", "rgb(110,139,61)", "rgb(69,139,0)") # Grønne
colors2 <- c("rgb(0,0,139)", "rgb(100,149,237)", "rgb(122,197,205)", "rgb(0,255,255)") # Blå
colors3 <- c("rgb(139,0,0)", "rgb(255,51,147)", "rgb(255,64,64)", "rgb(165,42,42)") # Røde 
colors4 <- c("rgb(139,101,9)", "rgb(205,149,12)", "rgb(208,255,51)", "rgb(255,204,51)") # Gule
x_legend_age <- list(title = "Age")
y_legend <- list(title = "Mortality rate")
y_log_legend <- list(title = "Mortality rate (log scale)")
x_legend_time <- list(title = "Year")




# Make plots for the appendix ----
gr0_men <- createPlotsOneGroup(data_GR0_demog, "men")
gr0_women <- createPlotsOneGroup(data_GR0_demog, "women")
gr1_men <- createPlotsOneGroup(data_GR1_demog, "men")
gr1_women <- createPlotsOneGroup(data_GR1_demog, "women")
gr2_men <- createPlotsOneGroup(data_GR2_demog, "men")
gr2_women <- createPlotsOneGroup(data_GR2_demog, "women")
gr3_men <- createPlotsOneGroup(data_GR3_demog, "men")
gr3_women <- createPlotsOneGroup(data_GR3_demog, "women")
gr4_men <- createPlotsOneGroup(data_GR4_demog, "men")
gr4_women <- createPlotsOneGroup(data_GR4_demog, "women")
gr5_men <- createPlotsOneGroup(data_GR5_demog, "men")
gr5_women <- createPlotsOneGroup(data_GR5_demog, "women")
gr6_men <- createPlotsOneGroup(data_GR6_demog, "men")
gr6_women <- createPlotsOneGroup(data_GR6_demog, "women")
gr7_men <- createPlotsOneGroup(data_GR7_demog, "men")
gr7_women <- createPlotsOneGroup(data_GR7_demog, "women")
gr8_men <- createPlotsOneGroup(data_GR8_demog, "men")
gr8_women <- createPlotsOneGroup(data_GR8_demog, "women")
gr9_men <- createPlotsOneGroup(data_GR9_demog, "men")
gr9_women <- createPlotsOneGroup(data_GR9_demog, "women")
# Format plots and save them
p1_1 <- subplot(gr0_men$plot_years, gr0_women$plot_years, nrows = 1, margin = 0.05)
p1_1 <- p1_1 %>% plotly::layout(annotations = list(list(x = 0.25 , y = 1.05, text = "<b>Mortality rates over the years (men)</b>", showarrow = T, xref='paper', yref='paper'),
                                                   list(x = 0.75 , y = 1.05, text = "<b>Mortality rates across years (women)</b>", showarrow = T, xref='paper', yref='paper')))
export(p1_1, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/smoothing_gr1_years.png")
p1_2 <- subplot(gr0_men$plot_ages, gr0_women$plot_ages, nrows = 1, margin = 0.05)
p1_2 <- p1_2 %>% plotly::layout(annotations = list(list(x = 0.25 , y = 1.05, text = "<b>Mortality rates over the ages (men)</b>", showarrow = T, xref='paper', yref='paper'),
                                                   list(x = 0.75 , y = 1.05, text = "<b>Mortality rates across ages (women)</b>", showarrow = T, xref='paper', yref='paper')))
export(p1_2, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/smoothing_gr1_ages.png")

p2_1 <- subplot(gr1_men$plot_years, gr1_women$plot_years, nrows = 1, margin = 0.05)
p2_1 <- p2_1 %>% plotly::layout(annotations = list(list(x = 0.25 , y = 1.05, text = "<b>Mortality rates over the years (men)</b>", showarrow = T, xref='paper', yref='paper'),
                                                   list(x = 0.75 , y = 1.05, text = "<b>Mortality rates across years (women)</b>", showarrow = T, xref='paper', yref='paper')))
export(p2_1, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/smoothing_gr2_years.png")
p2_2 <- subplot(gr1_men$plot_ages, gr1_women$plot_ages, nrows = 1, margin = 0.05)
p2_2 <- p2_2 %>% plotly::layout(annotations = list(list(x = 0.25 , y = 1.05, text = "<b>Mortality rates over the ages (men)</b>", showarrow = T, xref='paper', yref='paper'),
                                                   list(x = 0.75 , y = 1.05, text = "<b>Mortality rates across ages (women)</b>", showarrow = T, xref='paper', yref='paper')))
export(p2_2, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/smoothing_gr2_ages.png")

p3_1 <- subplot(gr2_men$plot_years, gr2_women$plot_years, nrows = 1, margin = 0.05)
p3_1 <- p3_1 %>% plotly::layout(annotations = list(list(x = 0.25 , y = 1.05, text = "<b>Mortality rates over the years (men)</b>", showarrow = T, xref='paper', yref='paper'),
                                                   list(x = 0.75 , y = 1.05, text = "<b>Mortality rates across years (women)</b>", showarrow = T, xref='paper', yref='paper')))
export(p3_1, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/smoothing_gr3_years.png")
p3_2 <- subplot(gr2_men$plot_ages, gr2_women$plot_ages, nrows = 1, margin = 0.05)
p3_2 <- p3_2 %>% plotly::layout(annotations = list(list(x = 0.25 , y = 1.05, text = "<b>Mortality rates over the ages (men)</b>", showarrow = T, xref='paper', yref='paper'),
                                                   list(x = 0.75 , y = 1.05, text = "<b>Mortality rates across ages (women)</b>", showarrow = T, xref='paper', yref='paper')))
export(p3_2, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/smoothing_gr3_ages.png")

p4_1 <- subplot(gr3_men$plot_years, gr3_women$plot_years, nrows = 1, margin = 0.05)
p4_1 <- p4_1 %>% plotly::layout(annotations = list(list(x = 0.25 , y = 1.05, text = "<b>Mortality rates over the years (men)</b>", showarrow = T, xref='paper', yref='paper'),
                                                   list(x = 0.75 , y = 1.05, text = "<b>Mortality rates across years (women)</b>", showarrow = T, xref='paper', yref='paper')))
export(p4_1, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/smoothing_gr4_years.png")
p4_2 <- subplot(gr3_men$plot_ages, gr3_women$plot_ages, nrows = 1, margin = 0.05)
p4_2 <- p4_2 %>% plotly::layout(annotations = list(list(x = 0.25 , y = 1.05, text = "<b>Mortality rates over the ages (men)</b>", showarrow = T, xref='paper', yref='paper'),
                                                   list(x = 0.75 , y = 1.05, text = "<b>Mortality rates across ages (women)</b>", showarrow = T, xref='paper', yref='paper')))
export(p4_2, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/smoothing_gr4_ages.png")

p5_1 <- subplot(gr4_men$plot_years, gr4_women$plot_years, nrows = 1, margin = 0.05)
p5_1 <- p5_1 %>% plotly::layout(annotations = list(list(x = 0.25 , y = 1.05, text = "<b>Mortality rates over the years (men)</b>", showarrow = T, xref='paper', yref='paper'),
                                                   list(x = 0.75 , y = 1.05, text = "<b>Mortality rates across years (women)</b>", showarrow = T, xref='paper', yref='paper')))
export(p5_1, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/smoothing_gr5_years.png")
p5_2 <- subplot(g4_men$plot_ages, gr4_women$plot_ages, nrows = 1, margin = 0.05)
p5_2 <- p5_2 %>% plotly::layout(annotations = list(list(x = 0.25 , y = 1.05, text = "<b>Mortality rates over the ages (men)</b>", showarrow = T, xref='paper', yref='paper'),
                                                   list(x = 0.75 , y = 1.05, text = "<b>Mortality rates across ages (women)</b>", showarrow = T, xref='paper', yref='paper')))
export(p5_2, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/smoothing_gr5_ages.png")

p6_1 <- subplot(gr5_men$plot_years, gr5_women$plot_years, nrows = 1, margin = 0.05)
p6_1 <- p6_1 %>% plotly::layout(annotations = list(list(x = 0.25 , y = 1.05, text = "<b>Mortality rates over the years (men)</b>", showarrow = T, xref='paper', yref='paper'),
                                                   list(x = 0.75 , y = 1.05, text = "<b>Mortality rates across years (women)</b>", showarrow = T, xref='paper', yref='paper')))
export(p6_1, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/smoothing_gr6_years.png")
p6_2 <- subplot(gr5_men$plot_ages, gr5_women$plot_ages, nrows = 1, margin = 0.05)
p6_2 <- p6_2 %>% plotly::layout(annotations = list(list(x = 0.25 , y = 1.05, text = "<b>Mortality rates over the ages (men)</b>", showarrow = T, xref='paper', yref='paper'),
                                                   list(x = 0.75 , y = 1.05, text = "<b>Mortality rates across ages (women)</b>", showarrow = T, xref='paper', yref='paper')))
export(p6_2, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/smoothing_gr6_ages.png")

p7_1 <- subplot(gr6_men$plot_years, gr6_women$plot_years, nrows = 1, margin = 0.05)
p7_1 <- p7_1 %>% plotly::layout(annotations = list(list(x = 0.25 , y = 1.05, text = "<b>Mortality rates over the years (men)</b>", showarrow = T, xref='paper', yref='paper'),
                                                   list(x = 0.75 , y = 1.05, text = "<b>Mortality rates across years (women)</b>", showarrow = T, xref='paper', yref='paper')))
export(p7_1, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/smoothing_gr7_years.png")
p7_2 <- subplot(gr6_men$plot_ages, gr6_women$plot_ages, nrows = 1, margin = 0.05)
p7_2 <- p7_2 %>% plotly::layout(annotations = list(list(x = 0.25 , y = 1.05, text = "<b>Mortality rates over the ages (men)</b>", showarrow = T, xref='paper', yref='paper'),
                                                   list(x = 0.75 , y = 1.05, text = "<b>Mortality rates across ages (women)</b>", showarrow = T, xref='paper', yref='paper')))
export(p7_2, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/smoothing_gr7_ages.png")

p8_1 <- subplot(gr7_men$plot_years, gr7_women$plot_years, nrows = 1, margin = 0.05)
p8_1 <- p8_1 %>% plotly::layout(annotations = list(list(x = 0.25 , y = 1.05, text = "<b>Mortality rates over the years (men)</b>", showarrow = T, xref='paper', yref='paper'),
                                                   list(x = 0.75 , y = 1.05, text = "<b>Mortality rates across years (women)</b>", showarrow = T, xref='paper', yref='paper')))
export(p8_1, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/smoothing_gr8_years.png")
p8_2 <- subplot(gr7_men$plot_ages, gr7_women$plot_ages, nrows = 1, margin = 0.05)
p8_2 <- p8_2 %>% plotly::layout(annotations = list(list(x = 0.25 , y = 1.05, text = "<b>Mortality rates over the ages (men)</b>", showarrow = T, xref='paper', yref='paper'),
                                                   list(x = 0.75 , y = 1.05, text = "<b>Mortality rates across ages (women)</b>", showarrow = T, xref='paper', yref='paper')))
export(p8_2, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/smoothing_gr8_ages.png")

p9_1 <- subplot(gr8_men$plot_years, gr8_women$plot_years, nrows = 1, margin = 0.05)
p9_1 <- p9_1 %>% plotly::layout(annotations = list(list(x = 0.25 , y = 1.05, text = "<b>Mortality rates over the years (men)</b>", showarrow = T, xref='paper', yref='paper'),
                                                   list(x = 0.75 , y = 1.05, text = "<b>Mortality rates across years (women)</b>", showarrow = T, xref='paper', yref='paper')))
export(p9_1, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/smoothing_gr9_years.png")
p9_2 <- subplot(gr8_men$plot_ages, gr8_women$plot_ages, nrows = 1, margin = 0.05)
p9_2 <- p9_2 %>% plotly::layout(annotations = list(list(x = 0.25 , y = 1.05, text = "<b>Mortality rates over the ages (men)</b>", showarrow = T, xref='paper', yref='paper'),
                                                   list(x = 0.75 , y = 1.05, text = "<b>Mortality rates across ages (women)</b>", showarrow = T, xref='paper', yref='paper')))
export(p9_2, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/smoothing_gr9_ages.png")

p10_1 <- subplot(gr9_men$plot_years, gr9_women$plot_years, nrows = 1, margin = 0.05)
p10_1 <- p10_1 %>% plotly::layout(annotations = list(list(x = 0.25 , y = 1.05, text = "<b>Mortality rates over the years (men)</b>", showarrow = T, xref='paper', yref='paper'),
                                                   list(x = 0.75 , y = 1.05, text = "<b>Mortality rates across years (women)</b>", showarrow = T, xref='paper', yref='paper')))
export(p10_1, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/smoothing_gr10_years.png")
p10_2 <- subplot(gr9_men$plot_ages, gr9_women$plot_ages, nrows = 1, margin = 0.05)
p10_2 <- p10_2 %>% plotly::layout(annotations = list(list(x = 0.25 , y = 1.05, text = "<b>Mortality rates over the ages (men)</b>", showarrow = T, xref='paper', yref='paper'),
                                                   list(x = 0.75 , y = 1.05, text = "<b>Mortality rates across ages (women)</b>", showarrow = T, xref='paper', yref='paper')))
export(p10_2, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/smoothing_gr10_ages.png")