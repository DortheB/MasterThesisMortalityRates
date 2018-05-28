# This file plots actual mortality rates based on the actual population observations




# Clean environment and load relevant packages -----

rm(list=ls()) 

library(forecast)
library(demography)
library(calibrate)
library(StMoMo)
library(reshape)
library(plotly)
library(dplyr)




# Load Malenes mortality data grouped ----
load(file="C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/Agg_data.Rdata")

mortality_M0 <- calculateMortality(death_df = death_list_int$deaths_M0, exp_df = exp_list_int$exp_M0, logIsTaken = FALSE)
mortality_M1 <- calculateMortality(death_df = death_list_int$deaths_M1, exp_df = exp_list_int$exp_M1, logIsTaken = FALSE)
mortality_M2 <- calculateMortality(death_df = death_list_int$deaths_M2, exp_df = exp_list_int$exp_M2, logIsTaken = FALSE)
mortality_M3 <- calculateMortality(death_df = death_list_int$deaths_M3, exp_df = exp_list_int$exp_M3, logIsTaken = FALSE) 
mortality_M4 <- calculateMortality(death_df = death_list_int$deaths_M4, exp_df = exp_list_int$exp_M4, logIsTaken = FALSE)
mortality_M5 <- calculateMortality(death_df = death_list_int$deaths_M5, exp_df = exp_list_int$exp_M5, logIsTaken = FALSE)
mortality_M6 <- calculateMortality(death_df = death_list_int$deaths_M6, exp_df = exp_list_int$exp_M6, logIsTaken = FALSE)
mortality_M7 <- calculateMortality(death_df = death_list_int$deaths_M7, exp_df = exp_list_int$exp_M7, logIsTaken = FALSE)
mortality_M8 <- calculateMortality(death_df = death_list_int$deaths_M8, exp_df = exp_list_int$exp_M8, logIsTaken = FALSE)
mortality_M9 <- calculateMortality(death_df = death_list_int$deaths_M9, exp_df = exp_list_int$exp_M9, logIsTaken = FALSE)
mortality_M_total <- calculateMortality(death_df = death_list_int$deaths_M_total, exp_df = exp_list_int$exp_M_total, logIsTaken = FALSE)

mortality_list_int <- list("mortality_M0" = mortality_M0, "mortality_M1" = mortality_M1, "mortality_M2" = mortality_M2,
                           "mortality_M3" = mortality_M3, "mortality_M4" = mortality_M4, "mortality_M5" = mortality_M5, 
                           "mortality_M6" = mortality_M6, "mortality_M7" = mortality_M7, "mortality_M8" = mortality_M8, 
                           "mortality_M9" = mortality_M9, "mortality_M_total" = mortality_M_total)

# Combine socio-economic groups
combineSocioEconomicGroups <- function(mortality_dfs, group_name, gender){
  # mortality_dfs <- mortality_list_int
  # group_name <- "x88.95"
  # gender <- "Men"
  
  if(gender == "Men"){
    df <- left_join(mortality_dfs[[1]]$Men[c("Year", group_name)], mortality_dfs[[2]]$Men[c("Year", group_name)], by = "Year")
    colnames(df) <- c("Year", "Group 1", "Group 2")
    for(i in 3:length(mortality_dfs)){
      df <- left_join(df, mortality_dfs[[i]]$Men[c("Year", group_name)], by = "Year")
      colnames(df)[i+1] <- paste("Group ", i, sep = "")
    }
    colnames(df)[ncol(df)] <- "Total"
  } else if (gender == "Women"){
    df <- left_join(mortality_dfs[[1]]$Women[c("Year", group_name)], mortality_dfs[[2]]$Women[c("Year", group_name)], by = "Year")
    colnames(df) <- c("Year", "Group 1", "Group 2")
    for(i in 3:length(mortality_dfs)){
      df <- left_join(df, mortality_dfs[[i]]$Women[c("Year", group_name)], by = "Year")
      colnames(df)[i+1] <- paste("Group ", i, sep = "")
    }
    colnames(df)[ncol(df)] <- "Total"
  }


  return(df)  
}

df_m_a1 <- combineSocioEconomicGroups(mortality_dfs = mortality_list_int, group_name = "X50.57", gender = "Men")
df_m_a2 <- combineSocioEconomicGroups(mortality_dfs = mortality_list_int, group_name = "X58.65", gender = "Men")
df_m_a3 <- combineSocioEconomicGroups(mortality_dfs = mortality_list_int, group_name = "X66.72", gender = "Men")
df_m_a4 <- combineSocioEconomicGroups(mortality_dfs = mortality_list_int, group_name = "X73.79", gender = "Men")
df_m_a5 <- combineSocioEconomicGroups(mortality_dfs = mortality_list_int, group_name = "X80.87", gender = "Men")
df_m_a6 <- combineSocioEconomicGroups(mortality_dfs = mortality_list_int, group_name = "X88.95", gender = "Men")

df_w_a1 <- combineSocioEconomicGroups(mortality_dfs = mortality_list_int, group_name = "X50.57", gender = "Women")
df_w_a2 <- combineSocioEconomicGroups(mortality_dfs = mortality_list_int, group_name = "X58.65", gender = "Women")
df_w_a3 <- combineSocioEconomicGroups(mortality_dfs = mortality_list_int, group_name = "X66.72", gender = "Women")
df_w_a4 <- combineSocioEconomicGroups(mortality_dfs = mortality_list_int, group_name = "X73.79", gender = "Women")
df_w_a5 <- combineSocioEconomicGroups(mortality_dfs = mortality_list_int, group_name = "X80.87", gender = "Women")
df_w_a6 <- combineSocioEconomicGroups(mortality_dfs = mortality_list_int, group_name = "X88.95", gender = "Women")


# Plot mortality rates ----
df_m_a1
# Function that plots mortality rate tables
plot_mortality_rates <- function(mortality_df, header, show_legend){

  # mortality_df <- df_m50.59
  # header <- "Mortality rates for 50-59 aged men across time"
  # show_legend <- FALSE

  dashes <- c("line", "dot", "dash", "line", "dot", "dash", "line", "dot", "dash", "line", "dot", "dash")
  colors <- c("rgb(239, 138, 0)", "rgb(239, 138, 0)", "rgb(248, 61, 0)", "rgb(255, 0, 0)", "rgb(204, 0, 51)", "rgb(153, 0, 102)", 
              "rgb(101, 0, 153)", "rgb(76, 0, 178)", "rgb(25, 0, 229)",  "rgb(0, 0, 255)", "rgb(0, 0, 208)") # "rgb(25, 0, 125)"

  numberGroups <- ncol(mortality_df)-1

  # Make men plots
  plot <- plot_ly(data = mortality_df, x=~Year, mode = 'lines')
  # Add for each age category
  for(i in 2:(numberGroups)){
    # color <- getColor(i)
    plot <- add_trace(plot, y=mortality_df[[c(i)]], name = colnames(mortality_df)[i], mode = 'lines', 
                      type = 'scatter', 
                      line = list(color = colors[i], width = 2, dash = dashes[i]),
                      showlegend = show_legend)
    if(show_legend){
      plot <- plot %>% plotly::layout(legend = list(orientation = 'h'), autosize = F, width = 1700/2, height = 3500/3)
    }
    # line = list(color = "rgb(255,140,0)", width = 4, dash = 'dot'
  }
  # Add total
  plot <- add_trace(plot, y=mortality_df[[c(ncol(mortality_df))]], name = colnames(mortality_df)[ncol(mortality_df)], mode = 'lines', 
                    type = 'scatter', 
                    line = list(color = "rgb(0, 0, 0)", width = 3, dash = "dash"),
                    showlegend = show_legend)
  
  # Add headers and legends to the graph
  #plot <- layout(plot, title = header, xaxis = list(title = "Year"), yaxis = list(title = "Log mortality rate"))
  plot

  return(plot)
}

plot_m_a1 <- plot_mortality_rates(mortality_df = df_m_a1, header = "Mortality rates for 50-59 aged men across time", show_legend = TRUE)
plot_m_a2 <- plot_mortality_rates(mortality_df = df_m_a2, header = "Mortality rates for 60-69 aged men across time", show_legend = FALSE)
plot_m_a3 <- plot_mortality_rates(mortality_df = df_m_a3, header ="Mortality rates for 70-79 aged men across time", show_legend = FALSE)
plot_m_a4 <- plot_mortality_rates(mortality_df = df_m_a4, header = "Mortality rates for 80-89 aged men across time", show_legend = TRUE)
plot_m_a5 <- plot_mortality_rates(mortality_df = df_m_a5, header = "Mortality rates for 90-95 aged men across time", show_legend = FALSE)
plot_m_a6 <- plot_mortality_rates(mortality_df = df_m_a6, header = "Mortality rates for 90-95 aged men across time", show_legend = TRUE)

plot_w_a1 <- plot_mortality_rates(mortality_df = df_w_a1, header = "Mortality rates for 50-59 aged women across time", show_legend = FALSE)
plot_w_a2 <- plot_mortality_rates(mortality_df = df_w_a2, header = "Mortality rates for 60-69 aged women across time", show_legend = FALSE)
plot_w_a3 <- plot_mortality_rates(mortality_df = df_w_a3, header ="Mortality rates for 70-79 aged women across time", show_legend = FALSE)
plot_w_a4 <- plot_mortality_rates(mortality_df = df_w_a4, header = "Mortality rates for 80-89 aged women across time", show_legend = FALSE)
plot_w_a5 <- plot_mortality_rates(mortality_df = df_w_a5, header = "Mortality rates for 90-95 aged women across time", show_legend = FALSE)
plot_w_a6 <- plot_mortality_rates(mortality_df = df_w_a6, header = "Mortality rates for 90-95 aged women across time", show_legend = TRUE)

rm(list=setdiff(ls(), c("plot_m_a1", "plot_m_a2", "plot_m_a3", "plot_m_a4", "plot_m_a5", "plot_m_a6", 
                        "plot_w_a1", "plot_w_a2", "plot_w_a3", "plot_w_a4", "plot_w_a5", "plot_w_a6")))




# New plots:
p1 <- subplot(plot_m_a3, plot_m_a4, nrows = 1, margin = 0.05)
p1 <- p1 %>% plotly::layout(annotations = list(list(x = 0.05 , y = 1.05, text = "<b>Mortality rates for 66-72 aged men</b>", showarrow = F, xref='paper', yref='paper'),
                                               list(x = 0.95 , y = 1.05, text = "<b>Mortality rates for 73-79 aged men</b>", showarrow = F, xref='paper', yref='paper')))
p2 <- subplot(plot_w_a3, plot_w_a4, nrows = 1, margin = 0.05)
p2 <- p2 %>% plotly::layout(annotations = list(list(x = 0.05 , y = 1.05, text = "<b>Mortality rates for 66-72 aged women</b>", showarrow = F, xref='paper', yref='paper'),
                                               list(x = 0.95 , y = 1.05, text = "<b>Mortality rates for 73-79 aged women</b>", showarrow = F, xref='paper', yref='paper')))
plot_mortality_new <- subplot(p1, p2, nrows = 2, margin = 0.05)
plot_mortality_new

export(plot_mortality_new, file = "C:/Users/Dorthe B/OneDrive/Speciale/SpecialeLatex/Figures/mortality_new_plot.png")


# Combine plots and save them ----

p1 <- subplot(plot_m_a1, plot_m_a2, nrows = 1, margin = 0.05)
p1 <- p1 %>% plotly::layout(annotations = list(list(x = 0.05 , y = 1.05, text = "<b>Mortality rates for 50-57 aged men</b>", showarrow = F, xref='paper', yref='paper'),
                                             list(x = 0.95 , y = 1.05, text = "<b>Mortality rates for 58-65 aged men</b>", showarrow = F, xref='paper', yref='paper')))
p2 <- subplot(plot_m_a3, plot_m_a4, nrows = 1, margin = 0.05)
p2 <- p2 %>% plotly::layout(annotations = list(list(x = 0.05 , y = 1.05, text = "<b>Mortality rates for 66-72 aged men</b>", showarrow = F, xref='paper', yref='paper'),
                                               list(x = 0.95 , y = 1.05, text = "<b>Mortality rates for 73-79 aged men</b>", showarrow = F, xref='paper', yref='paper')))
p3 <- subplot(plot_m_a5, plot_m_a6, nrows = 1, margin = 0.05)
p3 <- p3 %>% plotly::layout(annotations = list(list(x = 0.05 , y = 1.05, text = "<b>Mortality rates for 80-87 aged men</b>", showarrow = F, xref='paper', yref='paper'),
                                               list(x = 0.95 , y = 1.05, text = "<b>Mortality rates for 88-95 aged men</b>", showarrow = F, xref='paper', yref='paper')))
plot_mortality_m <- subplot(p1, p2, p3, nrows = 3, margin = 0.05)
export(plot_mortality_m, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataPlots/mortality_men.png")

p1 <- subplot(plot_w_a1, plot_w_a2, nrows = 1, margin = 0.05)
p1 <- p1 %>% plotly::layout(annotations = list(list(x = 0.05 , y = 1.05, text = "<b>Mortality rates for 50-57 aged women</b>", showarrow = F, xref='paper', yref='paper'),
                                               list(x = 0.95 , y = 1.05, text = "<b>Mortality rates for 58-65 aged women</b>", showarrow = F, xref='paper', yref='paper')))
p2 <- subplot(plot_w_a3, plot_w_a4, nrows = 1, margin = 0.05)
p2 <- p2 %>% plotly::layout(annotations = list(list(x = 0.05 , y = 1.05, text = "<b>Mortality rates for 66-72 aged women</b>", showarrow = F, xref='paper', yref='paper'),
                                               list(x = 0.95 , y = 1.05, text = "<b>Mortality rates for 73-79 aged women</b>", showarrow = F, xref='paper', yref='paper')))
p3 <- subplot(plot_w_a5, plot_w_a6, nrows = 1, margin = 0.05)
p3 <- p3 %>% plotly::layout(annotations = list(list(x = 0.05 , y = 1.05, text = "<b>Mortality rates for 80-87 aged women</b>", showarrow = F, xref='paper', yref='paper'),
                                               list(x = 0.95 , y = 1.05, text = "<b>Mortality rates for 88-95 aged women</b>", showarrow = F, xref='paper', yref='paper')))
plot_mortality_w <- subplot(p1, p2, p3, nrows = 3, margin = 0.05)
export(plot_mortality_w, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataPlots/mortality_women.png")


# library("RSelenium")
# plot_mortality_m %>% export(file = "men_data.svg", selenium = RSelenium::rsDriver(browser = "chrome"))

# export(plot_mortality_m, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/DataPlots_images/mortality_8089.png")
# 
# 
# # 
# shouldSaveImages <- TRUE
# if(shouldSaveImages){
#   plot_5059 <- subplot(plot_m50.59, plot_w50.59, margin = 0.05, nrows =1) 
#   plot_5059 <- plot_5059 %>% plotly::layout(annotations = list(list(x = 0.05 , y = 1.05, text = "<b>Mortality rates for 50-59 aged men</b>", showarrow = F, xref='paper', yref='paper'),
#                                                                list(x = 0.95 , y = 1.05, text = "<b>Mortality rates for 50-59 aged women</b>", showarrow = F, xref='paper', yref='paper')))
#   export(plot_5059, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/DataPlots_images/mortality_5059.png")
#   
#   plot_6069 <- subplot(plot_m60.69, plot_w60.69, margin = 0.05) 
#   plot_6069 <- plot_6069 %>% plotly::layout(annotations = list(
#     list(x = 0.05 , y = 1.05, text = "<b>Mortality rates for 60-69 aged men</b>", showarrow = F, xref='paper', yref='paper'),
#     list(x = 0.95 , y = 1.05, text = "<b>Mortality rates for 60-69 aged women</b>", showarrow = F, xref='paper', yref='paper')))
#   export(plot_6069, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/DataPlots_images/mortality_6069.png")
#   
#   plot_7079 <- subplot(plot_m70.79, plot_w70.79, margin = 0.05) 
#   plot_7079 <- plot_7079 %>% plotly::layout(annotations = list(
#     list(x = 0.05 , y = 1.05, text = "<b>Mortality rates for 70-79 aged men</b>", showarrow = F, xref='paper', yref='paper'),
#     list(x = 0.95 , y = 1.05, text = "<b>Mortality rates for 70-79 aged women</b>", showarrow = F, xref='paper', yref='paper')))
#   export(plot_7079, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/DataPlots_images/mortality_7079.png")
#   
#   plot_8089 <- subplot(plot_m80.89, plot_w80.89, margin = 0.05) 
#   plot_8089 <- plot_8089 %>% plotly::layout(annotations = list(
#     list(x = 0.05 , y = 1.05, text = "<b>Mortality rates for 80-89 aged men</b>", showarrow = F, xref='paper', yref='paper'),
#     list(x = 0.95 , y = 1.05, text = "<b>Mortality rates for 80-89 aged women</b>", showarrow = F, xref='paper', yref='paper')))
#   export(plot_8089, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/DataPlots_images/mortality_8089.png")
#   
#   
#   plot_8995 <- subplot(plot_m89.95, plot_w89.95, margin = 0.05) 
#   plot_8995 <- plot_8995 %>% plotly::layout(annotations = list(
#     list(x = 0.05 , y = 1.05, text = "<b>Mortality rates for 89-95 aged men</b>", showarrow = F, xref='paper', yref='paper'),
#     list(x = 0.95 , y = 1.05, text = "<b>Mortality rates for 89-95 aged women</b>", showarrow = F, xref='paper', yref='paper')))
#   export(plot_8995, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/DataPlots_images/mortality_8995.png")
#   
# }



# p <- subplot(plot_m50.59, plot_w50.59, margin = 0.05, nrows = 1) 
# p
# p %>% plotly::layout(annotations = list(
#   list(x = 0.05 , y = 1.05, text = "<b>Mortality rates for 50-59 aged men</b>", showarrow = F, xref='paper', yref='paper'),
#   list(x = 0.95 , y = 1.05, text = "<b>Mortality rates for 50-59 aged women</b>", showarrow = F, xref='paper', yref='paper'))
# )



png(file="mygraphic.png",width=210,height=290)
plot_mortality_m
dev.off()



library("RSelenium")
plot_mortality_m %>% export(file = "men_data.svg", selenium = RSelenium::rsDriver(browser = "chrome"))


m <- list(
  l = 50,
  r = 50,
  b = 100,
  t = 100,
  pad = 4
)


library("dplyr")
p <- plot_mortality_m %>% layout(autosize = F, width = 210, height = 290, margin = m)


https://stackoverflow.com/questions/43355444/how-can-i-save-plotly-graphs-in-high-quality



export_plotly2SVG(plotly_graph = plot_mortality_m,
                              filename = NULL,
                              parent_path = paste0(getwd(), "/output/"),
                              width = 2100,
                              height = 3000,
                              remove_title = FALSE,
                              font_family = "Work Sans",
                              incl_PDF_copy = FALSE,
                              incl_PNG_copy = FALSE,
                              png_scaling_factor = 1.8,
                              autocrop_png = TRUE)


# title='<b>Bold</b> <i>animals</i>
# 
# 
# https://rpubs.com/bcd/subplot-titles
# 
# 
# subplot(plot_m50.59, plot_m60.69, nrows = 1)
# 
# https://stackoverflow.com/questions/37285729/how-to-give-subtitles-for-subplot-in-plot-ly-using-r
# 
# https://plot.ly/r/subplots/

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#

# Funtion that determines the color of the plots
getColor <- function(number){

  number <- 10

  Colors<-colorRampPalette(c("black","black"))
  Colors1<-colorRampPalette(c("red","orange2"))

  specific_line_color <-  Colors(11)[number]
  specific_line_color1 <-  Colors1(11)[number]

  specific_line_color <- paste("rgb(", col2rgb(specific_line_color)[1], ", ", col2rgb(specific_line_color)[2], ", ", col2rgb(specific_line_color)[3], ")", sep = "")
  specific_line_color
  specific_line_color1 <- paste("rgb(", col2rgb(specific_line_color1)[1], ", ", col2rgb(specific_line_color1)[2], ", ", col2rgb(specific_line_color1)[3], ")", sep = "")

  # Turn into a list (the used format in the plot function)
  # specific_line_color <- list(color = specific_line_color)

  return(specific_line_color)
}








# FIND LIGE UD AF, OM DER SKAL TAGES LOGARITME ELLER EJ!
