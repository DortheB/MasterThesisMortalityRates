# This file plots actual mortality rates based on the actual population observations




# Clean environment and load relevant packages -----

rm(list=ls()) 

library(forecast)
library(demography)
library(calibrate)
library(StMoMo)
library(reshape2)
library(plotly)
library(dplyr)




# Load Malenes mortality data ----
deaths_M0 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Men/Deaths/Deaths0.txt", header = TRUE, skip = 0)
deaths_M1 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Men/Deaths/Deaths1.txt", header = TRUE, skip = 0)
deaths_M2 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Men/Deaths/Deaths2.txt", header = TRUE, skip = 0)
deaths_M3 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Men/Deaths/Deaths3.txt", header = TRUE, skip = 0)
deaths_M4 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Men/Deaths/Deaths4.txt", header = TRUE, skip = 0)
deaths_M5 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Men/Deaths/Deaths5.txt", header = TRUE, skip = 0)
deaths_M6 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Men/Deaths/Deaths6.txt", header = TRUE, skip = 0)
deaths_M7 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Men/Deaths/Deaths7.txt", header = TRUE, skip = 0)
deaths_M8 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Men/Deaths/Deaths8.txt", header = TRUE, skip = 0)
deaths_M9 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Men/Deaths/Deaths9.txt", header = TRUE, skip = 0)
deaths_M_total <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Men/Deaths/DeathsX.txt", header = TRUE, skip = 0)
deaths_W0 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Women/Deaths/Deaths0.txt", header = TRUE, skip = 0)
deaths_W1 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Women/Deaths/Deaths1.txt", header = TRUE, skip = 0)
deaths_W2 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Women/Deaths/Deaths2.txt", header = TRUE, skip = 0)
deaths_W3 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Women/Deaths/Deaths3.txt", header = TRUE, skip = 0)
deaths_W4 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Women/Deaths/Deaths4.txt", header = TRUE, skip = 0)
deaths_W5 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Women/Deaths/Deaths5.txt", header = TRUE, skip = 0)
deaths_W6 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Women/Deaths/Deaths6.txt", header = TRUE, skip = 0)
deaths_W7 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Women/Deaths/Deaths7.txt", header = TRUE, skip = 0)
deaths_W8 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Women/Deaths/Deaths8.txt", header = TRUE, skip = 0)
deaths_W9 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Women/Deaths/Deaths9.txt", header = TRUE, skip = 0)
deaths_W_total <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Women/Deaths/DeathsX.txt", header = TRUE, skip = 0)
# Check for the same data in the two folders (men and women)
identical(deaths_M0, deaths_W0)
identical(deaths_M1, deaths_W1)
identical(deaths_M2, deaths_W2)
identical(deaths_M3, deaths_W3)
identical(deaths_M4, deaths_W4)
identical(deaths_M5, deaths_W5)
identical(deaths_M6, deaths_W6)
identical(deaths_M7, deaths_W7)
identical(deaths_M8, deaths_W8)
identical(deaths_M9, deaths_W9)
identical(deaths_M_total, deaths_W_total)
# Kun forskelle i de gruppe 0 og gruppe 1-filerne, da disse to grupper er slået sammen for kvinderne. 
exp_M0 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Men/Exp/Exp0.txt", header = TRUE, skip = 0)
exp_M1 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Men/Exp/Exp1.txt", header = TRUE, skip = 0)
exp_M2 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Men/Exp/Exp2.txt", header = TRUE, skip = 0)
exp_M3 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Men/Exp/Exp3.txt", header = TRUE, skip = 0)
exp_M4 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Men/Exp/Exp4.txt", header = TRUE, skip = 0)
exp_M5 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Men/Exp/Exp5.txt", header = TRUE, skip = 0)
exp_M6 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Men/Exp/Exp6.txt", header = TRUE, skip = 0)
exp_M7 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Men/Exp/Exp7.txt", header = TRUE, skip = 0)
exp_M8 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Men/Exp/Exp8.txt", header = TRUE, skip = 0)
exp_M9 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Men/Exp/Exp9.txt", header = TRUE, skip = 0)
exp_M_total <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Men/Exp/ExpX.txt", header = TRUE, skip = 0)
exp_W0 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Women/Exp/Exp0.txt", header = TRUE, skip = 0)
exp_W1 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Women/Exp/Exp1.txt", header = TRUE, skip = 0)
exp_W2 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Women/Exp/Exp2.txt", header = TRUE, skip = 0)
exp_W3 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Women/Exp/Exp3.txt", header = TRUE, skip = 0)
exp_W4 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Women/Exp/Exp4.txt", header = TRUE, skip = 0)
exp_W5 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Women/Exp/Exp5.txt", header = TRUE, skip = 0)
exp_W6 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Women/Exp/Exp6.txt", header = TRUE, skip = 0)
exp_W7 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Women/Exp/Exp7.txt", header = TRUE, skip = 0)
exp_W8 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Women/Exp/Exp8.txt", header = TRUE, skip = 0)
exp_W9 <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Women/Exp/Exp9.txt", header = TRUE, skip = 0)
exp_W_total <- utils::read.table("C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataNy/Women/Exp/ExpX.txt", header = TRUE, skip = 0)
# Check for the same data in the two folders (men and women)
identical(exp_M0, exp_W0)
identical(exp_M1, exp_W1)
identical(exp_M2, exp_W2)
identical(exp_M3, exp_W3)
identical(exp_M4, exp_W4)
identical(exp_M5, exp_W5)
identical(exp_M6, exp_W6)
identical(exp_M7, exp_W7)
identical(exp_M8, exp_W8)
identical(exp_M9, exp_W9)
identical(exp_M_total, exp_W_total)
# Kun forskelle i de gruppe 0 og gruppe 1-filerne, da disse to grupper er slået sammen for kvinderne
# apply(exp_M1, 2, function(x) any(is.na(x)))
# apply(exp_M2, 2, function(x) any(is.na(x)))
# apply(exp_M3, 2, function(x) any(is.na(x)))
# apply(deaths_M1, 2, function(x) any(is.na(x)))
# apply(deaths_M2, 2, function(x) any(is.na(x)))
# apply(deaths_M3, 2, function(x) any(is.na(x)))



death_list <- list("deaths_M0" = deaths_M0, "deaths_M1" = deaths_M1, "deaths_M2" = deaths_M2,
                   "deaths_M3" = deaths_M3, "deaths_M4" = deaths_M4, "deaths_M5" = deaths_M5, 
                   "deaths_M6" = deaths_M6, "deaths_M7" = deaths_M7, "deaths_M8" = deaths_M8, 
                   "deaths_M9" = deaths_M9, "deaths_M_total" = deaths_M_total)
exp_list <- list("exp_M0" = exp_M0, "exp_M1" = exp_M1, "exp_M2" = exp_M2,
                 "exp_M3" = exp_M3, "exp_M4" = exp_M4, "exp_M5" = exp_M5, 
                 "exp_M6" = exp_M6, "exp_M7" = exp_M7, "exp_M8" = exp_M8, 
                 "exp_M9" = exp_M9, "exp_M_total" = exp_M_total)




# Aggregate into age intervals for socioeconomic group

# Functions that aggregates the number of deaths or the number of people for an age range
# Same year but an interval of ages
aggregateAgsIntoOneGroup <- function(df){
  df$AgeGroup <- ifelse(df$Age %in% c(50:57), "50-57", ifelse(df$Age %in% c(58:65), "58-65", 
                                                              ifelse(df$Age %in% c(66:72), "66-72", ifelse(df$Age %in% c(73:79), "73-79", 
                                                                                                           ifelse(df$Age %in% c(80:87), "80-87", ifelse(df$Age %in% c(88:95), "88-95", NA))))))
  interval_df <- df %>%  group_by(Year, AgeGroup) %>% summarise_all(funs(sum))
  interval_df <- data.frame(interval_df)
  interval_df$Age <- NULL
  colnames(interval_df)[2] <- "Age"
  
  return(interval_df)
}

death_list_int <- lapply(death_list, FUN = aggregateAgsIntoOneGroup)
exp_list_int <- lapply(exp_list, FUN = aggregateAgsIntoOneGroup)


# Functions that calculates mortality rates and prepare them for plotting
calculateMortality <- function(death_df, exp_df, logIsTaken){
  
  # death_df <- deaths_M2
  # exp_df <- exp_M2
  # logIsTaken <- TRUE
  
  commonYearsAndAges <- semi_join(death_df, exp_df, by = c("Year", "Age"))[c("Year", "Age")]
  death_df <- left_join(commonYearsAndAges, death_df, by = c("Year", "Age"))
  exp_df <- left_join(commonYearsAndAges, exp_df, by = c("Year", "Age"))
  
  mortality_rates <- death_df[ , 3:5] / exp_df[ , 3:5]
  if(logIsTaken == TRUE){
    mortality_rates <- log(mortality_rates) 
  }
  mortality_rates <- cbind(death_df[ , 1:2], mortality_rates)
  
  # Split into mortality rates tables for each group format (time x ages)
  mortality_rate_women <- data.frame(dcast(mortality_rates, Year ~ Age, value = "Women"))
  mortality_rate_men <- data.frame(dcast(mortality_rates, Year ~ Age, value = "Men"))
  mortality_rate_all <- data.frame(dcast(mortality_rates, Year ~ Age, value = "Total"))
  
  return_list <- list("Women" = mortality_rate_women, "Men" = mortality_rate_men, "Total" = mortality_rate_all)
  return(return_list)
}

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

rm(list=setdiff(ls(), c("calculateMortality", "death_list_int", "exp_list_int", "death_list", "exp_list")))

# Save RData and txt-files
save.image(file="C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/Agg_data.Rdata") 

write.table(death_list_int$deaths_M0, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/deaths_1.txt")
write.table(death_list_int$deaths_M1, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/deaths_2.txt")
write.table(death_list_int$deaths_M2, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/deaths_3.txt")
write.table(death_list_int$deaths_M3, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/deaths_4.txt")
write.table(death_list_int$deaths_M4, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/deaths_5.txt")
# write.table(death_list_int$deaths_M0, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/deaths_1.txt")
# write.table(death_list_int$deaths_M1, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/deaths_2.txt")
# write.table(death_list_int$deaths_M2, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/deaths_3.txt")
# write.table(death_list_int$deaths_M3, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/deaths_4.txt")
# write.table(death_list_int$deaths_M4, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/deaths_5.txt")
write.table(exp_list_int$exp_M0, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/exp_1.txt")
write.table(exp_list_int$exp_M1, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/exp_2.txt")
write.table(exp_list_int$exp_M2, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/exp_3.txt")
write.table(exp_list_int$exp_M3, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/exp_4.txt")
write.table(exp_list_int$exp_M4, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/exp_5.txt")
# write.table(exp_list_int$exp_M0, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/exp_1.txt")
# write.table(exp_list_int$exp_M1, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/exp_2.txt")
# write.table(exp_list_int$exp_M2, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/exp_3.txt")
# write.table(exp_list_int$exp_M3, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/exp_4.txt")
# write.table(exp_list_int$exp_M4, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/exp_5.txt")

death_list_int$deaths_M0$Age <- substr(death_list_int$deaths_M0$Age, start = 1, stop = 2)
write.table(death_list_int$deaths_M0, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/Deaths_1_1.txt")
death_list_int$deaths_M1$Age <- substr(death_list_int$deaths_M0$Age, start = 1, stop = 2)
write.table(death_list_int$deaths_M1, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/Deaths_2_1.txt")
death_list_int$deaths_M2$Age <- substr(death_list_int$deaths_M0$Age, start = 1, stop = 2)
write.table(death_list_int$deaths_M2, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/Deaths_3_1.txt")
death_list_int$deaths_M3$Age <- substr(death_list_int$deaths_M0$Age, start = 1, stop = 2)
write.table(death_list_int$deaths_M3, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/Deaths_4_1.txt")
death_list_int$deaths_M4$Age <- substr(death_list_int$deaths_M0$Age, start = 1, stop = 2)
write.table(death_list_int$deaths_M4, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/Deaths_5_1.txt")
# write.table(death_list_int$deaths_M0, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/deaths_1.txt")
# write.table(death_list_int$deaths_M1, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/deaths_2.txt")
# write.table(death_list_int$deaths_M2, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/deaths_3.txt")
# write.table(death_list_int$deaths_M3, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/deaths_4.txt")
# write.table(death_list_int$deaths_M4, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/deaths_5.txt")
exp_list_int$deaths_M0$Age <- substr(death_list_int$deaths_M0$Age, start = 1, stop = 2)
write.table(exp_list_int$exp_M0, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/Exp_1_1.txt")
exp_list_int$deaths_M1$Age <- substr(death_list_int$deaths_M0$Age, start = 1, stop = 2)
write.table(exp_list_int$exp_M1, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/Exp_2_1.txt")
exp_list_int$deaths_M2$Age <- substr(death_list_int$deaths_M0$Age, start = 1, stop = 2)
write.table(exp_list_int$exp_M2, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/Exp_3_1.txt")
exp_list_int$deaths_M3$Age <- substr(death_list_int$deaths_M0$Age, start = 1, stop = 2)
write.table(exp_list_int$exp_M3, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/Exp_4_1.txt")
exp_list_int$deaths_M4$Age <- substr(death_list_int$deaths_M0$Age, start = 1, stop = 2)
write.table(exp_list_int$exp_M4, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/Exp_5_1.txt")
# write.table(exp_list_int$exp_M0, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/exp_1.txt")
# write.table(exp_list_int$exp_M1, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/exp_2.txt")
# write.table(exp_list_int$exp_M2, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/exp_3.txt")
# write.table(exp_list_int$exp_M3, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/exp_4.txt")
# write.table(exp_list_int$exp_M4, file = "C:/Users/Dorthe B/Desktop/Speciale/R_coding/Data/DataByDorthe/exp_5.txt")

