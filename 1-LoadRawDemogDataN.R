# Loads death rates and exposures and turn them into demography data objects 
# Note: Contains values for both men and women

library("demography")

# Load Malenes mortality data grouped ----
mx_M0 <- "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/Data/DataNy/Men/Mx/Mx0.txt"
mx_M1 <- "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/Data/DataNy/Men/Mx/Mx1.txt"
mx_M2 <- "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/Data/DataNy/Men/Mx/Mx2.txt"
mx_M3 <- "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/Data/DataNy/Men/Mx/Mx3.txt"
mx_M4 <- "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/Data/DataNy/Men/Mx/Mx4.txt"
mx_M5 <- "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/Data/DataNy/Men/Mx/Mx5.txt"
mx_M6 <- "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/Data/DataNy/Men/Mx/Mx6.txt"
mx_M7 <- "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/Data/DataNy/Men/Mx/Mx7.txt"
mx_M8 <- "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/Data/DataNy/Men/Mx/Mx8.txt"
mx_M9 <- "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/Data/DataNy/Men/Mx/Mx9.txt"
mx_M_total <- "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/Data/DataNy/Men/Mx/MxX.txt"
exp_M0 <- "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/Data/DataNy/Men/Exp/Exp0.txt"
exp_M1 <- "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/Data/DataNy/Men/Exp/Exp1.txt"
exp_M2 <- "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/Data/DataNy/Men/Exp/Exp2.txt"
exp_M3 <- "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/Data/DataNy/Men/Exp/Exp3.txt"
exp_M4 <- "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/Data/DataNy/Men/Exp/Exp4.txt"
exp_M5 <- "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/Data/DataNy/Men/Exp/Exp5.txt"
exp_M6 <- "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/Data/DataNy/Men/Exp/Exp6.txt"
exp_M7 <- "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/Data/DataNy/Men/Exp/Exp7.txt"
exp_M8 <- "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/Data/DataNy/Men/Exp/Exp8.txt"
exp_M9 <- "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/Data/DataNy/Men/Exp/Exp9.txt"
exp_M_total <- "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/Data/DataNy/Men/Exp/ExpX.txt"

# Tun into the demography data object
data_GR0_demog <- read.demogdata(file=mx_M0, popfile=exp_M0, type="mortality", label="Denmark", skip=0, lambda=0)
data_GR1_demog <- read.demogdata(file=mx_M1, popfile=exp_M1, type="mortality", label="Denmark", skip=0, lambda=0)
data_GR2_demog <- read.demogdata(file=mx_M2, popfile=exp_M2, type="mortality", label="Denmark", skip=0, lambda=0)
data_GR3_demog <- read.demogdata(file=mx_M3, popfile=exp_M3, type="mortality", label="Denmark", skip=0, lambda=0)
data_GR4_demog <- read.demogdata(file=mx_M4, popfile=exp_M4, type="mortality", label="Denmark", skip=0, lambda=0)
data_GR5_demog <- read.demogdata(file=mx_M5, popfile=exp_M5, type="mortality", label="Denmark", skip=0, lambda=0)
data_GR6_demog <- read.demogdata(file=mx_M6, popfile=exp_M6, type="mortality", label="Denmark", skip=0, lambda=0)
data_GR7_demog <- read.demogdata(file=mx_M7, popfile=exp_M7, type="mortality", label="Denmark", skip=0, lambda=0)
data_GR8_demog <- read.demogdata(file=mx_M8, popfile=exp_M8, type="mortality", label="Denmark", skip=0, lambda=0)
data_GR9_demog <- read.demogdata(file=mx_M9, popfile=exp_M9, type="mortality", label="Denmark", skip=0, lambda=0)
data_GR_total_demog <- read.demogdata(file=mx_M_total, popfile=exp_M_total, type="mortality", label="Denmark", skip=0, lambda=0)

# Remove unecessary data
rm(list=setdiff(ls(), c("data_GR0_demog", "data_GR1_demog", "data_GR2_demog", "data_GR3_demog", 
                        "data_GR4_demog", "data_GR5_demog", "data_GR6_demog", "data_GR7_demog", 
                        "data_GR8_demog", "data_GR8_demog", "data_GR9_demog", "data_GR_total_demog") ))

save.image(file = "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/MortalityModels/RawDemogData.RData")
