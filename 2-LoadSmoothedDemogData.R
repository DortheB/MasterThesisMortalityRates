# Turn the demography data objects into smoothed demography data objects

library("demography")

# Load demography data objects:
load(file = "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/MortalityModels/RawDemogData.RData")

# Smooth the data
chosenSmoothinMethod <- "spline"
data_GR0_demog_smoothed <- demography::smooth.demogdata(data_GR0_demog, method = chosenSmoothinMethod)
data_GR1_demog_smoothed <- smooth.demogdata(data_GR1_demog, method = chosenSmoothinMethod)
data_GR2_demog_smoothed <- smooth.demogdata(data_GR2_demog, method = chosenSmoothinMethod)
data_GR3_demog_smoothed <- smooth.demogdata(data_GR3_demog, method = chosenSmoothinMethod)
data_GR4_demog_smoothed <- smooth.demogdata(data_GR4_demog, method = chosenSmoothinMethod)
data_GR5_demog_smoothed <- smooth.demogdata(data_GR5_demog, method = chosenSmoothinMethod)
data_GR6_demog_smoothed <- smooth.demogdata(data_GR6_demog, method = chosenSmoothinMethod)
data_GR7_demog_smoothed <- smooth.demogdata(data_GR7_demog, method = chosenSmoothinMethod)
data_GR8_demog_smoothed <- smooth.demogdata(data_GR8_demog, method = chosenSmoothinMethod)
data_GR9_demog_smoothed <- smooth.demogdata(data_GR9_demog, method = chosenSmoothinMethod)
data_GR_total_demog_smoothed <- smooth.demogdata(data_GR_total_demog, method = chosenSmoothinMethod)

# Remove unecessary data
rm(list=setdiff(ls(), c("data_GR0_demog_smoothed", "data_GR1_demog_smoothed", "data_GR2_demog_smoothed", "data_GR3_demog_smoothed", 
                        "data_GR4_demog_smoothed", "data_GR5_demog_smoothed", "data_GR6_demog_smoothed", "data_GR7_demog_smoothed", 
                        "data_GR8_demog_smoothed", "data_GR8_demog_smoothed", "data_GR9_demog_smoothed", "data_GR_total_demog_smoothed") ))

save.image(file = "C:/Users/Dorthe B/OneDrive/Speciale/R_Code/MortalityModels/SmoothedDemogData_spline.RData")
