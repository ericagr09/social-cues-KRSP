# Data file prep for final analysis files 
library(dplyr)
litter_raw <- read.csv("IntermediateData/litter.csv", header = TRUE)
# keep only neccessary rows 
litter_analysis <- litter_raw %>%
  select(squirrel_id, litter_id, grid, year, mean_growth, 
         cones_tm1, spr_density, age, bucket, bucket2, 
         ls, std_Growt, std_LS) 
write.csv(litter_analysis, "AnalysisData/litter_analysis.csv")