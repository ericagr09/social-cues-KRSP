# Data file prep for final analysis files 
library(dplyr)
litter_raw <- read.csv("IntermediateData/litter.csv", header = TRUE)
# keep only neccessary rows 
litter_analysis <- litter_raw %>%
  select(squirrel_id, litter_id, grid, year, mean_growth, 
         cones_tm1, spr_density, age, bucket, bucket2, 
         ls, std_Growt, std_LS) 
# add in part dates from Andrews updated datafile
part_dates <- read.csv("IntermediateData/part_dates.csv", header = TRUE)

part_dates1 <- part_dates %>% 
  select(litter_id, part_date)

litter_analysis2 <- litter_analysis %>% 
  left_join(part_dates1, by = "litter_id")

# add cov to data frame 
litter_clean <- na.omit(litter_analysis2)
covariances <- numeric(nrow(litter_clean))
# Calculate covariance for each individual observation
for (i in 1:nrow(litter_clean)) {
  covariances[i] <- cov(litter_clean$std_Growt[1:i], litter_clean$std_LS[1:i], use = "complete.obs")
}
litter_clean$covariance <- covariances

# create grid_year variable 
litter_clean1 <- litter_clean %>%
  mutate(grid_year = paste(grid, year, sep = "_"))

litter_clean2 <- litter_clean1 %>%
  group_by(grid_year) %>%
  mutate(scaled_part_date = scale(part_date)) %>%
  ungroup()

write.csv(litter_clean2, "~/University of Michigan Dropbox/Erica Griffin/Erica Griffin - shared folder/W25 social cue analysis/social-cues-KRSP/AnalysisData/litter_analysis_cov.csv")

