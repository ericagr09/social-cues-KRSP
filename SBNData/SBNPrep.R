## SBN data prep: need to link in the cort levels - I 

library (plyr)  #Causes conflicts with dplyr - needs to load first
library (krsp)
library (dplyr)
library (lubridate)
library(tidyr)
select = dplyr::select #necessary as MASS also has a select function

# Connection to the AWS database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)

poop <- read.csv("~/University of Michigan Dropbox/Erica Griffin/Erica Griffin - shared folder/W25 social cue analysis/social-cues-KRSP/SBNData/poop_db.csv")


trapping = tbl(con, "trapping") %>%
  collect() %>% 
  select(trapping_id = id, squirrel_id, wgt, trap_date = date, sex, nipple, grid = gr, rep_con) %>%
  mutate(trap_month = month((ymd(trap_date)), label = TRUE),
         trap_day = yday((ymd(trap_date))),
         year = year(trap_date),
         masting = case_when(
           year %in% c(1993, 1998, 2005, 2010, 2014, 2019, 2022) ~ "mast",
           TRUE ~ "nonmast")) 

# Pull the litter table (there are some litter table errors on the github that should probably be included here too)
litter = tbl(con, "litter") %>% 
  collect() %>%
  select(part = fieldBDate, food, squirrel_id, br, ln, litter_id = id)


#join the litter and trapping info
results = left_join(litter, trapping, by = "squirrel_id", relationship = "many-to-many") %>% #join trapping with litter data
  collect()




#its always best to link by trapping_id info with the poop_link table rather than poop_id

#this will give all the traps for a squirrel in a given year for each part date (multiple rows of traps for a part date in a given year)
female_litter_df <-  left_join (poop, results, by = c("trapping_id", "squirrel_id", "year"), relationship = "many-to-many") %>%
  #create integer values around part dates
  mutate(rel_part = as.integer(difftime(trap_date, part, units = "days")),      # time between trapping and part (negative means before part)
         rel_week = as.integer(difftime(trap_date, part, units = "weeks"))) %>% 
  #Pull traps within window of interest for a given litter from pre-estrus through lactation
  filter(rel_part <= 70 & rel_part >= -70) %>% 
  # Define rep status for samples
  mutate(rep_status = case_when( 
    rel_part < 0 & rel_part >= -35 ~ "Pregnant",
    rel_part >=0 ~ "Lactating",
    TRUE ~ "Pre-estrus")) %>% #samples that are within 35 days prior to mating chase
  #it's possible that a poop falls into two different rep_statuses if there are traps really close together, this will flag those cases and flag when rep_statuses disagree for a poop
  group_by(poop_id) %>%
  mutate(duplicate_poop_row = n() > 1,
         different_rep_status = n_distinct(rep_status) > 1) %>%
  ungroup() %>%
  filter(!is.na(poop_id)) #this will filter out poop_ids without traps

#Take a look :D
View(female_litter_df)

write.csv(female_litter_df, "~/University of Michigan Dropbox/Erica Griffin/Erica Griffin - shared folder/W25 social cue analysis/social-cues-KRSP/SBNData/female_litter_df.csv")

cort <- read.csv("~/University of Michigan Dropbox/Erica Griffin/Erica Griffin - shared folder/W25 social cue analysis/social-cues-KRSP/SBNData/cort_initial.csv")

cort1 <- cort %>% 
  select(Poop.Vial.ID, Cort.Final.Concentration) %>%
  rename(
    poop_id = Poop.Vial.ID, 
    cort = Cort.Final.Concentration)

cort2 <- female_litter_df %>%
  left_join(cort1, by = "poop_id") 


cort3 <- cort2 %>%
  filter(rep_status %in% c("Pregnant", "Lactating")) %>%
  mutate(
    target_relpart = case_when(
      rep_status == "Pregnant" ~ 0, # since multiple observatins, keeping the one that is closest to part date for preganant, and closest to 25 days (second nest entry) for lactating
      rep_status == "Lactating" ~ 25
    ),
    abs_diff = abs(rel_part - target_relpart)
  ) %>%
  group_by(litter_id, rep_status) %>%
  slice_min(order_by = abs_diff, n = 1, with_ties = FALSE) %>%
  ungroup()

# Check and make sure only 2 for each 
cort3 %>% 
  count(litter_id, rep_status) %>%
  filter(n > 1)

cort4 <- cort3 %>%
  select(litter_id, rep_status, cort) %>%
  pivot_wider(
    names_from = rep_status,
    values_from = cort,
    names_glue = "{tolower(substr(rep_status, 1, 3))}_cort"
  )

write.csv(cort4, "~/University of Michigan Dropbox/Erica Griffin/Erica Griffin - shared folder/W25 social cue analysis/social-cues-KRSP/SBNData/raw_cort.csv")

cov <- read.csv("~/University of Michigan Dropbox/Erica Griffin/Erica Griffin - shared folder/W25 social cue analysis/social-cues-KRSP/AnalysisData/litter_analysis_cov.csv")

cort5 <- cov %>%
  left_join(cort4, by = "litter_id") 

write.csv(cort5, "~/University of Michigan Dropbox/Erica Griffin/Erica Griffin - shared folder/W25 social cue analysis/social-cues-KRSP/SBNData/cort_analysis.csv")

