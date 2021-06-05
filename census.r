library(tidyverse)
library(readxl)
library(here)

# States we are interested in
states_of_interest <- data.frame(State = c("Alabama", "Kentucky", "Missouri", "South Carolina", "Texas", "West Virginia"), Abb = c("AL", "KY", "MO", "SC", "TX", "WV"))
states_of_interest <- as_tibble(states_of_interest)

# education data
census_edu <- as_tibble(read.csv(here("Census Data", "ACS_10_5YR_S1501.csv"), header = T))

# Extract data for bachelor or higher
census_edu_clean <- census_edu %>%
  select(County = which(str_detect(unlist(.[1,]), "Geographic Area Name")== T), PercentBachelorOrHigher = which(str_detect(unlist(.[1,]), "Total!!Estimate!!Percent bachelor's degree or higher") == T)) %>%
  slice(-1) %>%
  mutate(PercentBachelorOrHigher = as.numeric(PercentBachelorOrHigher) * 0.01) %>%
  drop_na()

# Check no data was lost
nrow(census_edu) -1
nrow(census_edu_clean)

# Age data
census_age <- as_tibble(read.csv(here("Census Data", "ACS_1Y_2010_AGE.csv"), header = T))

# Extract data for 18 and under
census_age_clean <- census_age %>%
  select(County = which(str_detect(unlist(.[1,]), "Geographic Area Name") == T), TotalUnder18 = which(str_detect(unlist(.[1,]), "Estimate!!Total") == T)) %>%
  select(1, TotalUnder18 = 2) %>%
  slice(-1)

# Check no data was lost
nrow(census_age) -1
nrow(census_age_clean)

# Population data
census_totalpop2010 <- as_tibble(read.csv(here("Census Data", "ACS_1Y_2010_POP_TOTAL.csv"), header = T))

# Extract data for total population
census_totalpop2010_clean <- census_totalpop2010 %>%
  select(County = 2, total_pop_2010 = 3) %>%
  slice(-1) %>%
  mutate(total_pop_2010 = as.numeric(total_pop_2010)) %>%
  drop_na()

# Check no data was lost
nrow(census_totalpop2010) -1
nrow(census_totalpop2010_clean)

# census_clean_sets <- list(census_edu_clean, census_age_clean, census_totalpop2010_clean)
# glimpse(census_clean_sets)

# Merge all 3 dataframes
census_edu_age_pop_merge <- inner_join(census_edu_clean, census_age_clean, "County") %>%
  inner_join(census_totalpop2010_clean, "County")

View(census_edu_age_pop_merge)
