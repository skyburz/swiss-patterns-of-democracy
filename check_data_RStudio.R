# Check data

# Import data cantonal democracy data
library(readr)
library(dplyr)
library(tidyr)

# Import data
df_cantonal_democracy_data <- read_csv("data/cantonal_democracy_data.csv")

df_test <- df_cantonal_democracy_data %>%
  filter(kanton == "GL")

# Filter kantonnr kanton jahr sitzparl and all variables ending in _parl_s
df_test <- df_test %>%
  select(kantonnr, kanton, jahr, sitzparl, ends_with("_parl_s")) 

# Make the data set long format, keep all variables, use partei as the party variable
df_test_long <- df_test %>%
  pivot_longer(cols = -c(kantonnr, kanton, jahr, sitzparl), 
               names_to = "partei", 
               values_to = "partei_s") %>%
  mutate(partei = gsub("_parl_s", "", partei)) %>%
  select(kantonnr, kanton, jahr, sitzparl, partei, partei_s)

# clean the data with turning . into NA
df_test_long <- df_test_long %>%
  mutate(partei_s = na_if(partei_s, ".")) %>%
  mutate(partei_s = as.numeric(partei_s))

