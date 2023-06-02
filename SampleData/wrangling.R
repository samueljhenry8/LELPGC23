library(tidyverse)
library(janitor)
library(lubridate)
library(kableExtra)
library(formatR)

setwd("/Users/Sam/OneDrive - University of Edinburgh/PhD/Conferences, Talks, Workshops/Workshops/LELPGC23/SampleData")

main_data_raw <- read_csv("main_data.csv")
cds_frequency_raw <- read_csv("Norwegian_CDS_frequency.csv")
nw_full_raw <- read_csv("ordforradet_for_analysis.csv")

glimpse(main_data_raw)
glimpse(cds_frequency_raw)
glimpse(nw_full_raw)

# main_data_raw$Word_NW <- main_data_raw$Word_NW %>% str_replace_all("'", replacement = "")

nw_full <- nw_full_raw %>% clean_names()

main_data <- main_data_raw %>%
  clean_names() %>%
  rename(word = word_nw, aoa = ao_a, vsoa = v_so_a) %>%
  mutate(word = word %>% str_replace_all(pattern = "'", replacement = ""), 
         translation = translation %>% str_replace_all(pattern = "'", replacement = ""), 
         aoa = as.numeric(aoa),
         vsoa = as.numeric(vsoa), 
         freq = as.numeric(freq)) %>%
  select(word, translation, aoa, vsoa, lex_cat, broad_lex, freq)

summary(main_data)

tmp_all <- main_data %>%
  left_join(y = nw_full, by = "word") %>%
  filter(is.na(imageability) == FALSE)