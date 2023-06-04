library(tidyverse)
library(janitor)
library(patchwork)
# library(lubridate)
# library(kableExtra)
# library(formatR)

# ==== Load in Data ==== #

# Option 1 (recommended): set working directory to project or file location, then load files:
setwd("/Users/Sam/OneDrive - University of Edinburgh/PhD/Conferences, Talks, Workshops/Workshops/LELPGC23/Files-main/data-files")
main_data_raw <- read_csv("hansen_main_data_for_analysis.csv")
nw_full_raw <- read_csv("norwegian_word_for_analysis.csv")

# Option 2: Read using full file path
main_data_raw <- read_csv("/Users/Sam/OneDrive - University of Edinburgh/PhD/Conferences, Talks, Workshops/Workshops/LELPGC23/Files-main/data-files/hansen_main_data_for_analysis.csv")
nw_full_raw <- read_csv("/Users/Sam/OneDrive - University of Edinburgh/PhD/Conferences, Talks, Workshops/Workshops/LELPGC23/Files-main/data-files/norwegian_word_for_analysis.csv")

# ==== Examine data ==== #

glimpse(nw_full_raw)
glimpse(main_data_raw)

head(main_data_raw)
head(nw_full_raw)

# ==== Tidy datasets ==== 

nw_full_clean <- nw_full_raw %>%
  clean_names() # tidy variable names, put in snake_case

nw_full <- nw_full_clean %>%
  # some variables are classed as characters when should be numbers
  mutate(number_of_sounds = as.numeric(number_of_sounds), 
         number_of_syllables = as.numeric(number_of_syllables),
         # Now we have some that should be factors -- non-numeric, but 'verb' 'noun' and 'adjective' mean something
         word_class = as.factor(word_class),
         imageability_level = fct_relevel(imageability_level, # order of levels matters, so we use fct_relevel
                                          c("low", "medium", "high")), 
         number_of_phonological_neighbours_level = fct_relevel(number_of_phonological_neighbours_level,
                                                               c("few", "some", "many", "no info"))
  )

main_data_clean <- main_data_raw %>%
  clean_names()

main_data <- main_data_clean %>%
  rename(word = word_nw, aoa = ao_a, vsoa = v_so_a) %>%
  mutate(word = word %>% str_replace_all(pattern = "'", replacement = ""), 
         translation = translation %>% str_replace_all(pattern = "'", replacement = ""), 
         aoa = as.numeric(aoa),
         vsoa = as.numeric(vsoa), 
         freq = as.numeric(freq), 
         lex_cat = as.factor(lex_cat), 
         broad_lex = as.factor(broad_lex)
         ) %>%
  select(word, # now choose only the variables that we're interested in: remove CDS only; 
         translation, 
         aoa, 
         vsoa, 
         freq, 
         lex_cat, 
         broad_lex)

# Or just select() variables we *don't* want: 
main_data <- main_data_raw %>%
  clean_names() %>%
  rename(word = word_nw, aoa = ao_a, vsoa = v_so_a) %>%
  mutate(word = word %>% str_replace_all(pattern = "'", replacement = ""), 
         translation = translation %>% str_replace_all(pattern = "'", replacement = ""), 
         aoa = as.numeric(aoa),
         vsoa = as.numeric(vsoa), 
         freq = as.numeric(freq), 
         lex_cat = as.factor(lex_cat), 
         broad_lex = as.factor(broad_lex)
  ) %>%
  select(-c(id_cdi_i, id_cdi_ii, cds_freq, word_cdi)) # could also use select(-id_cdi_i, -id_cdi_ii, ...) 


# ==== Combine datasets ====

all_data <- main_data %>%
  left_join(y = nw_full, by = "word")
  # filter(is.na(imageability) == FALSE)
  
all_data <- left_join(x = main_data, y = nw_full, by = "word")
  # filter(is.na(imageability) == FALSE)


# Some basic descriptive statistics  
summary(all_data) # get distributions -- lots of missing values 


# Summarise data only by word class
all_data %>% 
  filter(is.na(imageability) == FALSE) %>% # because there are many missing cases 
  group_by(word_class) %>%
  summarise(mean.aoa = mean(aoa, na.rm = T), 
            mean.vsoa = mean(vsoa, na.rm = T), 
            sd.vsoa = sd(vsoa, na.rm = T)) %>% 
  arrange(desc(mean.aoa), desc(mean.vsoa))

# What if we also were interested in how this


# ==== Tidy Environment ==== 
rm(list = ls()[!ls() %in% c("main_data", "nw_full", "all_data")])
# save.image("NW_LELPGC23.RData") # Saves background image to an R data file

# Can also export to excel
write_csv(all_data, "all_data.csv")
