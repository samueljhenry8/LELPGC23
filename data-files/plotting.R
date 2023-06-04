library(tidyverse)
library(patchwork)

load("NW_LELPGC23.RData")

# ==== Visualisations / Plotting ====
# Histograms of 
hist.aoa <- all_data %>% ggplot(aes(x= aoa)) + 
  geom_histogram(aes(y = (after_stat(count))/sum(after_stat(count))), fill = 'lightgreen', color = "darkgray", bins = 18) +
  geom_density(aes(y = (after_stat(count))/sum(after_stat(count)) * 10)) +
  scale_y_continuous(name = "Percentage of All Words", labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(name = "Age of Aquisition (Months)", breaks = seq(from = 0, to = 40, by = 5)) + 
  ggtitle("Distribution of Age of Acquisition in Norwegian Words") +
  theme_minimal()

hist.vsoa <- ggplot(all_data, aes(x= vsoa)) + 
  geom_histogram(aes(y = (after_stat(count))/sum(after_stat(count))), fill = 'lightblue', color = "darkgray", bins = 20) +
  geom_density(aes(y = (after_stat(count))/sum(after_stat(count)) * 10)) +
  scale_y_continuous(name = "Percentage of All Words", labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(name = "Vocabulary Size of Aquisition", breaks = seq(from = 0, to = 700, by = 100)) + 
  ggtitle("Distribution of Vocabulary Size of Acquisition in Norwegian Words") +
  theme_minimal()

plot1 <- hist.aoa + hist.vsoa
plot1

# Plot 2: Scatter plot of AoA vs VSoA
all_data %>% ggplot(aes(x= vsoa, y = aoa)) + 
  geom_point(position = "jitter", alpha = .5) + 
  stat_smooth(color = "black") + 
  scale_x_continuous(limits = c(0, 700), breaks = seq(0, 700, by = 100)) +
  scale_y_continuous(limits = c(10, 40), breaks = seq(0, 40, by = 5)) +
  xlab("Vocabulary Size of Aquisition") + 
  ylab("Age of Aquisition (months)") + 
  ggtitle("The relationship between AoA and VSoA") + 
  theme_bw()


# Plot 3: AoA for different classes: noun/verb/adj, number of phonological neighbours
ggplot(all_data, aes(x = word_class, y = aoa)) + 
  geom_violin(fill = "lightgrey", color = "black", alpha = 0.8) +
  # geom_point(position = "jitter", alpha = .3) + 
  geom_boxplot(varwidth = T, fill = "white", color = "black", alpha = 0.5) +
  labs(x = "Word Class", 
       y = "Age of Acquisition (months)", 
       title = "Distributions of AoA by Word Class", 
       subtitle = "for 467 Norwegian words",
       caption = "Note. Width of boxplot reflects proportion of total words") +
  scale_y_continuous(limits = c(10,40), breaks = seq(from = 0, to = 40, by = 5)) +
  coord_flip() +
  theme_classic()


# Plot 4 (Gettin' Fancy): x = Imageability, y = VSoA — both variables standardised; add shapes for 1) word class and 2) number of phonological neighbours
all_data$vsoa %>% summary
all_data$imageability %>% summary

all_data <- all_data %>%
  mutate(imageability_exp_std = exp(imageability))

all_data$imageability %>% plot
all_data$imageability_exp_std %>% plot

# Plot 4
all_data %>% 
  filter(number_of_phonological_neighbours_level != "no info") %>% 
  ggplot(aes(scale(exp(imageability)), scale(vsoa), colour = number_of_phonological_neighbours_level, group = number_of_phonological_neighbours_level, shape = number_of_phonological_neighbours_level)) + 
  geom_point(position = "jitter") +
  # geom_count() +
  geom_smooth(method = "lm", se = F, alpha = .2) +
  facet_wrap(~word_class) +
  # Specify both colour and shape
  scale_shape_discrete(name  ="Neighbours",
                       breaks=c("few", "some", "many"),
                       labels=c("Few", "Some", "Many")) +
  scale_colour_discrete(name  ="Neighbours",
                        breaks=c("few", "some", "many"),
                        labels=c("Few", "Some", "Many")) + 
  xlab("Imageability (standardised)") + 
  ylab("VSoA (standardised)") + 
  theme_bw()

