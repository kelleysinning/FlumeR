# Flume Experiment Data for Ch. 1 of K. Sinning Dissertation Research
# Spring 2026


#load important packages#
library(ggplot2)
library(gridExtra)
library(viridis)
library(ggthemes)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(rcartocolor)
library(purrr)


# Set working directory 
setwd("~/Library/CloudStorage/OneDrive-Colostate/Flume experiment/Data/FlumeR")

# Flume data!
Floom <- read.csv("FlumeData.csv")

library(tidyverse)

library(tidyverse)

Floom_Long <- Floom %>%
  pivot_longer(cols = starts_with("Percent.Change"),
               names_to = "variable",
               values_to = "pct_change")



ggplot(Floom_Long, aes(x = Trial.....details, y = pct_change)) +
  
  # boxplots: fill = sediment, color = slope
  geom_boxplot(aes(fill = Sediment.Type, color = Slope),
               position = position_dodge2(width = 0.8),
               alpha = 0.7) +
  
  # optional: show raw rock points
 # geom_jitter(aes(color = Slope),
  #            width = 0.2,
   #           alpha = 0.5,
    #          size = 1.5) +
  
  # facet by variable (cyano, diatom, green)
  facet_wrap(~ variable, scales = "free_y") +
  
  
  # labels and theme
  labs(x = "Trial",
       y = "% Change",
       fill = "Sediment Type",
       color = "Slope") +
  theme_classic()
