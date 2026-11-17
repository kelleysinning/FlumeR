# AGU Final Figures

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
install.packages("ggpubr") 
library(ggpubr)
library(lme4)
library(lmerTest)   
library(emmeans)     
install.packages("multcompView")
library(multcompView)

# Set working directory 
setwd("~/Library/CloudStorage/OneDrive-TheUniversityofMontana/Flume experiment/Data/FlumeR")
Front.Back <- read.csv("Front.Back.csv")

Front.Back <- Front.Back %>%
  mutate(
    across(starts_with("X..Change"), ~ as.numeric(as.character(.)))
  )%>%
  rename(
    Trial = Trial..,   # old name = a → new name = new_a
    Percent.Change.Mat.Thickness = X..Change.Mat.Thickness,
    Percent.Change.in.Diatoms = X..Change.in.Diatom,
    Percent.Change.in.Green = X..Change.in.Green,
    Percent.Change.in.Cyano = X..Change.in.Cyano,
    Percent.Change.in.AFDM = X..Change.AFDM,
    Shear.Stress = Shear.Stress..Pa.,
    Shear.Velocity = Shear.Velocity..m.s.)

Front.Back <- Front.Back %>%
  mutate(
    y_raw = pmin(pmax(-.data[[resp]], 0), 100) / 100, # Proportion REDUCTION; increases (positive % change) capped at 0
    y = (y_raw * (n() - 1) + 0.5) / n()) # Squeeze off 0 and 1 for the beta likelihood

Front.Back <- Front.Back %>% # adding a column with z-scored shear stress 
  group_by(Trial) %>%
  mutate(
    Mean.Trial.Shear = mean(Shear.Stress, na.rm = TRUE),
    Mean.Trial.AFDM.Removal = mean(y, na.rm = TRUE),
    Mean.Trial.Diatom.Removal = mean(Percent.Change.in.Diatoms, na.rm = TRUE)) %>%
  ungroup()

Front.Back$Trial <- factor(Front.Back$Trial)
Front.Back$Slope <- factor(Front.Back$Slope, levels = c("Low", "Medium", "High"))
Front.Back$Sediment.Type <- factor(Front.Back$Sediment.Type,
                                   levels = c("None", "Sand", "Gravel"))
Front.Back$Position <- factor(Front.Back$Position, levels = c("Front", "Back"))



# Trials on x--------------------------------

ggplot(Front.Back, aes(x = Mean.Trial.Shear,
                       y = Mean.Trial.AFDM.Removal,
                       color = Slope,
                       shape = Sediment.Type,
                       fill = Sediment.Type)) +
  #geom_boxplot(position = position_dodge(width = 0.8),
   #            alpha = 0.7,
    #           outlier.shape = NA) +
  geom_point(size = 5)+
  labs(x = "Trial",
       y = "% Change",
       shape = "Sediment Type",
       #fill = "Sediment.Type",
       color = "Slope") +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme_classic()




# Sediment on X-----------------------------

# % change

ggplot(Front.Back, aes(x = Sediment.Type, y = y_raw, fill = Slope, color = Slope)) +
  geom_boxplot(position = position_dodge2(width = 0.8), alpha = 0.7, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              alpha = 0.5, size = 1.5) +
  facet_wrap(~ Slope, scales = "free_y") +
  labs(x = "Sediment", y = "% Change", fill = "Slope", color = "Slope") +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "top")

# Slopes on X-----------------------------

# % change
ggplot(Front.Back, aes(x = Slope, y = y_raw, fill = Sediment.Type, color = Sediment.Type)) +
  geom_boxplot(position = position_dodge2(width = 0.8), alpha = 0.7, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              size = 1.5,
              alpha = 0.5) +
  facet_wrap(~ Sediment.Type, nrow = 1, scales = "free_y") +
  labs(x = "Slope", y = "% Change", fill = "Sediment Type", color = "Sediment Type") +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "top")


