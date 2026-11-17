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

AFDM <- "Percent.Change.in.AFDM"

Front.Back <- Front.Back %>%
  mutate(
    AFDM_proportion_raw = pmin(pmax(-.data[[AFDM]], 0), 100) / 100, # Proportion REDUCTION; increases (positive % change) capped at 0
    AFDM_proportion_squeezed = (AFDM_proportion_raw * (n() - 1) + 0.5) / n(), # Squeeze off 0 and 1 for the beta likelihood
    adjusted_Percent.Change.in.AFDM = AFDM_proportion_raw*100)
    

Diatom <- "Percent.Change.in.Diatoms"

Front.Back <- Front.Back %>%
  mutate(
    Diatom_proportion_raw = pmin(pmax(-.data[[Diatom]], 0), 100) / 100, # Proportion REDUCTION; increases (positive % change) capped at 0
    Diatom_proportion_squeezed = (Diatom_proportion_raw * (n() - 1) + 0.5) / n(), # Squeeze off 0 and 1 for the beta likelihood
    adjusted_Percent.Change.in.Diatoms = Diatom_proportion_raw*100)

Front.Back <- Front.Back %>%
  mutate(Slope.num = case_when(
    Slope == "Low"  ~ 3.1,
    Slope == "Medium" ~ 6.7,
    Slope == "High" ~ 8.9,
    TRUE            ~ NA_real_
  ))

Front.Back$Trial <- factor(Front.Back$Trial)
Front.Back$Slope <- factor(Front.Back$Slope, levels = c("Low", "Medium", "High"))
Front.Back$Sediment.Type <- factor(Front.Back$Sediment.Type,
                                   levels = c("None", "Sand", "Gravel"))
Front.Back$Position <- factor(Front.Back$Position, levels = c("Front", "Back"))

# Making averaged trial df
Front.Back.Trial.Summary <- Front.Back %>%
  group_by(Trial, Slope, Sediment.Type) %>%
  summarise(
    Mean.Trial.Shear                   = mean(Shear.Stress, na.rm = TRUE),
    Mean.Trial.AFDM.Proportion.Removal = mean(proportion_raw, na.rm = TRUE),
    Mean.Trial.AFDM.Percent.Removal    = mean(adjusted_Percent.Change.in.AFDM, na.rm = TRUE),
    Mean.Trial.Diatom.Percent.Removal          = mean(adjusted_Percent.Change.in.Diatoms, na.rm = TRUE),
    .groups = "drop"
  )


Front.Back.Trial.Summary <- Front.Back.Trial.Summary %>%
  mutate(Slope.num = case_when(
    Slope == "Low"  ~ 3.1,
    Slope == "Medium" ~ 6.7,
    Slope == "High" ~ 8.9,
    TRUE            ~ NA_real_
  ))



# Trials on x--------------------------------

# AFDM
ggplot(Front.Back.Trial.Summary, aes(x = Mean.Trial.Shear,
                       y = Mean.Trial.AFDM.Percent.Removal,
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

# Diatom

ggplot(Front.Back.Trial.Summary, aes(x = Mean.Trial.Shear,
                                     y = Mean.Trial.Diatom.Removal,
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

# % change AFDM boxplots

ggplot(Front.Back, aes(x = Sediment.Type, y = adjusted_Percent.Change.in.AFDM, fill = Slope, color = Slope)) +
  geom_boxplot(position = position_dodge2(width = 0.8), alpha = 0.7, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              alpha = 0.5, size = 1.5) +
  facet_wrap(~ Slope, scales = "free_y") +
  labs(x = "Sediment", y = "% AFDM Removed", fill = "Slope", color = "Slope") +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "top")


# % change Diatom boxplots
ggplot(Front.Back, aes(x = Sediment.Type, y = adjusted_Percent.Change.in.Diatoms, fill = Slope, color = Slope)) +
  geom_boxplot(position = position_dodge2(width = 0.8), alpha = 0.7, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              alpha = 0.5, size = 1.5) +
  facet_wrap(~ Slope, scales = "free_y") +
  labs(x = "Sediment", y = "% Diatom Removed", fill = "Slope", color = "Slope") +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "top")

# Slopes on X-----------------------------

# % change AFDM boxplots
ggplot(Front.Back, aes(x = Slope, y = adjusted_Percent.Change.in.AFDM, fill = Sediment.Type, color = Sediment.Type)) +
  geom_boxplot(position = position_dodge2(width = 0.8), alpha = 0.7, outlier.shape = 17) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              size = 1.5,
              alpha = 0.5) +
  facet_wrap(~ Sediment.Type, nrow = 1, scales = "free_y") +
  labs(x = "Slope", y = "% AFDM Removed", fill = "Sediment Type", color = "Sediment Type") +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "top")


# % change AFDM linear

ggplot(Front.Back, aes(x = Slope.num,
                       y = adjusted_Percent.Change.in.AFDM,
                       fill = Sediment.Type,
                       color = Sediment.Type)) +
  #geom_jitter(width = 0.1, height = 0, size = 1.5, alpha = 0.5) +
  scale_x_continuous(limits = c(3.0, NA), expand = expansion(mult = c(0, 0.05))) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  labs(x = "% Slope", y = "% AFDM Removed",
       fill = "Sediment Type", color = "Sediment Type") +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "top")


# This leaves out so much information about individual rocks
ggplot(Front.Back.Trial.Summary, aes(x = Slope.num,
                       y = Mean.Trial.AFDM.Percent.Removal,
                       fill = Sediment.Type,
                       color = Sediment.Type)) +
  geom_jitter(width = 0.1, height = 0, size = 1.5, alpha = 0.5) +
  scale_x_continuous(limits = c(3.0, NA), expand = expansion(mult = c(0, 0.05))) +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.2) +
  labs(x = "% Slope", y = "% AFDM Removed",
       fill = "Sediment Type", color = "Sediment Type") +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "top")




# % change Diatom boxplots
ggplot(Front.Back, aes(x = Slope, y = adjusted_Percent.Change.in.Diatoms, fill = Sediment.Type, color = Sediment.Type)) +
  geom_boxplot(position = position_dodge2(width = 0.8), alpha = 0.7, outlier.shape = 17) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              size = 1.5,
              alpha = 0.5) +
  facet_wrap(~ Sediment.Type, nrow = 1, scales = "free_y") +
  labs(x = "Slope", y = "% Diatom Removed", fill = "Sediment Type", color = "Sediment Type") +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "top")


# % change Diatom linear

ggplot(Front.Back, aes(x = Slope.num,
                       y = adjusted_Percent.Change.in.Diatoms,
                       fill = Sediment.Type,
                       color = Sediment.Type)) +
  #geom_jitter(width = 0.1, height = 0, size = 1.5, alpha = 0.5) +
  scale_x_continuous(limits = c(3.0, NA), expand = expansion(mult = c(0, 0.05))) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  labs(x = "% Slope", y = "% AFDM Removed",
       fill = "Sediment Type", color = "Sediment Type") +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "top")


# This leaves out so much information about individual rocks
ggplot(Front.Back.Trial.Summary, aes(x = Slope.num,
                                     y = Mean.Trial.Diatom.Percent.Removal,
                                     fill = Sediment.Type,
                                     color = Sediment.Type)) +
  geom_jitter(width = 0.1, height = 0, size = 1.5, alpha = 0.5) +
  scale_x_continuous(limits = c(3.0, NA), expand = expansion(mult = c(0, 0.05))) +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.2) +
  labs(x = "% Slope", y = "% AFDM Removed",
       fill = "Sediment Type", color = "Sediment Type") +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "top")


