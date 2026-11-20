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
Quality <- read.csv("Front.Back_GoodQuality.csv")




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
    adjusted_Percent.Change.in.AFDM = AFDM_proportion_raw*100) # new % that doesn't exceed bounda of 0 and 1
    

Diatom <- "Percent.Change.in.Diatoms"
Front.Back <- Front.Back %>% 
  mutate(
    Diatom_proportion_raw = pmin(pmax(-.data[[Diatom]], 0), 100) / 100, # Proportion REDUCTION; increases (positive % change) capped at 0
    adjusted_Percent.Change.in.Diatoms = Diatom_proportion_raw*100) # new % that doesn't exceed bounda of 0 and 1

Front.Back <- Front.Back %>% # assigning slope as a continuous var
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




# Making averaged trial df--------------------

none_label <- "None"   

# Per-trial summary (unchanged)
Front.Back.Trial.Summary <- Front.Back %>%
  group_by(Trial, Slope, Sediment.Type) %>%
  summarise(
    Mean.Trial.Shear                   = mean(Shear.Stress, na.rm = TRUE),
    Mean.Trial.AFDM.Proportion.Removal = mean(AFDM_proportion_raw, na.rm = TRUE),
    Mean.Trial.AFDM.Percent.Removal    = mean(adjusted_Percent.Change.in.AFDM, na.rm = TRUE),
    Mean.Trial.Diatom.Percent.Removal  = mean(adjusted_Percent.Change.in.Diatoms, na.rm = TRUE),
    SE.Trial.AFDM    = sd(adjusted_Percent.Change.in.AFDM, na.rm = TRUE) / sqrt(n()),
    SE.Trial.Diatoms = sd(adjusted_Percent.Change.in.Diatoms, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# No-sediment baseline per Slope, pooled across all no-sediment trials
baseline <- Front.Back %>%
  filter(Sediment.Type == none_label) %>%
  group_by(Slope) %>%
  summarise(
    Base.Mean.AFDM    = mean(adjusted_Percent.Change.in.AFDM, na.rm = TRUE),
    Base.SE.AFDM      = sd(adjusted_Percent.Change.in.AFDM, na.rm = TRUE) / sqrt(sum(!is.na(adjusted_Percent.Change.in.AFDM))),
    Base.Mean.Diatoms = mean(adjusted_Percent.Change.in.Diatoms, na.rm = TRUE),
    Base.SE.Diatoms   = sd(adjusted_Percent.Change.in.Diatoms, na.rm = TRUE) / sqrt(sum(!is.na(adjusted_Percent.Change.in.Diatoms))),
    .groups = "drop"
  )

Front.Back.Trial.Summary <- Front.Back.Trial.Summary %>%
  left_join(baseline, by = "Slope") %>%
  mutate(
    is.sed = Sediment.Type != none_label,
    Diff.Mean.AFDM    = if_else(is.sed, Mean.Trial.AFDM.Percent.Removal - Base.Mean.AFDM, NA_real_),
    Diff.SE.AFDM      = if_else(is.sed, sqrt(SE.Trial.AFDM^2 + Base.SE.AFDM^2), NA_real_),
    Diff.Mean.Diatoms = if_else(is.sed, Mean.Trial.Diatom.Percent.Removal - Base.Mean.Diatoms, NA_real_),
    Diff.SE.Diatoms   = if_else(is.sed, sqrt(SE.Trial.Diatoms^2 + Base.SE.Diatoms^2), NA_real_)
  ) %>%
  select(-is.sed, -starts_with("Base."))


Front.Back.Trial.Summary <- Front.Back.Trial.Summary %>% # assigning slope as a continuous var
  mutate(Slope.num = case_when(
    Slope == "Low"  ~ 3.1,
    Slope == "Medium" ~ 6.7,
    Slope == "High" ~ 8.9,
    TRUE            ~ NA_real_
  ))

# Evaluating quality 

Quality <- Quality %>%
  mutate(
    across(starts_with("X..Change"), ~ as.numeric(as.character(.)))
  )%>%
  rename(
    Trial = Trial..,   # old name = a → new name = new_a
    Percent.Change.Mat.Thickness = X..Change.Mat.Thickness,
    Percent.Change.in.Diatoms = X..Change.in.Diatom,
    Percent.Change.in.AFDM = X..Change.AFDM,
    Shear.Stress = Shear.Stress..Pa.,
    Shear.Velocity = Shear.Velocity..m.s.)


Quality$Trial <- factor(Quality$Trial)
Quality$Slope <- factor(Quality$Slope, levels = c("Low", "Medium", "High"))
Quality$Sediment.Type <- factor(Quality$Sediment.Type,
                                levels = c("None", "Sand ", "Gravel"))



# Trials on x--------------------------------------------------------------------------------------

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
  labs(x = "Shear",
       y = "% AFDM Removed",
       shape = "Sediment Type",
       #fill = "Sediment.Type",
       color = "Slope") +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme_classic()

ggplot(Front.Back.Trial.Summary, aes(x = Mean.Trial.Shear, # prettier version of above
                                     y = Mean.Trial.AFDM.Percent.Removal,
                                     color = Sediment.Type,
                                     fill = Sediment.Type,
                                     shape = Slope,
                                     group = Sediment.Type)) +
  geom_line() +
  geom_point(size = 5) +
  scale_shape_manual(values = c(21, 24, 22)) +   # circle, triangle, square
  labs(x = "Mean shear stress (Pa)",
       y = "% AFDM Removed",
       color = "Sediment Type",
       fill = "Sediment Type",
       shape = "Sediment Type") +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme_classic()


ggplot(Front.Back.Trial.Summary, # With error
       aes(x = Mean.Trial.Shear,
           y = Mean.Trial.AFDM.Percent.Removal,
           color = Sediment.Type,
           fill = Sediment.Type,
           shape = Slope,
           group = Sediment.Type)) +
  geom_line() +
  geom_errorbar(aes(ymin = Mean.Trial.AFDM.Percent.Removal - SE.Trial.AFDM,
                    ymax = Mean.Trial.AFDM.Percent.Removal + SE.Trial.AFDM),
                width = 0, linewidth = 0.6, show.legend = FALSE) +
  geom_point(size = 5) +
  scale_shape_manual(values = c(21, 24, 22)) +
  labs(x = "Mean shear stress (Pa)",
       y = "% AFDM Removed",
       color = "Sediment Type",
       fill = "Sediment Type",
       shape = "Slope") +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme_classic()


# A bit of QAQC for out "low quality" data
# All values
ggplot(Front.Back, aes(x = Shear.Stress,
                                     y = Percent.Change.in.AFDM,
                                     color = Slope,
                                     shape = Sediment.Type,
                                     fill = Sediment.Type)) +
  geom_point(size = 5)+
  coord_cartesian(ylim = c(-100, 250)) +
  labs(x = "Shear",
       y = "% AFDM Removed",
       shape = "Sediment Type",
       #fill = "Sediment.Type",
       color = "Slope") +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme_classic()

# only clean data
ggplot(Quality, aes(x = Shear.Stress,
                                     y = Percent.Change.in.AFDM,
                                     color = Slope,
                                     shape = Sediment.Type,
                                     fill = Sediment.Type)) +
  geom_point(size = 5)+
  labs(x = "Shear",
       y = "% AFDM Removed",
       shape = "Sediment Type",
       #fill = "Sediment.Type",
       color = "Slope") +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme_classic()

# Diatom-------------

ggplot(Front.Back.Trial.Summary, aes(x = Mean.Trial.Shear, 
                                     y = Mean.Trial.Diatom.Percent.Removal,
                                     color = Sediment.Type,
                                     fill = Sediment.Type,
                                     shape = Slope,
                                     group = Sediment.Type)) +
  geom_line() +
  geom_point(size = 5) +
  scale_shape_manual(values = c(21, 24, 22)) +   # circle, triangle, square
  labs(x = "Mean shear stress (Pa)",
       y = "% Diatom Removed",
       color = "Sediment Type",
       fill = "Sediment Type",
       shape = "Sediment Type") +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme_classic()

ggplot(Front.Back.Trial.Summary, # With error
       aes(x = Mean.Trial.Shear,
           y = Mean.Trial.Diatom.Percent.Removal,
           color = Sediment.Type,
           fill = Sediment.Type,
           shape = Slope,
           group = Sediment.Type)) +
  geom_line() +
  geom_errorbar(aes(ymin = Mean.Trial.Diatom.Percent.Removal - SE.Trial.Diatoms,
                    ymax = Mean.Trial.Diatom.Percent.Removal + SE.Trial.Diatoms),
                width = 0, linewidth = 0.6, show.legend = FALSE) +
  geom_point(size = 5) +
  scale_shape_manual(values = c(21, 24, 22)) +
  labs(x = "Mean shear stress (Pa)",
       y = "% Diatoms Removed",
       color = "Sediment Type",
       fill = "Sediment Type",
       shape = "Slope") +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme_classic()

# A bit of QAQC for out "low quality" data
# All values
ggplot(Front.Back, aes(x = Shear.Stress,
                       y = Percent.Change.in.Diatoms,
                       color = Slope,
                       shape = Sediment.Type,
                       fill = Sediment.Type)) +
  geom_point(size = 5)+
  coord_cartesian(ylim = c(-100, 0)) +
  labs(x = "Shear",
       y = "% Diatom Removed",
       shape = "Sediment Type",
       #fill = "Sediment.Type",
       color = "Slope") +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme_classic()

# only clean data
ggplot(Quality, aes(x = Shear.Stress,
                    y = Percent.Change.in.Diatoms,
                    color = Slope,
                    shape = Sediment.Type,
                    fill = Sediment.Type)) +
  geom_point(size = 5)+
  labs(x = "Shear",
       y = "% Diatom Removed",
       shape = "Sediment Type",
       #fill = "Sediment.Type",
       color = "Slope") +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme_classic()
# Sediment on X-----------------------------------------------------------------------------------

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


# With rock position

sed_cols <- c("None"   = "#66C2A5",   # Set2 green
              "Sand"   = "#FC8D62",   # Set2 orange
              "Gravel" = "#8DA0CB")   # Set2 purple

pos_alpha <- c("Front" = 0.8, "Back" = 0.3)

ggplot(Front.Back, aes(x = Sediment.Type,
                       y = adjusted_Percent.Change.in.AFDM,
                       fill = Sediment.Type,
                       color = Sediment.Type,
                       alpha = Position)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.7,
               outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15,
                                             dodge.width = 0.8),
             stroke = 0, size = 1.5, show.legend = FALSE) +
  facet_wrap(~ Slope, scales = "free_y") +
  labs(x = "Sediment", y = "% AFDM Removed",
       fill = "Sediment", color = "Sediment", alpha = "Rock position") +
  scale_fill_manual(values = sed_cols) +
  scale_color_manual(values = sed_cols) +
  scale_alpha_manual(values = pos_alpha) +
  guides(alpha = guide_legend(override.aes = list(fill = "grey40", color = "grey40")),
         fill = "none", color = "none") +
  theme_classic() +
  theme(legend.position = "top")


# % change Diatom boxplots
Front.Back <- Front.Back %>% # adding a column with z-scored shear stress 
  group_by(Rock) %>%
  mutate(
    Mean.Shear = mean(Shear.Stress)) %>%
  ungroup()

Diatoms.Rock <- Front.Back %>%
  filter(Position == "Front")    # use whichever label you want to keep

ggplot(Diatoms.Rock, aes(x = Sediment.Type, y = adjusted_Percent.Change.in.Diatoms, fill = Slope, color = Slope)) +
  geom_boxplot(position = position_dodge2(width = 0.8), alpha = 0.7, outlier.shape = 17) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              alpha = 0.5, size = 1.5) +
  facet_wrap(~ Slope, scales = "free_y") +
  labs(x = "Sediment", y = "% Diatom Removed", fill = "Slope", color = "Slope") +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "top")


# Slopes on X--------------------------------------------------------------------

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

ggplot(Front.Back, aes(x = Slope, # with rock position
                       y = adjusted_Percent.Change.in.AFDM,
                       fill = Position, color = Position)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.7,
               alpha = 0.7, outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15,
                                             dodge.width = 0.8),
             alpha = 0.5, size = 1.5, show.legend = FALSE) +
  facet_wrap(~ Sediment.Type, nrow = 1, scales = "free_y") +
  labs(x = "Slope", y = "% AFDM Removed",
       fill = "Rock position", color = "Rock position") +
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




# % change Diatom boxplots----
Front.Back <- Front.Back %>% # adding a column with z-scored shear stress 
  group_by(Rock) %>%
  mutate(
    Mean.Shear = mean(Shear.Stress)) %>%
  ungroup()

Diatoms.Rock <- Front.Back %>%
  filter(Position == "Front")    # use whichever label you want to keep

ggplot(Diatoms.Rock, aes(x = Slope, y = adjusted_Percent.Change.in.Diatoms, fill = Sediment.Type, color = Sediment.Type)) +
  geom_boxplot(position = position_dodge2(width = 0.8), alpha = 0.7, outlier.shape = NA) +
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
  labs(x = "% Slope", y = "% Diatoms Removed",
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





# Difference in removal compared to no sediment--------------------------------------

# AFDM removal
diff_dat <- Front.Back.Trial.Summary %>%
  filter(!is.na(Diff.Mean.AFDM))

sed_cols <- c("Sand"   = "#FC8D62",   # Set2 orange
              "Gravel" = "#8DA0CB")   # Set2 purple

slope_shapes <- c("Low"    = 21,   # circle
                  "Medium" = 24,   # triangle
                  "High"   = 22)   # square

ggplot(diff_dat, aes(x = Slope,
                     y = Diff.Mean.AFDM,
                     color = Sediment.Type,
                     shape = Slope,
                     fill = Sediment.Type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#66C2A5") +
  geom_errorbar(aes(ymin = Diff.Mean.AFDM - Diff.SE.AFDM,
                    ymax = Diff.Mean.AFDM + Diff.SE.AFDM),
                position = position_dodge(width = 0.5),
                width = 0, linewidth = 0.6, show.legend = FALSE) +
  geom_point(position = position_dodge(width = 0.5),
             size = 4, stroke = 0, alpha = 0.9) +
  labs(x = "Slope",
       y = "Difference in % AFDM removed\n(vs. no-sediment)",
       color = "Sediment Type",
       fill = "Sediment Type",
       shape = "Slope") +
  scale_fill_manual(values = sed_cols) +
  scale_color_manual(values = sed_cols) +
  scale_shape_manual(values = slope_shapes) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  theme_classic() +
  theme(legend.position = "top")


# Diatom removal

diff_dat <- Front.Back.Trial.Summary %>%
  filter(!is.na(Diff.Mean.Diatoms))


ggplot(diff_dat, aes(x = Slope,
                     y = Diff.Mean.Diatoms,
                     color = Sediment.Type,
                     shape = Slope,
                     fill = Sediment.Type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#66C2A5") +
  geom_errorbar(aes(ymin = Diff.Mean.Diatoms - Diff.SE.Diatoms,
                    ymax = Diff.Mean.Diatoms + Diff.SE.Diatoms),
                position = position_dodge(width = 0.5),
                width = 0, linewidth = 0.6, show.legend = FALSE) +
  geom_point(position = position_dodge(width = 0.5),
             size = 4, stroke = 0, alpha = 0.9) +
  labs(x = "Slope",
       y = "Difference in % Diatoms removed\n(vs. no-sediment)",
       color = "Sediment Type",
       fill = "Sediment Type",
       shape = "Slope") +
  scale_fill_manual(values = sed_cols) +
  scale_color_manual(values = sed_cols) +
  scale_shape_manual(values = slope_shapes) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  theme_classic() +
  theme(legend.position = "top")


