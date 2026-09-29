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
install.packages("ggpubr") 
library(ggpubr)
library(lme4)
library(lmerTest)   
library(emmeans)     
install.packages("multcompView")
library(multcompView)

# Set working directory 
setwd("~/Library/CloudStorage/OneDrive-TheUniversityofMontana/Flume experiment/Data/FlumeR")

# Flume data!
Floom <- read.csv("CSV.R.csv")

Floom <- Floom %>%
  mutate(
    across(starts_with("X..Change"), ~ as.numeric(as.character(.)))
  )%>%
  rename(
    Trial = Trial..,   # old name = a → new name = new_a
    Percent.Change.Low.Mat.Thickness = X..Change..in.Low.Impact..Mat.Thickness,
    Percent.Change.High.Mat.Thickness = X..Change..in.High.Impact..Mat.Thickness,
    Percent.Change.in.Diatoms = X..Change.in.Diatom,
    Percent.Change.in.Green = X..Change.in.Green,
    Percent.Change.in.Cyano = X..Change.in.Cyano,
    Percent.Change.in.High.Mat.AFDM = X..Change..in.High.Impact.AFDM,
    Percent.Change.in.Low.Mat.AFDM = X..Change..in.Low.Impact.AFDM)

str(Floom)


# Pivot percent change
# name this pct if you want to use code below which combines abs and pct into a different Floom_Long
Floom_Long <- Floom %>%
  pivot_longer(
    cols = starts_with("Percent.Change"),
    names_to = "variable_pct",
    values_to = "pct_change"
  )%>%
  select(Trial, Slope, Sediment.Type, Rock.ID,  variable_pct, pct_change,)  # keep extra columns


# Making % change absolute values
# This will lose information about the AFDM that is "added" after each trial 
Floom_Long <- Floom_Long %>%
  mutate(pct_change = abs(pct_change))


Floom_Long$Trial <- factor(Floom_Long$Trial)
Floom_Long$Slope <- factor(Floom_Long$Slope, levels = c("Low", "Medium", "High"))
Floom_Long$Sediment.Type <- factor(Floom_Long$Sediment.Type,
                                   levels = c("None", "Sand ", "Gravel"))
Floom_Long$variable_pct <- factor(Floom_Long$variable_pct,
                                   levels = c("Percent.Change.High.Mat.Thickness", "Percent.Change.Low.Mat.Thickness", 
                                              "Percent.Change.in.High.Mat.AFDM", "Percent.Change.in.Low.Mat.AFDM",
                                              "Percent.Change.in.Diatoms"))


# Trials on x--------------------------------

ggplot(Floom_Long, aes(x = Trial, y = pct_change, color = Slope)) +
  geom_boxplot(aes(fill = Sediment.Type),
               position = position_dodge(width = 0.8),
               alpha = 0.7,
               outlier.shape = NA) + # add this to avoid double dots if you want raw data displayed
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
             size = 1.5,
             alpha = 0.6) +
  facet_wrap(~ variable_pct, scales = "free_y") +
  labs(x = "Trial",
       y = "% Change",
       fill = "Sediment Type",
       color = "Slope") +
  theme_classic()

# Define comparisons (pairs of x-axis groups)
my_comparisons <- list(
  c("1", "2"),
  c("1", "3"),
  c("1", "4"),
  c("1", "7"),
  c("2", "3"),
  c("2", "5"),
  c("2", "8"),
  c("3", "6"),
  c("3", "9"),
  c("4", "5"),
  c("4", "6"),
  c("4", "7"),
  c("5", "6"),
  c("5", "8"),
  c("6", "9"),
  c("7", "8"),
  c("7", "9")
)

# Compute pairwise t-tests for % change-------------
res <- compare_means(
  pct_change ~ Trial,
  data = Floom_Long,
  group.by = "variable_pct",
  method = "t.test",
  p.adjust.method = "BH"
)

# Keep only significant results
sig_res <- res %>% filter(p < 0.05)

sig_res <- sig_res %>%
  rowwise() %>%
  filter(any(sapply(my_comparisons, function(x) all(sort(c(group1, group2)) == sort(x))))) %>%
  ungroup()

# Compute max pct_change per variable
y_pos <- Floom_Long %>%
  group_by(variable_pct) %>%
  summarise(y.position = max(pct_change, na.rm = TRUE) * 1.05, .groups = "drop")

# Join y-position to significant results
sig_res <- sig_res %>%
  left_join(y_pos, by = "variable_pct") %>%
  group_by(variable_pct) %>%
  # Stagger bars slightly to prevent overlap
  mutate(y.position = y.position + row_number() * 0.1 * y.position) %>%
  ungroup()

# Plot for % change
ggplot(Floom_Long, aes(x = factor(Trial), y = pct_change, fill = Sediment.Type)) +
  geom_boxplot(aes(color = Slope),
               position = position_dodge2(width = 0.8),
               alpha = 0.7,
               outlier.shape = NA) + 
  geom_jitter(aes(color = Slope),
              position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
              size = 1.5,
              alpha = 0.6) +
  facet_wrap(~ variable_pct, scales = "free_y") +
  labs(x = "Trial",
       y = "% Change",
       fill = "Sediment Type",
       color = "Slope") +
  stat_pvalue_manual(
    sig_res,
    label = "p.signif",
    y.position = "y.position",   # your computed starting y
    #step.increase = 0.05 * max(Floom_Long$pct_change, na.rm = TRUE)
  ) +
  theme_classic()


# Slopes on X-----------------------------
Floom_Long <- Floom_Long %>%
  filter(variable_pct != "Percent.Change.in.Green") %>%
  filter(variable_pct != "Percent.Change.in.Cyano") %>%
  filter(!(variable_pct == "Percent.Change.in.Low.Mat.AFDM" & pct_change > 1000)) # Target the specific outlier

# % change
ggplot(Floom_Long, aes(x = Slope, y = pct_change, fill = Sediment.Type, color = Sediment.Type)) +
  geom_boxplot(position = position_dodge2(width = 0.8), alpha = 0.7, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
              size = 1.5,
              alpha = 0.6) +
  facet_wrap(~ variable_pct, nrow = 3, ncol = 2, scales = "free_y") +
  labs(x = "Slope", y = "% Change", fill = "Sediment Type", color = "Sediment Type") +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "top")

my_comparisons <- list(
  c("Low", "Medium"),
  c("Medium", "High"),
  c("Low", "High"))


# Compute pairwise t-tests
res <- compare_means(
  pct_change ~ Slope,
  data = Floom_Long,
  group.by = "variable_pct",
  method = "t.test",
  p.adjust.method = "BH"
)

# Keep only significant results
sig_res <- res %>% filter(p < 0.05)


# Max pct_change per variable from full dataset
y_pos <- Floom_Long %>%
  group_by(variable_pct) %>%
  summarise(y.position = max(pct_change, na.rm = TRUE) * 1.05, .groups = "drop")


# Join y-position to significant results
sig_res <- sig_res %>%
  left_join(y_pos, by = "variable_pct") %>%
  group_by(variable_pct) %>%
  # Stagger bars slightly to prevent overlap
  mutate(y.position = y.position + row_number() * 0.05 * y.position) %>%
  ungroup()

# % change Plot

ggplot(Floom_Long, aes(x = Slope, y = pct_change, fill = Sediment.Type, colour = Sediment.Type)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Sediment.Type),
              position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
              size = 1.5,
              alpha = 0.6) +
  facet_wrap(~ variable_pct, scales = "free_y") +
  labs(x = "Slope", y = "% Change", fill = "Sediment Type", color = "Sediment Type") +
  stat_pvalue_manual(
    sig_res,
    label = "p.signif"
  ) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "top")



# Sediment on X-----------------------------

# % change

ggplot(Floom_Long, aes(x = Sediment.Type, y = pct_change, fill = Slope, color = Slope)) +
  geom_boxplot(position = position_dodge2(width = 0.8), alpha = 0.7, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              alpha = 0.5, size = 1.5) +
  facet_wrap(~ variable_pct, scales = "free_y") +
  labs(x = "Sediment", y = "% Change", fill = "Slope", color = "Slope") +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "top")


# Compute pairwise t-tests for %
res <- compare_means(
  pct_change ~ Sediment.Type,
  data = Floom_Long,
  group.by = "variable_pct",
  method = "t.test",
  p.adjust.method = "BH"
)

# Keep only significant results
sig_res <- res %>% filter(p < 0.05)

# Max pct_change per variable from full dataset
y_pos <- Floom_Long %>%
  group_by(variable_pct) %>%
  summarise(y.position = max(pct_change, na.rm = TRUE) * 1.05, .groups = "drop")

# Join y-position to significant results
sig_res <- sig_res %>%
  left_join(y_pos, by = "variable_pct") %>%
  group_by(variable_pct) %>%
  # Stagger bars slightly to prevent overlap
  mutate(y.position = y.position + row_number() * 0.05 * y.position) %>%
  ungroup()


# Plot

ggplot(Floom_Long, aes(x = Sediment.Type, y = pct_change, fill = Slope, color = Slope)) +
  geom_boxplot(position = position_dodge2(width = 0.8), alpha = 0.7, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              alpha = 0.5, size = 1.5) +
  facet_wrap(~ variable_pct, scales = "free_y") +
  labs(x = "Sediment.Type",
       y = "% Change",
       fill = "Slope") +
  stat_pvalue_manual(
    sig_res,
    label = "p.signif"
  ) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "top")

