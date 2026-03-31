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

# Set working directory 
setwd("~/Library/CloudStorage/OneDrive-Colostate/Flume experiment/Data/FlumeR")

# Flume data!
Floom <- read.csv("FlumeData.csv")

Floom <- Floom %>%
  rename(
    Trial = Trial.....details,   # old name = a → new name = new_a
    Percent.Change.Low.Mat.Thickness = Percent.Change..in.Low.Impact..Mat.Thickness,
    Percent.Change.High.Mat.Thickness = Percent.Change..in.High.Impact..Mat.Thickness)


Floom_Long <- Floom %>%
  pivot_longer(cols = starts_with("Percent.Change"),
               names_to = "variable",
               values_to = "pct_change") 



ggplot(Floom_Long, aes(x = Trial, y = pct_change)) +
  geom_boxplot(aes(fill = Sediment.Type, color = Slope),
               position = position_dodge2(width = 0.8),
               alpha = 0.7) +
  geom_jitter(aes(color = Slope),
              width = 0.2,
            alpha = 0.5,
              size = 1.5) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = "Trial",
       y = "% Change",
       fill = "Sediment Type",
       color = "Slope") +
  theme_classic()

# Define comparisons (pairs of x-axis groups)
my_comparisons <- list(
  c("1: high-slope, no sed", "2: mid-slope, no sed"),
  c("1: high-slope, no sed", "3: low-slope, no sed"),
  c("1: high-slope, no sed", "4: high-slope, sand"),
  c("2: mid-slope, no sed", "3: low-slope, no sed"),
  c("2: mid-slope, no sed","4: high-slope, sand"),
  c("3: low-slope, no sed", "4: high-slope, sand")
)

ggplot(Floom_Long, aes(x = Trial, y = pct_change, fill = Sediment.Type)) +
  geom_boxplot(aes(color = Slope),
               position = position_dodge2(width = 0.8),
               alpha = 0.7) +
  geom_jitter(aes(color = Slope),
              width = 0.2,
              alpha = 0.5,
              size = 1.5) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = "Trial",
       y = "% Change",
       fill = "Sediment Type",
       color = "Slope") +
  stat_compare_means(
    comparisons = my_comparisons,
    method = "t.test",
    label = "p.signif",
    position = 1.05,  # slightly above boxplots
    hide.ns = TRUE    # hides non-significant labels
  ) +
  theme_classic()


res <- compare_means(
  pct_change ~ Trial,
  data = Floom_Long,
  group.by = "variable",
  method = "t.test",
  p.adjust.method = "BH"
)
sig_res <- res %>% filter(p.adj < 0.05)

ggplot(Floom_Long, aes(x = Trial, y = pct_change, fill = Sediment.Type)) +
  geom_boxplot(aes(color = Slope),
               position = position_dodge2(width = 0.8),
               alpha = 0.7) +
  geom_jitter(aes(color = Slope),
              width = 0.2,
              alpha = 0.5,
              size = 1.5) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = "Trial",
       y = "% Change",
       fill = "Sediment Type",
       color = "Slope") +
  stat_pvalue_manual(
    sig_res,
    label = "p.signif",
    y.position = max(Floom_filtered$pct_change, na.rm = TRUE) 
  ) +
  theme_classic()



# Compute pairwise t-tests
res <- compare_means(
  pct_change ~ Trial,
  data = Floom_Long,
  group.by = "variable",
  method = "t.test",
  p.adjust.method = "BH"
)

# Keep only significant results
sig_res <- res %>% filter(p < 0.05)

# Compute y-position per facet using Floom_Long
sig_res <- sig_res %>%
  group_by(variable) %>%
  mutate(y.position = max(Floom_Long$pct_change[Floom_Long$variable == variable], na.rm = TRUE) * 1.05) %>%
  ungroup()


# Plot
ggplot(Floom_Long, aes(x = Trial, y = pct_change, fill = Sediment.Type)) +
  geom_boxplot(aes(color = Slope),
               position = position_dodge2(width = 0.8),
               alpha = 0.7) +
  geom_jitter(aes(color = Slope),
              width = 0.2,
              alpha = 0.5,
              size = 1.5) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = "Trial",
       y = "% Change",
       fill = "Sediment Type",
       color = "Slope") +
  stat_pvalue_manual(
    sig_res,
    label = "p.signif"
  ) +
  theme_classic()
