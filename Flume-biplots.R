# Incorporating shear stress and velocity into biological variables for Flume analysis
# Chapter 1 of Kelley Sinning's Dissertation
# Fall 2026


#load important packages#
library(ggplot2)
library(gridExtra)
library(viridis)
library(ggthemes)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(rcartocolor)

setwd("~/Library/CloudStorage/OneDrive-TheUniversityofMontana/Flume experiment/Data/FlumeR")

Floom <- read.csv("CSV.R.csv")

# Organizing
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
    Percent.Change.in.Low.Mat.AFDM = X..Change..in.Low.Impact.AFDM,
    Front.Shear.Stress = Shear.Stress.Front..Pa.,
    Back.Shear.Stress = Shear.Stress.Back..Pa.,
    Front.Shear.Velocity = Shear.Velocity.Front..m.s.,
    Back.Shear.Velocity = Shear.Velocity.Back..m.s.)

Floom$Trial <- factor(Floom$Trial)
Floom$Slope <- factor(Floom$Slope, levels = c("Low", "Medium", "High"))
Floom$Sediment.Type <- factor(Floom$Sediment.Type,
                                   levels = c("None", "Sand ", "Gravel"))


# PLOTS ON PLOTS

## SHEAR STRESS x AFDM ##
# How does shear stress on the front of rock remove AFDM on front (i.e., high impact section) of rock?
ggplot(Floom, aes(x = Front.Shear.Stress, 
                  y = Percent.Change.in.High.Mat.AFDM, 
                  color = Slope, 
                  linetype = Sediment.Type)) +
  geom_point() +  # Optional: Adds the raw data points
  geom_smooth(method = "lm", se = FALSE)+  # Draws the distinct trend lines/slopes
  labs(x = "Front of Rock Shear Stress (m/s)",
       y = "% Change in Front of Rock AFDM",
       fill = "Sediment Type",
       color = "Slope") +
  facet_wrap(~ Sediment.Type) +
  theme_classic() # this is real ugly

ggplot(Floom, aes(x = Front.Shear.Stress, 
                  y = Percent.Change.in.High.Mat.AFDM, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  coord_cartesian(xlim = c(0, 75)) +  # Zooms in to hide the empty space up to 200
  theme_bw()


# How does shear stress on the back of rock remove AFDM on back (i.e., low impact section) of rock?
ggplot(Floom, aes(x = Back.Shear.Stress, 
                  y = Percent.Change.in.Low.Mat.AFDM, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  coord_cartesian(xlim = c(0, 80), ylim = c(-100,260)) +  # Zooms in to hide the empty space up to 200
  theme_bw()

## SHEAR VELOCITY x AFDM ##
# How does shear velocity on the front of rock remove AFDM on front (i.e., high impact section) of rock?
ggplot(Floom, aes(x = Front.Shear.Velocity, 
                  y = Percent.Change.in.High.Mat.AFDM, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  coord_cartesian(xlim = c(0, 0.50)) +  # Zooms in to hide the empty space up to 200
  theme_bw()


# How does shear velocity on the back of rock remove AFDM on back (i.e., low impact section) of rock?
ggplot(Floom, aes(x = Back.Shear.Velocity, 
                  y = Percent.Change.in.Low.Mat.AFDM, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  coord_cartesian(xlim = c(0, 0.50), ylim = c(-100,260)) +  # Zooms in to hide the empty space up to 200
  theme_bw()



## SHEAR STRESS x MAT THICKESS ##
# How does shear stress on the front of rock remove mat thickness on front (i.e., high impact section) of rock?

ggplot(Floom, aes(x = Front.Shear.Stress, 
                  y = Percent.Change.High.Mat.Thickness, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  theme_bw()


# How does shear stress on the back of rock remove mat thickness on back (i.e., low impact section) of rock?
ggplot(Floom, aes(x = Back.Shear.Stress, 
                  y = Percent.Change.Low.Mat.Thickness, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  theme_bw()


## SHEAR VELOCITY x MAT THICKESS ##
# How does shear velocity on the front of rock remove mat thickness on front (i.e., high impact section) of rock?

ggplot(Floom, aes(x = Front.Shear.Velocity, 
                  y = Percent.Change.High.Mat.Thickness, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  theme_bw()


# How does shear velocity on the back of rock remove mat thickness on back (i.e., low impact section) of rock?
ggplot(Floom, aes(x = Back.Shear.Velocity, 
                  y = Percent.Change.Low.Mat.Thickness, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  theme_bw()



## SHEAR STRESS x DIATOMS ##
# How does shear stress on the front of rock remove diatoms across rock?

ggplot(Floom, aes(x = Front.Shear.Stress, 
                  y = Percent.Change.in.Diatoms, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  theme_bw()


# How does shear stress on the back of rock remove diatoms across rock?
ggplot(Floom, aes(x = Back.Shear.Stress, 
                  y = Percent.Change.in.Diatoms, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  theme_bw()



## SHEAR VELOCITY x DIATOMS ##
# How does shear velocity on the front of rock remove diatoms across rock?

ggplot(Floom, aes(x = Front.Shear.Velocity, 
                  y = Percent.Change.in.Diatoms, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  theme_bw()


# How does shear stress on the back of rock remove diatoms across rock?
ggplot(Floom, aes(x = Back.Shear.Velocity, 
                  y = Percent.Change.in.Diatoms, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  theme_bw()
