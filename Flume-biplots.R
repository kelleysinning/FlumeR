# Incorporating shear stress and velocity into biological variables for Flume analysis
# Chapter 1 of Kelley Sinning's Dissertation
# Fall 2026


#load important packages#
library(ggplot2)
library(patchwork)
install.packages("patchwork")
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

plot1 <- ggplot(Floom, aes(x = Front.Shear.Stress, 
                  y = Percent.Change.in.High.Mat.AFDM, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  coord_cartesian(xlim = c(0, 75)) +  # Zooms in to hide the empty space up to 200
  theme_bw()+
  theme(legend.position = "none")


# How does shear stress on the back of rock remove AFDM on back (i.e., low impact section) of rock?
plot2 <- ggplot(Floom, aes(x = Back.Shear.Stress, 
                  y = Percent.Change.in.Low.Mat.AFDM, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  coord_cartesian(xlim = c(0, 80), ylim = c(-100,260)) +  # Zooms in to hide the empty space up to 200
  theme_bw()


plot1 + plot2

## SHEAR VELOCITY x AFDM ##
# How does shear velocity on the front of rock remove AFDM on front (i.e., high impact section) of rock?
plot3 <- ggplot(Floom, aes(x = Front.Shear.Velocity, 
                  y = Percent.Change.in.High.Mat.AFDM, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  coord_cartesian(xlim = c(0, 0.50)) +  # Zooms in to hide the empty space up to 200
  theme_bw()+
  theme(legend.position = "none")


# How does shear velocity on the back of rock remove AFDM on back (i.e., low impact section) of rock?
plot4 <- ggplot(Floom, aes(x = Back.Shear.Velocity, 
                  y = Percent.Change.in.Low.Mat.AFDM, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  coord_cartesian(xlim = c(0, 0.50), ylim = c(-100,260)) +  # Zooms in to hide the empty space up to 200
  theme_bw()


plot3 + plot4
## SHEAR STRESS x MAT THICKESS ##
# How does shear stress on the front of rock remove mat thickness on front (i.e., high impact section) of rock?

plot5 <- ggplot(Floom, aes(x = Front.Shear.Stress, 
                  y = Percent.Change.High.Mat.Thickness, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  theme_bw()+
  theme(legend.position = "none")


# How does shear stress on the back of rock remove mat thickness on back (i.e., low impact section) of rock?
plot6 <- ggplot(Floom, aes(x = Back.Shear.Stress, 
                  y = Percent.Change.Low.Mat.Thickness, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  theme_bw()

plot5 + plot6

## SHEAR VELOCITY x MAT THICKESS ##
# How does shear velocity on the front of rock remove mat thickness on front (i.e., high impact section) of rock?

plot7 <- ggplot(Floom, aes(x = Front.Shear.Velocity, 
                  y = Percent.Change.High.Mat.Thickness, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  theme_bw()+
  theme(legend.position = "none")


# How does shear velocity on the back of rock remove mat thickness on back (i.e., low impact section) of rock?
plot8 <- ggplot(Floom, aes(x = Back.Shear.Velocity, 
                  y = Percent.Change.Low.Mat.Thickness, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  theme_bw()


plot7 + plot8

## SHEAR STRESS x DIATOMS ##
# How does shear stress on the front of rock remove diatoms across rock?

plot9 <- ggplot(Floom, aes(x = Front.Shear.Stress, 
                  y = Percent.Change.in.Diatoms, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  theme_bw()+
  theme(legend.position = "none")


# How does shear stress on the back of rock remove diatoms across rock?
plot10 <- ggplot(Floom, aes(x = Back.Shear.Stress, 
                  y = Percent.Change.in.Diatoms, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  theme_bw()


plot9 + plot10

## SHEAR VELOCITY x DIATOMS ##
# How does shear velocity on the front of rock remove diatoms across rock?

plot11 <- ggplot(Floom, aes(x = Front.Shear.Velocity, 
                  y = Percent.Change.in.Diatoms, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  theme_bw()+
  theme(legend.position = "none")


# How does shear stress on the back of rock remove diatoms across rock?
plot12 <- ggplot(Floom, aes(x = Back.Shear.Velocity, 
                  y = Percent.Change.in.Diatoms, 
                  color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ Sediment.Type) +
  theme_bw()

plot11 + plot12

plot1 + plot2 + plot3 + plot4 + plot5 + plot6 + plot7 + plot8 + plot9 + plot10 + plot11 + plot12
