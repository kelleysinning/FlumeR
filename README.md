These are all the relevant CSVs and scripts used for the Didymo Flume Experiment for Chapter 1 of Kelley Sinning's dissertation, in collaboration with the Colorado State University Hyrdraulics Lab.

Front.Back_GoodQuality.csv: Only "good" quality shear stress and shear velocity data that fit the logarithmic relationship
Front.Back.csv: Includes all, even poor quality shear stress and shear velocity estimates, data for front of rock vs back of rock analysis
CSV.R.csv: rock IDs are not separated by rock position, so there is one row for each rock as opposed to Front.Back.csv. This is used for the diatoms only model, since % change in diatoms wasn't calculated at the rock position scale, but at the rock scale

Floom-Algae Only.R: preliminary figures of removal in diatoms, mat, and AFDM, not including any inclusion of shear stress or shear velocity
Flume-biplots.R: Plots on plots, incorporating shear stress and velocity into biological variables NOT USING JAGS
Flume-Model.R: Script for running jags models including ALL data
      jags model diatoms.R: Model for diatom removal, doesn't include rock position and averages front-back shear stress and shear velocity
      jags model front back.R: Model for the front-back relationships of AFDM and mat removal, includes rock position 

sim.R and jags model.R: Simulated data and code that Yoichiro developed to help us prior to having results
