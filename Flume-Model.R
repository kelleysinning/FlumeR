# Running jags model for Flume Experiment for Ch.1 of Kelley Sinning Dissertation
# Fall 2026

library(tidyr)
library(dplyr)
library(ggplot2)
install.packages("jagsUI")
library(jagsUI)
install.packages("MCMCvis")
library(MCMCvis)


setwd("~/Library/CloudStorage/OneDrive-TheUniversityofMontana/Flume experiment/Data/FlumeR")

Floom <- read.csv("CSV.R.csv")
Front.Back <- read.csv("Front.Back.csv")

# Cleaning dfs up
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


Front.Back$Trial <- factor(Front.Back$Trial)
Front.Back$Slope <- factor(Front.Back$Slope, levels = c("Low", "Medium", "High"))
Front.Back$Sediment.Type <- factor(Front.Back$Sediment.Type,
                              levels = c("None", "Sand", "Gravel"))
Front.Back$Position <- factor(Front.Back$Position, levels = c("Front", "Back"))


names(Front.Back)


# FRONT BACK MODEL---------------------------------------------------------
# Choose the response: Percent.Change.in.AFDM or Percent.Change.Mat.Thickness
resp <- "Percent.Change.Mat.Thickness"

dat <- Front.Back %>%
  mutate(
    y_raw = pmin(pmax(-.data[[resp]], 0), 100) / 100, # Proportion REDUCTION; increases (positive % change) capped at 0
    y = (y_raw * (n() - 1) + 0.5) / n() # Squeeze off 0 and 1 for the beta likelihood
  ) %>%
  group_by(Trial) %>%
  mutate(hydraulic = as.numeric(scale(log(Shear.Stress)))) %>%   # standardized within trial
  ungroup() %>%
  filter(!is.na(y), !is.na(hydraulic)) %>% # Removes NAs of shear stress
  mutate(rock_idx = as.integer(factor(Rock)))

table(dat$Position)   # check front/back rows survived
table(dat$Trial)                 # should be roughly 9-10 rows per trial
sum(Front.Back$Percent.Change.in.AFDM > 0, na.rm = TRUE)   # how many increases got capped at 0



# Run Beta Regression Model in JAGS---------------

# Data
jags.data <- list(
  N = nrow(dat), nRock = max(dat$rock_idx),
  rock = dat$rock_idx,
  y = dat$y,
  slopeMedInd  = as.numeric(dat$Slope == "Medium"),
  slopeHighInd = as.numeric(dat$Slope == "High"), # this an above captures between trial shear effect
  SandInd      = as.numeric(dat$Sediment.Type == "Sand"),
  GravelInd    = as.numeric(dat$Sediment.Type == "Gravel"),
  #hydraulic    = dat$hydraulic, # only sees the leftover variation among rocks within the same trial?
  FrontInd     = as.numeric(dat$Position == "Front")
)


params <- c("gamma0","gamma1","gamma2","gamma3","gamma4","gamma5",#"gamma6",
            "sigma.rock","phi")

inits <- function(){
  list(gamma0 = rnorm(1,0,1), gamma1 = rnorm(1,0,1), gamma2 = rnorm(1,0,1), gamma3 = rnorm(1,0,1), gamma4 = rnorm(1,0,1), 
       gamma5 = rnorm(1,0,1), #gamma6 = rnorm(1,0,1),
       sigma.rock = runif(1, 0.1, 1), phi = rgamma(1,1,1))
}

out <- jags(data = jags.data, inits = inits, parameters.to.save = params,
            model.file = "jags model front back.R",
            n.chains = 3, n.iter = 6000, n.burnin = 1000, n.thin = 5, parallel = TRUE)
print(out)       # check Rhat < 1.1 and decent n.eff
plot(out)

MCMCplot(out,
         params = c("gamma0", "gamma1","gamma2","gamma3","gamma4","gamma5"),#,"gamma6"),
         ISB = TRUE, exact = TRUE, main = "",  ci = c(85, 95),
         xlab = "Parameter Estimate (logit scale)",
         col = c("black","#009E73","#009E73","#56B4E9","#56B4E9","#CC79A7"),#,"#E69F00"),
         labels = c("Intercept (low slope & no subst)","Medium slope (vs Low)", "High slope (vs Low)",
                    "Sand (vs None)", "Gravel (vs None)",
                    #"Hydraulic (log shear stress)", 
                    "Front (vs Back)"),
         guide_lines = TRUE)

# T-TESTS------------------------------
# Extract Front and Back into separate vectors
front_data <- Front.Back$Shear.Stress[Front.Back$Position == "Front"]
back_data  <- Front.Back$Shear.Stress[Front.Back$Position == "Back"]
# Run the paired t-test
t.test(front_data, back_data, paired = TRUE)
# shear stress is significantly different between front and back for paired 

# ODDS RATIO-----------------------------
exp(out$mean$gamma1) # percent change in AFDM is 1.8x more likely to occur in medium slopes than low slopes
exp(out$mean$gamma2) # percent change in AFDM is 1.7x more likely to occur in high slopes than low slopes
exp(out$mean$gamma3) # percent change in AFDM is 0.63x more likely to occur in sand treatments than no sediment treatments
exp(out$mean$gamma4) # percent change in AFDM is 1.1x more likely to occur in gravel treatments than no sediment treatments
exp(out$mean$gamma5) # percent change in AFDM is 2.25x more likely to occur on front of rock than back of rock

exp(out$mean$gamma1) # percent change in mat thickness is 1.5x more likely to occur in medium slopes than low slopes
exp(out$mean$gamma2) # percent change in mat thickness is 3.2x more likely to occur in high slopes than low slopes -- > 0 
exp(out$mean$gamma3) # percent change in mat thickness is 0.5x more likely to occur in sand treatments than no sediment treatments
exp(out$mean$gamma4) # percent change in mat thickness is 1.3x more likely to occur in gravel treatments than no sediment treatments
exp(out$mean$gamma5) # percent change in mat thickness is 2x more likely to occur on front of rock than back of rock -- > 0 

# MORE PLOTS---------------------------
dat$removal <- dat$y_raw * 100

ggplot(dat, aes(hydraulic, removal)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_log10() +
  labs(x = "Shear stress (Pa, log scale)", y = "AFDM removal") +
  theme_bw()

ggplot(dat, aes(hydraulic, removal, color = Position)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_log10() +
  labs(x = "Shear stress (Pa, log scale)", y = "AFDM removal") +
  theme_bw()

ggplot(dat, aes(hydraulic, removal, color = Slope)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_log10() +
  labs(x = "Shear stress (Pa, log scale)", y = "AFDM removal") +
  theme_bw()

ggplot(dat, aes(hydraulic, removal, color = Sediment.Type)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_log10() +
  labs(x = "Shear stress (Pa, log scale)", y = "AFDM removal") +
  theme_bw()

ggplot(dat, aes(hydraulic, removal, color = Position)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  facet_wrap(Slope ~ Sediment.Type) +
  labs(x = "Shear stress (Pa, log scale)", y = "AFDM removal") +
  theme_bw()

ggplot(dat, aes(hydraulic, removal, group = Rock)) +
  geom_line(color = "grey60") +
  geom_point(aes(color = Position), size = 2.5) +
  scale_x_log10() +
  facet_wrap(Slope ~ Sediment.Type) +
  labs(x = "Shear stress (Pa, log scale)", y = "AFDM removal") +
  theme_bw() # Much more similar removal at gravel and high slope

ggplot(dat, aes(x = hydraulic, y = removal)) + 
  geom_point() +
  facet_grid(Slope ~ Sediment.Type) +
  xlab("Hydraulic Metric") +
  ylab("Percent Reduction in Didymo") +
  theme_bw()





# DIATOM MODEL-----------------------------------------------------------------
# Choose the response: Percent.Change.in.AFDM or Percent.Change.Mat.Thickness
resp <- "Percent.Change.in.Diatoms"

# Cleaning this dataset for the purposes of this model
Floom <- Floom %>%
  mutate(Avg.Shear.Stress = (Back.Shear.Stress + Front.Shear.Stress)/2) %>%
  mutate(Avg.Shear.Velocity = (Back.Shear.Velocity + Front.Shear.Velocity)/2) %>%
  select(-Percent.Change.in.Cyano, -Percent.Change.in.Green, -Percent.Change.in.High.Mat.AFDM,
       -Percent.Change.in.Low.Mat.AFDM, -Percent.Change.High.Mat.Thickness, -Percent.Change.Low.Mat.Thickness,
       -Quality.B, -Quality.F, -Back.Shear.Stress, -Front.Shear.Stress, -Back.Shear.Velocity, -Front.Shear.Velocity) 

dat <- Floom %>%
  mutate(
    y_raw = pmin(pmax(-.data[[resp]], 0), 100) / 100, # Proportion REDUCTION; increases (positive % change) capped at 0
    y = (y_raw * (n() - 1) + 0.5) / n() # Squeeze off 0 and 1 for the beta likelihood
  ) %>%
  group_by(Trial) %>%
  mutate(hydraulic = as.numeric(scale(log(Avg.Shear.Stress)))) %>%   # standardized within trial
  ungroup() %>%
  filter(!is.na(y), !is.na(hydraulic)) %>% # Removes NAs of shear stress
  mutate(rock_idx = as.integer(factor(Rock.ID)))

table(dat$Trial)                 # should be roughly 4-5 rows (i.e., rocks) per trial 
sum(Floom$Percent.Change.in.AFDM > 0, na.rm = TRUE)   # how many increases got capped at 0



# Run Beta Regression Model in JAGS---------------

# Data
jags.data <- list(
  N = nrow(dat),
  y = dat$y,
  slopeMedInd  = as.numeric(dat$Slope == "Medium"),
  slopeHighInd = as.numeric(dat$Slope == "High"), # this an above captures between trial shear effect
  SandInd      = as.numeric(dat$Sediment.Type == "Sand"),
  GravelInd    = as.numeric(dat$Sediment.Type == "Gravel")
)


params <- c("gamma0","gamma1","gamma2","gamma3","gamma4","phi")

inits <- function(){
  list(gamma0 = rnorm(1,0,1), gamma1 = rnorm(1,0,1), gamma2 = rnorm(1,0,1), gamma3 = rnorm(1,0,1), gamma4 = rnorm(1,0,1), 
 phi = phi = rgamma(1,1,1))
}

out <- jags(data = jags.data, inits = inits, parameters.to.save = params,
            model.file = "jags model diatoms.R",
            n.chains = 3, n.iter = 6000, n.burnin = 1000, n.thin = 5, parallel = TRUE)
print(out)       # check Rhat < 1.1 and decent n.eff
plot(out)

MCMCplot(out,
         params = c("gamma0", "gamma1","gamma2","gamma3","gamma4"),
         ISB = TRUE, exact = TRUE, main = "",  ci = c(85, 95),
         xlab = "Parameter Estimate (logit scale)",
         col = c("black","#009E73","#009E73","#56B4E9","#56B4E9"),
         labels = c("Intercept (low slope & no subst)","Medium slope (vs Low)", "High slope (vs Low)",
                    "Sand (vs None)", "Gravel (vs None)"),
         guide_lines = TRUE)

# ODDS RATIO-----------------------------
exp(out$mean$gamma1) # percent change in diatoms is 0.88x more likely to occur in medium slopes than low slopes
exp(out$mean$gamma2) # percent change in diatoms is 1.33x more likely to occur in high slopes than low slopes
exp(out$mean$gamma3) # percent change in diatoms is 0.93x more likely to occur in sand treatments than no sediment treatments
exp(out$mean$gamma4) # percent change in diatoms is 1.3x more likely to occur in gravel treatments than no sediment treatments

