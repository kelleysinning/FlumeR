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

Front.Back <- Front.Back %>% # adding a column with z-scored shear stress 
  group_by(Trial, Position) %>%
  mutate(z_Shear = as.numeric(scale(Shear.Stress))) %>%
  ungroup()
         
Front.Back$Trial <- factor(Front.Back$Trial)
Front.Back$Slope <- factor(Front.Back$Slope, levels = c("Low", "Medium", "High"))
Front.Back$Sediment.Type <- factor(Front.Back$Sediment.Type,
                              levels = c("None", "Sand", "Gravel"))
Front.Back$Position <- factor(Front.Back$Position, levels = c("Front", "Back"))


names(Front.Back)


# FRONT BACK MODEL---------------------------------------------------------
# RESPONSE IS PERCENT CHANGE IN AFDM
resp <- "Percent.Change.in.AFDM"

dat <- Front.Back %>%
  mutate(
    y_raw = pmin(pmax(-.data[[resp]], 0), 100) / 100, # Proportion REDUCTION; increases (positive % change) capped at 0
    y = (y_raw * (n() - 1) + 0.5) / n() # Squeeze off 0 and 1 for the beta likelihood
  ) %>%
  filter(!is.na(y), !is.na(z_Shear)) %>% # Removes NAs of shear stress
  mutate(rock_idx = as.integer(factor(Rock)))

table(dat$Position)   # check front/back rows survived
table(dat$Trial)                 # should be roughly 9-10 rows per trial
sum(Front.Back$Percent.Change.in.AFDM > 0, na.rm = TRUE)   # how many increases got capped at 0


str(dat)
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
  Shear       = dat$z_Shear, # shear stress as continuous covariate,after accounting for the trial specific indicators, effect of shear stress conditional on slope, substrate,etc. can residual be explained by shear stress
  # could include shear stress in model in leiu of front and back position
  # is difference in shear stress explanatory of difference in front of rocks?--> I don't think so, it sits around zero so I think z-standardized shear is telling us the slope is mostly capturing shear stress effect?
  
   FrontInd     = as.numeric(dat$Position == "Front")
)


# will still have to include raw data in a figure for shear stress as opposed to z scores
# this could be a 3 way anova


params <- c("gamma0","gamma1","gamma2","gamma3","gamma4","gamma5", "gamma6",
            "sigma.rock","phi")

inits <- function(){
  list(gamma0 = rnorm(1,0,1), gamma1 = rnorm(1,0,1), gamma2 = rnorm(1,0,1), gamma3 = rnorm(1,0,1), gamma4 = rnorm(1,0,1), 
       gamma5 = rnorm(1,0,1), gamma6 = rnorm(1,0,1),
       sigma.rock = runif(1, 0.1, 1), phi = rgamma(1,1,1))
}

out <- jags(data = jags.data, inits = inits, parameters.to.save = params,
            model.file = "jags model front back.R",
            n.chains = 3, n.iter = 6000, n.burnin = 1000, n.thin = 5, parallel = TRUE)
print(out)       # check Rhat < 1.1 and decent n.eff
plot(out)

MCMCplot(out,
         params = c("gamma0", "gamma1","gamma2","gamma3","gamma4","gamma5", "gamma6"),
         ISB = TRUE, exact = TRUE, main = "",  ci = c(85, 95),
         xlab = "Parameter Estimate (logit scale)",
         col = c("black","#009E73","#009E73","#56B4E9","#56B4E9","#CC79A7", "#E69F00"),
         labels = c("Intercept (low slope & no subst)","Medium slope (vs Low)", "High slope (vs Low)",
                    "Sand (vs None)", "Gravel (vs None)", "Z-shear stress",
         "Front (vs Back)"),
         guide_lines = TRUE)

# Smooth scatter Plot
library(dplyr); library(ggplot2); library(reshape2)

post <- out$sims.list
shear_mean <- mean(Front.Back$Shear.Stress, na.rm = TRUE)  # same values used for z_Shear
shear_sd   <- sd(Front.Back$Shear.Stress,   na.rm = TRUE)

line_fun <- function(slope_level, sed_level) {
  d <- dat %>% filter(Slope == slope_level, Sediment.Type == sed_level)
  if (nrow(d) == 0) return(NULL)
  z_seq <- seq(min(d$z_Shear), max(d$z_Shear), length.out = 50)
  
  eta <- sapply(z_seq, function(z)
    post$gamma0 +
      post$gamma1 * (slope_level == "Medium") +
      post$gamma2 * (slope_level == "High") +
      post$gamma3 * (sed_level == "Sand") +
      post$gamma4 * (sed_level == "Gravel") +
      post$gamma5 * z +
      post$gamma6 * mean(d$Position == "Front"))
  
  p <- plogis(eta) * 100                       # rows = draws, cols = shear values
  long <- melt(p, varnames = c("draw", "idx"), value.name = "removal")
  
  long %>%
    group_by(idx) %>%
    summarise(med = median(removal),
              lo  = quantile(removal, 0.025),
              hi  = quantile(removal, 0.975), .groups = "drop") %>%
    mutate(shear = z_seq[idx] * shear_sd + shear_mean,
           Slope = slope_level, Sediment = sed_level,
           group = paste(slope_level, sed_level))
}

grid <- expand.grid(Slope = unique(as.character(dat$Slope)),
                    Sediment = unique(as.character(dat$Sediment.Type)),
                    stringsAsFactors = FALSE)
lines_df <- bind_rows(Map(line_fun, grid$Slope, grid$Sediment))

pts <- dat %>%
  mutate(shear = z_Shear * shear_sd + shear_mean,
         removal = y_raw * 100,
         Sediment = as.character(Sediment.Type),
         group = paste(Slope, Sediment))

ggplot() +
  geom_ribbon(data = lines_df, aes(shear, ymin = lo, ymax = hi, fill = Slope, group = group), alpha = 0.15) +
  geom_line(data = lines_df, aes(shear, med, colour = Slope, linetype = Sediment, group = group), linewidth = 1) +
  geom_point(data = pts, aes(shear, removal, colour = Slope, shape = Sediment), alpha = 0.6) +
  labs(x = "Shear stress", y = "Removal of AFDM (%)") +
  theme_classic()




library(dplyr); library(reshape2); library(KernSmooth)

post <- out$sims.list
shear_mean <- mean(Front.Back$Shear.Stress, na.rm = TRUE)  # same values used for z_Shear
shear_sd   <- sd(Front.Back$Shear.Stress,   na.rm = TRUE)

z_seq <- seq(min(dat$z_Shear), max(dat$z_Shear), length.out = 100)

# Each line = gamma0 + one other gamma (NULL = baseline, gamma0 alone)
offsets <- list(
  "gamma0: baseline"       = 0,
  "gamma1: Medium slope"   = post$gamma1,
  "gamma2: High slope"     = post$gamma2,
  "gamma3: Sand"           = post$gamma3,
  "gamma4: Gravel"         = post$gamma4,
  "gamma6: Front position" = post$gamma6
)

preds <- lapply(names(offsets), function(nm) {
  py <- sapply(z_seq, function(z)
    plogis(post$gamma0 + offsets[[nm]] + post$gamma5 * z) * 100)  # rows = draws, cols = shear
  predy <- melt(py)
  names(predy) <- c("iteration", "ps", "y")
  predy$x <- z_seq[predy$ps] * shear_sd + shear_mean
  predy$param <- nm
  predy
})

cols <- setNames(hcl.colors(length(offsets), "Dark 3"), names(offsets))

plot(NA, xlim = range(unlist(lapply(preds, `[[`, "x"))), ylim = c(0, 100),
     xlab = "Shear stress", ylab = "Removal of AFDM (%)", cex.lab = 1.5, bty = "l")

for (p in preds) {
  col  <- cols[[p$param[1]]]
  dens <- bkde2D(cbind(p$x, p$y), bandwidth = c(diff(range(p$x)) / 40, 1),
                 gridsize = c(256, 256), range.x = list(range(p$x), c(0, 100)))
  ramp <- colorRampPalette(c(adjustcolor(col, 0), adjustcolor(col, 0.9)), alpha = TRUE)
  image(dens$x1, dens$x2, dens$fhat^0.25, col = ramp(256), add = TRUE)
  
  med <- p %>% group_by(x) %>% summarise(y = median(y), .groups = "drop")
  lines(med$x, med$y, col = col, lwd = 3)
}

for (p in preds) {
  col <- cols[[p$param[1]]]
  s <- p %>% group_by(x) %>%
    summarise(med = median(y), lo = quantile(y, 0.025), hi = quantile(y, 0.975), .groups = "drop")
  polygon(c(s$x, rev(s$x)), c(s$lo, rev(s$hi)),
          col = adjustcolor(col, 0.15), border = NA)
  lines(s$x, s$med, col = col, lwd = 3)
}

# Observed data
points(dat$z_Shear * shear_sd + shear_mean, dat$y_raw * 100,
       pch = 21, bg = adjustcolor("grey60", 0.5), col = "black", cex = 0.9)

legend("topleft", bty = "n", legend = names(cols), col = cols, lwd = 3, cex = 0.8)
# ODDS RATIO-----------------------------
exp(out$mean$gamma1) # percent change in AFDM is 1.8x more likely to occur in medium slopes than low slopes
exp(out$mean$gamma2) # percent change in AFDM is 1.7x more likely to occur in high slopes than low slopes
exp(out$mean$gamma3) # percent change in AFDM is 0.63x more likely to occur in sand treatments than no sediment treatments
exp(out$mean$gamma4) # percent change in AFDM is 1.1x more likely to occur in gravel treatments than no sediment treatments
exp(out$mean$gamma5) # percent change in AFDM is 2.25x more likely to occur on front of rock than back of rock




# RESPONSE IS PERCENT CHANGE IN MAT THICKNESS-------------------
resp <- "Percent.Change.Mat.Thickness"

dat <- Front.Back %>%
  mutate(
    y_raw = pmin(pmax(-.data[[resp]], 0), 100) / 100, # Proportion REDUCTION; increases (positive % change) capped at 0
    y = (y_raw * (n() - 1) + 0.5) / n() # Squeeze off 0 and 1 for the beta likelihood
  ) %>%
  filter(!is.na(y), !is.na(z_Shear)) %>% # Removes NAs of shear stress
  mutate(rock_idx = as.integer(factor(Rock)))

table(dat$Position)   # check front/back rows survived
table(dat$Trial)                 # should be roughly 9-10 rows per trial
sum(Front.Back$Percent.Change.Mat.Thickness > 0, na.rm = TRUE)   # how many increases got capped at 0


str(dat)
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
  Shear       = dat$z_Shear, # shear stress as continuous covariate,after accounting for the trial specific indicators, effect of shear stress conditional on slope, substrate,etc. can residual be explained by shear stress
  # could include shear stress in model in leiu of front and back position
  # is difference in shear stress explanatory of difference in front of rocks?--> I don't think so, it sits around zero so I think z-standardized shear is telling us the slope is mostly capturing shear stress effect?
  
  FrontInd     = as.numeric(dat$Position == "Front")
)


# will still have to include raw data in a figure for shear stress as opposed to z scores
# this could be a 3 way anova


params <- c("gamma0","gamma1","gamma2","gamma3","gamma4","gamma5", "gamma6",
            "sigma.rock","phi")

inits <- function(){
  list(gamma0 = rnorm(1,0,1), gamma1 = rnorm(1,0,1), gamma2 = rnorm(1,0,1), gamma3 = rnorm(1,0,1), gamma4 = rnorm(1,0,1), 
       gamma5 = rnorm(1,0,1), gamma6 = rnorm(1,0,1),
       sigma.rock = runif(1, 0.1, 1), phi = rgamma(1,1,1))
}

out <- jags(data = jags.data, inits = inits, parameters.to.save = params,
            model.file = "jags model front back.R",
            n.chains = 3, n.iter = 6000, n.burnin = 1000, n.thin = 5, parallel = TRUE)
print(out)       # check Rhat < 1.1 and decent n.eff
plot(out)

MCMCplot(out,
         params = c("gamma0", "gamma1","gamma2","gamma3","gamma4","gamma5", "gamma6"),
         ISB = TRUE, exact = TRUE, main = "",  ci = c(85, 95),
         xlab = "Parameter Estimate (logit scale)",
         col = c("black","#009E73","#009E73","#56B4E9","#56B4E9","#CC79A7", "#E69F00"),
         labels = c("Intercept (low slope & no subst)","Medium slope (vs Low)", "High slope (vs Low)",
                    "Sand (vs None)", "Gravel (vs None)", "Z-shear stress",
                    "Front (vs Back)"),
         guide_lines = TRUE)

# ODDS RATIO-----------------------------
exp(out$mean$gamma1) # percent change in mat thickness is 1.5x more likely to occur in medium slopes than low slopes
exp(out$mean$gamma2) # percent change in mat thickness is 3.2x more likely to occur in high slopes than low slopes -- > 0 
exp(out$mean$gamma3) # percent change in mat thickness is 0.5x more likely to occur in sand treatments than no sediment treatments
exp(out$mean$gamma4) # percent change in mat thickness is 1.3x more likely to occur in gravel treatments than no sediment treatments
exp(out$mean$gamma5) # percent change in mat thickness is 2x more likely to occur on front of rock than back of rock -- > 0 


# come back to this and clean it up

library(dplyr)
library(ggplot2)

sims <- out$sims.list   # posterior draws for each parameter

# Shear values to predict across (z-scored, same scale as the model)
shear_seq <- seq(min(dat$z_Shear), max(dat$z_Shear), length.out = 100)

# Helper: predicted % removal across shear for one intercept
# (rock random effect u set to 0, i.e., an "average" rock)
pred_line <- function(intercept_draws, label) {
  mat <- sapply(shear_seq, function(x) plogis(intercept_draws + sims$gamma5 * x))  # draws x 100
  tibble(
    shear = shear_seq,
    mean  = 100 * colMeans(mat),
    lo    = 100 * apply(mat, 2, quantile, 0.025),
    hi    = 100 * apply(mat, 2, quantile, 0.975),
    group = label
  )
}

lab <- c("Intercept (Low slope, no substrate)",
         "Medium slope", "High slope", "Sand", "Gravel")

plot_df <- bind_rows(
  pred_line(sims$gamma0,                lab[1]),
  pred_line(sims$gamma0 + sims$gamma1,  lab[2]),
  pred_line(sims$gamma0 + sims$gamma2,  lab[3]),
  pred_line(sims$gamma0 + sims$gamma3,  lab[4]),
  pred_line(sims$gamma0 + sims$gamma4,  lab[5])
) %>%
  mutate(group = factor(group, levels = lab))

cols <- c("black", "#009E73", "#E69F00", "#56B4E9", "#CC79A7")
names(cols) <- lab

ggplot(plot_df, aes(shear, mean, color = group, fill = group)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.2) +
  # optional: raw data behind the lines
  geom_point(data = dat, aes(z_Shear, y * 100),
             inherit.aes = FALSE, color = "grey50", alpha = 0.4, size = 1.5) +
  scale_color_manual(values = cols, name = NULL) +
  scale_fill_manual(values = cols, name = NULL) +
  labs(x = "Shear stress (z-score)",
       y = "AFDM removal (%)") +
  theme_classic(base_size = 14) +
  theme(legend.position = "right")

ggplot(Front.Back, aes(z_Shear, Percent.Change.in.AFDM, color = Sediment.Type))+
  geom_line(linewidth = 1.2)



smoothScatter(shear_seq ~ mean,                                     # y ~ and x coordinates
              colramp = colorRampPalette(c('white','forestgreen')), # color ramp
              nrpoints = 0,                                         # no little dark points
              xaxt = 'n',                                           # no x-axis
              ylab = 'Expected clutch',
              xlab = 'Size', cex.lab = 2)  

# T-TESTS------------------------------
# Extract Front and Back into separate vectors
front_data <- Front.Back$Shear.Stress[Front.Back$Position == "Front"]
back_data  <- Front.Back$Shear.Stress[Front.Back$Position == "Back"]
# Run the paired t-test
t.test(front_data, back_data, paired = TRUE)
# shear stress is significantly different between front and back for paired 
mean(front_data, na.rm = TRUE)
mean(back_data, na.rm = TRUE)


# Extract Front and Back into separate vectors
front_data <- Front.Back$Shear.Velocity[Front.Back$Position == "Front"]
back_data  <- Front.Back$Shear.Velocity[Front.Back$Position == "Back"]
# Run the paired t-test
t.test(front_data, back_data, paired = TRUE)
# shear stress is significantly different between front and back for paired 
mean(front_data, na.rm = TRUE)
mean(back_data, na.rm = TRUE)

plot(x = Front.Back$Shear.Stress, y = Front.Back$Shear.Velocity)

ggplot(Front.Back, aes( x= Position, y = Shear.Stress))


# Check this out and fix
library(ggplot2)

ggplot(Front.Back, aes(x = Position, y = Shear.Stress)) +
  geom_boxplot(aes(fill = Position), width = 0.5, alpha = 0.4,
               outlier.shape = NA) +
  #geom_line(aes(group = Rock), color = "grey50", alpha = 0.5) +
  geom_point(aes(color = Position), size = 2.5, alpha = 0.8) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4,
               fill = "white", color = "black") +
  labs(x = NULL, y = "Shear stress") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")


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


# sand should shrink


# Run Beta Regression Model in JAGS---------------

# Data
jags.data <- list(
  N = nrow(dat),
  y = dat$y,
  slopeMedInd  = as.numeric(dat$Slope == "Medium"),
  slopeHighInd = as.numeric(dat$Slope == "High"), # this an above captures between trial shear effect
  SandInd      = as.numeric(dat$Sediment.Type == "Sand "),
  GravelInd    = as.numeric(dat$Sediment.Type == "Gravel")
)


params <- c("gamma0","gamma1","gamma2","gamma3","gamma4","phi")

inits <- function(){
  list(gamma0 = rnorm(1,0,1), gamma1 = rnorm(1,0,1), gamma2 = rnorm(1,0,1), gamma3 = rnorm(1,0,1), gamma4 = rnorm(1,0,1), 
 phi = rgamma(1,1,1))
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


# next...keep trying to understand what is happening
# look at derived quantities between treatments (e.g., mid vs high)
