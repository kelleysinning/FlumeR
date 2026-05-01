###
### Settings
### 
library(tidyr)
library(dplyr)
library(ggplot2)
library(jagsUI)
library(MCMCvis)

###
### Simulate Data
###

## Experimental design
N = 90
trial <- rep(1:9, each = 10)
slope <- rep(rep(c("Low", "Medium", "High") , each = 10), times = 3)
sediment <- rep(c("None", "Sand", "Gravel"), each = 30)
hydraulic <- rnorm(n = 90, mean = 0, sd = 1)  # assuming mean standardization by TRIAL
sim.data <- data.frame(trial, slope, sediment, hydraulic) %>%
  mutate(slopeMedInd = if_else(slope == "Medium", 1, 0),     # Indicator (1, 0)
         slopeHighInd = if_else(slope == "High", 1, 0),
         SandInd = if_else(sediment == "Sand", 1, 0),
         GravelInd = if_else(sediment == "Gravel", 1, 0))

## Parameters: Low slope and no sediment are reference levels 
gamma0 <- -0.5  # Intercept
gamma1 <- 0.7   # Effect of Medium slope relative to Low slope
gamma2 <- 1.5   # Effect of High slope relative to Low slope
gamma3 <- 0.5   # Effect of Sand relative to No sediment
gamma4 <- 1.2   # Effect of Gravel relative to No sediment
gamma5 <- 0.8   # Effect of Hydraulic metric
phi <- 10       # Precision (noise)

## Response (Percent change in Didymo)
eta <- gamma0 + gamma1*sim.data$slopeMedInd + gamma2*sim.data$slopeHighInd + 
         gamma3*sim.data$SandInd + gamma4*sim.data$GravelInd + 
         gamma5*sim.data$hydraulic  

mu <- 1 / (1 + exp(-eta))  # logit transformation
shape1 <- mu*phi           # convert mu and phi to Beta shape parameters (a and b) - Needed fro rbeta function below
shape2 <- (1-mu)*phi
sim.data$perc.change <- rbeta(N, shape1, shape2)

## Visualize data
sim.data$slope <- factor(sim.data$slope, levels = c("High", "Medium", "Low"))
sim.data$sediment <- factor(sim.data$sediment, levels = c("Gravel", "Sand", "None"))

ggplot(sim.data, aes(x = hydraulic, y = perc.change*100)) + 
  geom_point() +
  facet_grid(slope ~ sediment) +
  xlab("Hydraulic Metric") +
  ylab("Percent Reduction in Didymo") +
  theme_bw()


###
### Run Beta Regression Model in JAGS
###

## Data
jags.data <- list(
  N = N,
  y = sim.data$perc.change,
  slopeMedInd = sim.data$slopeMedInd,
  slopeHighInd = sim.data$slopeHighInd,
  SandInd = sim.data$SandInd,
  GravelInd = sim.data$GravelInd,
  hydraulic = sim.data$hydraulic
)

## Parameters to monitor
params <- c("gamma0", "gamma1", "gamma2", "gamma3", "gamma4", "gamma5", "phi")

## Initial values
inits <- function(){
  list(gamma0 = rnorm(1,0,1), gamma1 = rnorm(1,0,1), gamma2 = rnorm(1,0,1), gamma3 = rnorm(1,0,1), gamma4 = rnorm(1,0,1), 
       gamma5 = rnorm(1,0,1), phi = rgamma(1,1,1))
}

## Run model
out <- jags(data = jags.data, inits = inits, parameters.to.save = params, model.file = "jags model.R",
            n.chains = 3, n.iter = 6000, n.burnin = 1000, n.thin = 5, parallel = TRUE)
print(out)
plot(out)


###
### Plot posterior samples
###

MCMCplot(out,
         params = c('gamma0', 'gamma1', 'gamma2', 'gamma3', 'gamma4', 'gamma5'),
         ISB = TRUE, exact = TRUE,
         main = '',
         xlab = 'Parameter Estimate', # horiz = FALSE,
         xlim = c(-3, 3), 
         col = c("#E69F00", "#009E73", "#009E73", "#56B4E9", "#56B4E9", "#56B4E9"),
         labels = c("Intercept (low slope & no subst)", "Relative effect of Medium slope", "Relative effect of High slope", 
                    "Relative effect of Sand", "Relative effect of Gravel", "Hydrualic effect"),
         guide_lines = TRUE)


###
###  Save Output
###

save.image("flume_Didymo.RData")

