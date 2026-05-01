model {
  for (i in 1:N) {
    y[i] ~ dbeta(alpha[i], beta[i])
    
    alpha[i] <- mu[i] * phi
    beta[i]  <- (1 - mu[i]) * phi
    
    logit(mu[i]) <- gamma0 + gamma1 * slopeMedInd[i] + gamma2 * slopeHighInd[i] + gamma3 * SandInd[i] + gamma4 * GravelInd[i] + gamma5 * hydraulic[i]
  }
  
  # Priors
  gamma0 ~ dnorm(0, 0.001)
  gamma1 ~ dnorm(0, 0.001)
  gamma2 ~ dnorm(0, 0.001)
  gamma3 ~ dnorm(0, 0.001)
  gamma4 ~ dnorm(0, 0.001)
  gamma5 ~ dnorm(0, 0.001)
  
  phi ~ dgamma(0.01, 0.01)   # precision parameter
}