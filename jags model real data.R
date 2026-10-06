model {
  for (i in 1:N) {
    y[i] ~ dbeta(a[i], b[i])
    a[i] <- mu[i] * phi
    b[i] <- (1 - mu[i]) * phi
    logit(mu[i]) <- gamma0 + gamma1*slopeMedInd[i] + gamma2*slopeHighInd[i] +
      gamma3*SandInd[i] + gamma4*GravelInd[i] +
      gamma5*hydraulic[i] + gamma6*FrontInd[i] +
      u[rock[i]]
  }
  
  # Rock-level random intercept (front and back of the same rock share it)
  for (r in 1:nRock) { u[r] ~ dnorm(0, tau.rock) }
  sigma.rock ~ dunif(0, 3)
  tau.rock <- pow(sigma.rock, -2)
  
  # Priors
  gamma0 ~ dnorm(0, 0.1); gamma1 ~ dnorm(0, 0.1); gamma2 ~ dnorm(0, 0.1)
  gamma3 ~ dnorm(0, 0.1); gamma4 ~ dnorm(0, 0.1); gamma5 ~ dnorm(0, 0.1)
  gamma6 ~ dnorm(0, 0.1)
  phi ~ dgamma(0.1, 0.1)
}