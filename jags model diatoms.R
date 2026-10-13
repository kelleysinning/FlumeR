model {
  for (i in 1:N) {
    y[i] ~ dbeta(a[i], b[i])
    a[i] <- mu[i] * phi
    b[i] <- (1 - mu[i]) * phi
    logit(mu[i]) <- gamma0 + gamma1*slopeMedInd[i] + gamma2*slopeHighInd[i] +
      gamma3*SandInd[i] + gamma4*GravelInd[i]
  }
  
  
  # Priors
  gamma0 ~ dnorm(0, 0.1); gamma1 ~ dnorm(0, 0.1); gamma2 ~ dnorm(0, 0.1)
  gamma3 ~ dnorm(0, 0.1); gamma4 ~ dnorm(0, 0.1)
  phi ~ dgamma(0.1, 0.1)
}