model {
  for (i in 1:N) {
    y[i] ~ dbeta(a[i], b[i])
    a[i] <- mu[i] * phi
    b[i] <- (1 - mu[i]) * phi
    logit(mu[i]) <- gamma0 + inprod(beta[], X[i, ]) + u[rock[i]]
  }
  
  for (r in 1:nRock) { u[r] ~ dnorm(0, tau.rock) }
  sigma.rock ~ dunif(0, 3)
  tau.rock <- pow(sigma.rock, -2)
  
  gamma0 ~ dnorm(0, 0.1)
  for (k in 1:K) { beta[k] ~ dnorm(0, 0.1) }
  phi ~ dgamma(0.1, 0.1)
}