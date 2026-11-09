model {
  for (i in 1:N) {
    y[i] ~ dbeta(a[i], b[i])
    a[i] <- mu[i] * phi
    b[i] <- (1 - mu[i]) * phi
    logit(mu[i]) <- gamma0 + gamma1*slopeMedInd[i] + gamma2*slopeHighInd[i] +
      gamma3*SandInd[i] + gamma4*GravelInd[i] + gamma5*Shear[i] # + gamma6[Trial[i]]
  }
  
  
  # Priors
  gamma0 ~ dnorm(0, 0.001); gamma1 ~ dnorm(0, 0.001); gamma2 ~ dnorm(0, 0.001)
  gamma3 ~ dnorm(0, 0.001); gamma4 ~ dnorm(0, 0.001); gamma5 ~ dnorm(0, 0.001); 
  
  #for (j in 1:9) {  
  #  gamma6[j] ~ dnorm(lambda, tau) 
  #  
  #} 
  #lambda ~  dnorm(0, 0.001) 
  
  #tau <- 1 / (sigma*sigma) 
  #sigma ~ dunif(0, 5) 
  
  phi ~ dlnorm(0, 1) # precision parameter
}
