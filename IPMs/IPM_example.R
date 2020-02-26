# THESE ARE NOT MY DATA
# EXAMPLES TAKEN FROM THE BOOK : BAYESIAN POPULATION ANALYSIS USING WinBugs, by Kéry and Schaub

# Chapitre 11 BPAWB ####

library("R2WinBUGS")
library("lme4")
#library("MCMCpack")

bugs.dir <- "C:/Users/vuill/Documents/WinBUGS14/"
setwd("C:/Users/vuill/Documents/Github/geek-lunch/IPM/")



# 11.3. Example of a simple IPM (counts, capture-recapture, reproduction) ####
# 11.3.1. Load data
# Population counts (from years 1 to 10)
y <- c(45, 48, 44, 59, 62, 62, 55, 51, 46, 42)

# Capture-recapture data (in m-array format, from years 1 to 10)
# nb d'individus relachés jamais recapturés
m <- matrix(c(11,  0,  0,  0,  0,  0,  0,  0,  0,  70,# individus marqués juvéniles
              0, 12,  0,  1,  0,  0,  0,  0,  0,  52,
              0,  0, 15,  5,  1,  0,  0,  0,  0,  42,
              0,  0,  0,  8,  3,  0,  0,  0,  0,  51,
              0,  0,  0,  0,  4,  3,  0,  0,  0,  61,
              0,  0,  0,  0,  0, 12,  2,  3,  0,  66,
              0,  0,  0,  0,  0,  0, 16,  5,  0,  44,
              0,  0,  0,  0,  0,  0,  0, 12,  0,  46,
              0,  0,  0,  0,  0,  0,  0,  0, 11,  71,
              10,  2,  0,  0,  0,  0,  0,  0,  0,  13,# individus marqués adultes
              0,  7,  0,  1,  0,  0,  0,  0,  0,  27,
              0,  0, 13,  2,  1,  1,  0,  0,  0,  14,
              0,  0,  0, 12,  2,  0,  0,  0,  0,  20,
              0,  0,  0,  0, 10,  2,  0,  0,  0,  21,
              0,  0,  0,  0,  0, 11,  2,  1,  1,  14,
              0,  0,  0,  0,  0,  0, 12,  0,  0,  18,
              0,  0,  0,  0,  0,  0,  0, 11,  1,  21,
              0,  0,  0,  0,  0,  0,  0,  0, 10,  26), ncol = 10, byrow = TRUE)

# Productivity data (from years 1 to 9)
J <- c(64, 132,  86, 154, 156, 134, 116, 106, 110)    # nb d'oisillons enregistrés
R <- c(21, 28, 26, 38, 35, 33, 31, 30, 33)            # nb annuel de couvées suivies


# 11.3.2. Analysis of the model
# Specify model in BUGS language
sink("ipm.bug")
cat("
model {
#-------------------------------------------------
#  Integrated population model
#  - Age structured model with 2 age classes: 
#		1-year old and adult (at least 2 years old)
#  - Age at first breeding = 1 year
#  - Prebreeding census, female-based
#  - All vital rates assumed to be constant
#-------------------------------------------------

#-------------------------------------------------
# 1. Define the priors for the parameters 
#-------------------------------------------------
# Observation error
tauy <- pow(sigma.y, -2)
sigma.y ~ dunif(0, 50)
sigma2.y <- pow(sigma.y, 2)

# Initial population sizes
N1[1] ~ dnorm(100, 0.0001)I(0,)     # 1-year
Nad[1] ~ dnorm(100, 0.0001)I(0,)    # Adults

# Survival and recapture probabilities, as well as productivity
for (t in 1:(nyears-1)){
   sjuv[t] <- mean.sjuv
   sad[t] <- mean.sad
   p[t] <- mean.p
   f[t] <- mean.fec
   }

mean.sjuv ~ dunif(0, 1)
mean.sad ~ dunif(0, 1)
mean.p ~ dunif(0, 1)
mean.fec ~ dunif(0, 20)


#-------------------------------------------------
# 2. Derived parameters
#-------------------------------------------------
# Population growth rate
for (t in 1:(nyears-1)){
   lambda[t] <- Ntot[t+1] / Ntot[t]
   }


#-------------------------------------------------
# 3. The likelihoods of the single data sets
#-------------------------------------------------
# 3.1. Likelihood for population population count data (state-space model)
   # 3.1.1 System process
   for (t in 2:nyears){
      mean1[t] <- f[t-1] / 2 * sjuv[t-1] * Ntot[t-1]
      N1[t] ~ dpois(mean1[t])
      Nad[t] ~ dbin(sad[t-1], Ntot[t-1])
      }
   for (t in 1:nyears){
      Ntot[t] <- Nad[t] + N1[t]
      }
   
   # 3.1.2 Observation process
   for (t in 1:nyears){
      y[t] ~ dnorm(Ntot[t], tauy)
      }

# 3.2 Likelihood for capture-recapture data: CJS model (2 age classes)
# Multinomial likelihood
for (t in 1:2*(nyears-1)){
   m[t,1:nyears] ~ dmulti(pr[t,], r[t])
   }

# Calculate the number of released individuals
for (t in 1:2*(nyears-1)){
   r[t] <- sum(m[t,])
   }

# m-array cell probabilities for juveniles
for (t in 1:(nyears-1)){
   # Main diagonal
   q[t] <- 1-p[t]
   pr[t,t] <- sjuv[t] * p[t]
   # Above main diagonal
   for (j in (t+1):(nyears-1)){
      pr[t,j] <- sjuv[t]*prod(sad[(t+1):j])*prod(q[t:(j-1)])*p[j]
      } #j	
   # Below main diagonal
   for (j in 1:(t-1)){
      pr[t,j] <- 0
      } #j
   # Last column: probability of non-recapture
   pr[t,nyears] <- 1-sum(pr[t,1:(nyears-1)])
   } #t

# m-array cell probabilities for adults
for (t in 1:(nyears-1)){
   # Main diagonal
   pr[t+nyears-1,t] <- sad[t] * p[t]
   # Above main diagonal
   for (j in (t+1):(nyears-1)){
      pr[t+nyears-1,j] <- prod(sad[t:j])*prod(q[t:(j-1)])*p[j]
      } #j
   # Below main diagonal
   for (j in 1:(t-1)){
      pr[t+nyears-1,j] <- 0
      } #j
   # Last column
   pr[t+nyears-1,nyears] <- 1 - sum(pr[t+nyears-1,1:(nyears-1)])
   } #t

# 3.3. Likelihood for productivity data: Poisson regression
for (t in 1:(nyears-1)){
   J[t] ~ dpois(rho[t])
   rho[t] <- R[t]*f[t]
   }
}
",fill = TRUE)
sink()


# Bundle data
bugs.data <- list(m = m, y = y, J = J, R = R, nyears = dim(m)[2])

# Initial values
inits <- function(){list(mean.sjuv = runif(1, 0, 1), mean.sad = runif(1, 0, 1), mean.p = runif(1, 0, 1), 
                         mean.fec = runif(1, 0, 10), N1 = rpois(dim(m)[2], 30), Nad = rpois(dim(m)[2], 30), 
                         sigma.y = runif(1 ,0, 10))}  

# Parameters monitored
parameters <- c("mean.sjuv", "mean.sad", "mean.p", "mean.fec", "N1", "Nad", "Ntot", "lambda", "sigma2.y")

# MCMC settings
ni <- 20000
nt <- 6
nb <- 5000
nc <- 3

# Call WinBUGS from R (BRT 2 min)
ipm <- bugs(bugs.data, inits, parameters, "ipm.bug", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            debug = TRUE, bugs.directory = bugs.dir, working.directory = getwd())

print(ipm, digits = 3)


# Produce Fig. 11-4
par(cex = 1.2)
lower <- upper <- numeric()
for (i in 1:10){
  lower[i] <- quantile(ipm$sims.list$Ntot[,i], 0.025)
  upper[i] <- quantile(ipm$sims.list$Ntot[,i], 0.975)
}
plot(ipm$mean$Ntot, type = "b", ylim = c(35, 65), ylab = "Population size", xlab = "Year", las = 1, pch = 16, 
     col = "blue", frame = F, cex = 1.5)
segments(1:10, lower, 1:10, upper, col = "blue")
points(y, type = "b", col = "black", pch = 16, lty = 2, cex = 1.5)
legend(x = 1, y = 65, legend = c("Counts", "Estimates"), pch = c(16, 16), col = c("black", "blue"), lty = c(2, 1), 
       bty = "n")




# 11.6. Real data example: hoopoe population dynamics #### 
# Load data
nyears <- 9	  # Number of years

# Capture-recapture data: m-array of juveniles and adults (these are males and females together)
marray.j <- matrix (c(15, 3, 0, 0, 0, 0, 0, 0, 198, 0, 34, 9, 1, 0, 0, 0, 0, 287, 0, 0, 56, 8, 1, 0, 0, 0, 455, 0, 0, 0, 48, 3, 1, 0, 0, 518, 0, 0, 0, 0, 45, 13, 2, 0, 463, 0, 0, 0, 0, 0, 27, 7, 0, 493, 0, 0, 0, 0, 0, 0, 37, 3, 434, 0, 0, 0, 0, 0, 0, 0, 39, 405), nrow = 8, ncol = 9, byrow = TRUE)
marray.a <- matrix(c(14, 2, 0, 0, 0, 0, 0, 0, 43, 0, 22, 4, 0, 0, 0, 0, 0, 44, 0, 0, 34, 2, 0, 0, 0, 0, 79, 0, 0, 0, 51, 3, 0, 0, 0, 94, 0, 0, 0, 0, 45, 3, 0, 0, 118, 0, 0, 0, 0, 0, 44, 3, 0, 113, 0, 0, 0, 0, 0, 0, 48, 2, 99, 0, 0, 0, 0, 0, 0, 0, 51, 90), nrow = 8, ncol = 9, byrow = TRUE)

# Population count data
popcount <- c(32, 42, 64, 85, 82, 78, 73, 69, 79)

# Productivity data
J <- c(189, 274, 398, 538, 520, 476, 463, 438) # Number of offspring 
R <- c(28, 36, 57, 77, 81, 83, 77, 72)         # Number of surveyed broods 

# Specify model in BUGS language
sink("ipm.hoopoe.bug")
cat("
model {
#------------------------------------------------------------
#  Integrated population model
#  - Age structured model with 2 age classes: 
#		1-year old and adult (at least 2 years old)
#  - Age at first breeding = 1 year
#  - Prebreeding census, female-based
#  - All vital rates are assumed to be time-dependent (random)
#  - Explicit estimation of immigration
#-------------------------------------------------------------

#----------------------------------------
# 1. Define the priors for the parameters
#----------------------------------------
# Initial population sizes
N1[1] ~ dnorm(100, 0.0001)I(0,)           # 1-year old individuals
NadSurv[1] ~ dnorm(100, 0.0001)I(0,)      # Adults >= 2 years
Nadimm[1] ~ dnorm(100, 0.0001)I(0,)       # Immigrants

# Mean demographic parameters (on appropriate scale)
l.mphij ~ dnorm(0, 0.0001)I(-10,10)       # Bounded to help with convergence
l.mphia ~ dnorm(0, 0.0001)I(-10,10)
l.mfec ~ dnorm(0, 0.0001)I(-10,10)
l.mim ~ dnorm(0, 0.0001)I(-10,10)
l.p ~ dnorm(0, 0.0001)I(-10,10)

# Precision of standard deviations of temporal variability
sig.phij ~ dunif(0, 10)
tau.phij <- pow(sig.phij, -2)
sig.phia ~ dunif(0, 10)
tau.phia <- pow(sig.phia, -2)
sig.fec ~ dunif(0, 10)
tau.fec <- pow(sig.fec, -2)
sig.im ~ dunif(0, 10)
tau.im <- pow(sig.im, -2)

# Distribution of error terms (Bounded to help with convergence)
for (t in 1:(nyears-1)){
   epsilon.phij[t] ~ dnorm(0, tau.phij)I(-15,15)	
   epsilon.phia[t] ~ dnorm(0, tau.phia)I(-15,15)
   epsilon.fec[t] ~ dnorm(0, tau.fec)I(-15,15)
   epsilon.im[t] ~ dnorm(0, tau.im)I(-15,15)
   }

#-------------------------
# 2. Constrain parameters
#-------------------------
for (t in 1:(nyears-1)){
   logit(phij[t]) <- l.mphij + epsilon.phij[t]  # Juv. apparent survival
   logit(phia[t]) <- l.mphia + epsilon.phia[t]  # Adult apparent survival
   log(f[t]) <- l.mfec + epsilon.fec[t]         # Productivity
   log(omega[t]) <- l.mim + epsilon.im[t]       # Immigration
   logit(p[t]) <- l.p                           # Recapture probability
   }

#-----------------------
# 3. Derived parameters
#-----------------------
mphij <- exp(l.mphij)/(1+exp(l.mphij))   # Mean juvenile survival probability
mphia <- exp(l.mphia)/(1+exp(l.mphia))   # Mean adult survival probability
mfec <- exp(l.mfec)                      # Mean productivity
mim <- exp(l.mim)                        # Mean immigration rate

# Population growth rate
for (t in 1:(nyears-1)){
   lambda[t] <- Ntot[t+1] / Ntot[t]
   logla[t] <- log(lambda[t])
   }
mlam <- exp((1/(nyears-1))*sum(logla[1:(nyears-1)]))   # Geometric mean

#--------------------------------------------
# 4. The likelihoods of the single data sets
#--------------------------------------------
# 4.1. Likelihood for population population count data (state-space model)
   # 4.1.1 System process
   for (t in 2:nyears){
      mean1[t] <- 0.5 * f[t-1] * phij[t-1] * Ntot[t-1]
      N1[t] ~ dpois(mean1[t])
      NadSurv[t] ~ dbin(phia[t-1], Ntot[t-1])
      mpo[t] <- Ntot[t-1] * omega[t-1]
      Nadimm[t] ~ dpois(mpo[t])
      }

   # 4.1.2 Observation process
   for (t in 1:nyears){
      Ntot[t] <- NadSurv[t] + Nadimm[t] + N1[t]
      y[t] ~ dpois(Ntot[t])
      }

# 4.2 Likelihood for capture-recapture data: CJS model (2 age classes)
# Multinomial likelihood
for (t in 1:(nyears-1)){
   marray.j[t,1:nyears] ~ dmulti(pr.j[t,], r.j[t])
   marray.a[t,1:nyears] ~ dmulti(pr.a[t,], r.a[t])
   }

# Calculate number of released individuals
for (t in 1:(nyears-1)){
   r.j[t] <- sum(marray.j[t,])
   r.a[t] <- sum(marray.a[t,])
   }

# m-array cell probabilities for juveniles
for (t in 1:(nyears-1)){
   q[t] <- 1-p[t]
   # Main diagonal
   pr.j[t,t] <- phij[t]*p[t]
   # Above main diagonal
   for (j in (t+1):(nyears-1)){
      pr.j[t,j] <- phij[t]*prod(phia[(t+1):j])*prod(q[t:(j-1)])*p[j]
      } #j
   # Below main diagonal
   for (j in 1:(t-1)){
      pr.j[t,j] <- 0
      } #j
   # Last column
   pr.j[t,nyears] <- 1-sum(pr.j[t,1:(nyears-1)])
   } #t

# m-array cell probabilities for adults
for (t in 1:(nyears-1)){
   # Main diagonal
   pr.a[t,t] <- phia[t]*p[t]
   # above main diagonal
   for (j in (t+1):(nyears-1)){
      pr.a[t,j] <- prod(phia[t:j])*prod(q[t:(j-1)])*p[j]
      } #j
   # Below main diagonal
   for (j in 1:(t-1)){
      pr.a[t,j] <- 0
      } #j
   # Last column
   pr.a[t,nyears] <- 1-sum(pr.a[t,1:(nyears-1)])
   } #t

# 4.3. Likelihood for productivity data: Poisson regression
for (t in 1:(nyears-1)){
   J[t] ~ dpois(rho[t])
   rho[t] <- R[t] * f[t]
   }
}
",fill = TRUE)
sink()

# Bundle data
bugs.data <- list(nyears = nyears, marray.j = marray.j, marray.a = marray.a, y = popcount, J = J, R = R)

# Initial values
inits <- function(){list(l.mphij = rnorm(1, 0.2, 0.5), l.mphia = rnorm(1, 0.2, 0.5), 
                         l.mfec = rnorm(1, 0.2, 0.5), l.mim = rnorm(1, 0.2, 0.5), l.p = rnorm(1, 0.2, 1), 
                         sig.phij = runif(1, 0.1, 10), sig.phia = runif(1, 0.1, 10), sig.fec = runif(1, 0.1, 10), 
                         sig.im = runif(1, 0.1, 10), N1 = round(runif(nyears, 1, 50), 0), 
                         NadSurv = round(runif(nyears, 5, 50), 0), Nadimm = round(runif(nyears, 1, 50), 0))}

# Parameters monitored
parameters <- c("phij", "phia", "f", "omega", "p", "lambda", "mphij", "mphia", "mfec", "mim", "mlam", 
                "sig.phij", "sig.phia", "sig.fec", "sig.im", "N1", "NadSurv", "Nadimm", "Ntot")

# MCMC settings
ni <- 20000
nt <- 6
nb <- 10000
nc <- 3

# Call WinBUGS from R (BRT 5 min)
ipm.hoopoe <- bugs(bugs.data, inits, parameters, "ipm.hoopoe.bug", n.chains = nc, n.thin = nt, n.iter = ni, 
                   n.burnin = nb, debug = TRUE, bugs.directory = bugs.dir, working.directory = getwd())


print(ipm.hoopoe, dig=3)








# 11.4. Another example of an IPM: ####
# Estimating productivity without explicit productivity data
# Specify model in BUGS language
sink("ipm-prod.bug")
cat("
model {
#-------------------------------------------------
#  Integrated population model
#  - Age structured model with 2 age classes: 
#		1-year old and adult (at least 2 years old)
#  - Age at first breeding = 1 year
#  - Prebreeding census, female-based
#  - All vital rates assumed to be constant
#-------------------------------------------------

#-------------------------------------------------
# 1. Define the priors for the parameters
#-------------------------------------------------
# Observation error
tauy <- pow(sigma.y, -2)
sigma.y ~ dunif(0, 50)
sigma2.y <- pow(sigma.y, 2)

# Initial population sizes
N1[1] ~ dnorm(100, 0.0001)I(0,)     # 1-year
Nad[1] ~ dnorm(100, 0.0001)I(0,)    # Adults

# Survival and recapture probabilities, as well as productivity
for (t in 1:(nyears-1)){
   sjuv[t] <- mean.sjuv
   sad[t] <- mean.sad
   p[t] <- mean.p
   f[t] <- mean.fec
   }

mean.sjuv ~ dunif(0, 1)
mean.sad ~ dunif(0, 1)
mean.p ~ dunif(0, 1)
mean.fec ~ dunif(0, 20)

#-------------------------------------------------
# 2. Derived parameters
#-------------------------------------------------
# Population growth rate
for (t in 1:(nyears-1)){
   lambda[t] <- Ntot[t+1] / Ntot[t]
   }

#-------------------------------------------------
# 3. The likelihoods of the single data sets
#-------------------------------------------------
# 3.1. Likelihood for population population count data (state-space model)
   # 3.1.1 System process
   for (t in 2:nyears){
      mean1[t] <- f[t-1] / 2 * sjuv[t-1] * Ntot[t-1]
      N1[t] ~ dpois(mean1[t])
      Nad[t] ~ dbin(sad[t-1], Ntot[t-1])
      }
   for (t in 1:nyears){
      Ntot[t] <- Nad[t] + N1[t]
      }

   # 3.1.2 Observation process
   for (t in 1:nyears){
      y[t] ~ dnorm(Ntot[t], tauy)
      }

# 3.2 Likelihood for capture-recapture data: CJS model (2 age classes)
# Multinomial likelihood
for (t in 1:2*(nyears-1)){
   m[t,1:nyears] ~ dmulti(pr[t,], r[t])
   }

# Calculate the number of released individuals
for (t in 1:2*(nyears-1)){
   r[t] <- sum(m[t,])
   }

# m-array cell probabilities for juveniles
for (t in 1:(nyears-1)){
   # Main diagonal
   q[t] <- 1-p[t]
   pr[t,t] <- sjuv[t] * p[t]
   # Above main diagonal
   for (j in (t+1):(nyears-1)){
      pr[t,j] <- sjuv[t]*prod(sad[(t+1):j])*prod(q[t:(j-1)])*p[j]
      } #j	
   # Below main diagonal
   for (j in 1:(t-1)){
      pr[t,j] <- 0
      } #j
   # Last column: probability of non-recapture
   pr[t,nyears] <- 1-sum(pr[t,1:(nyears-1)])
   } #t

# m-array cell probabilities for adults
for (t in 1:(nyears-1)){
   # Main diagonal
   pr[t+nyears-1,t] <- sad[t] * p[t]
   # Above main diagonal
   for (j in (t+1):(nyears-1)){
      pr[t+nyears-1,j] <- prod(sad[t:j])*prod(q[t:(j-1)])*p[j]
      } #j
   # Below main diagonal
   for (j in 1:(t-1)){
      pr[t+nyears-1,j] <- 0
      } #j
   # Last column
   pr[t+nyears-1,nyears] <- 1 - sum(pr[t+nyears-1,1:(nyears-1)])
   } #t
}
",fill = TRUE)
sink()

# Bundle data
bugs.data <- list(m = m, y = y, nyears = dim(m)[2])

# Initial values
inits <- function(){list(mean.sjuv= runif(1, 0, 1), mean.sad = runif(1, 0, 1), mean.p = runif(1, 0, 1), 
                         mean.fec = runif(1, 0, 10), N1 = rpois(dim(m)[2], 30), Nad = rpois(dim(m)[2], 30), 
                         sigma.y = runif(1, 0, 10))}  

# Parameters monitored
parameters <- c("mean.sjuv", "mean.sad", "mean.p", "mean.fec", "N1", "Nad", "Ntot", "lambda", "sigma2.y")

# MCMC settings
ni <- 20000
nt <- 6
nb <- 5000
nc <- 3

# Call WinBUGS from R (BRT 1 min)
ipm.prod <- bugs(bugs.data, inits, parameters, "ipm-prod.bug", n.chains = nc, n.thin = nt, n.iter = ni, 
                 n.burnin = nb, debug = TRUE, bugs.directory = bugs.dir, working.directory = getwd())

# Summarize posteriors
print(ipm.prod, digits = 3)




# 11.5. IPMs for population viability analysis ####
# Specify model in BUGS language
sink("ipm-pred.bug")
cat("
model {
#-------------------------------------------------
#  Integrated population model
#  - Age structured model with 2 age classes: 
#		1-year old and adult (at least 2 years old)
#  - Age at first breeding = 1 year
#  - Prebreeding census, female-based
#  - All vital rates assumed to be constant
#-------------------------------------------------

#-------------------------------------------------
# 1. Define the priors for the parameters
#-------------------------------------------------
# Observation error
tauy <- pow(sigma.y, -2)
sigma.y ~ dunif(0, 50)
sigma2.y <- pow(sigma.y, 2)

# Initial population sizes
N1[1] ~ dnorm(100, 0.0001)I(0,)     # 1-year
Nad[1] ~ dnorm(100, 0.0001)I(0,)    # Adults

# Survival and recapture probabilities, as well as productivity
for (t in 1:(nyears-1+t.pred)){
   sjuv[t] <- mean.sjuv
   sad[t] <- mean.sad
   p[t] <- mean.p
   f[t] <- mean.fec
   }

mean.sjuv ~ dunif(0, 1)
mean.sad ~ dunif(0, 1)
mean.p ~ dunif(0, 1)
mean.fec ~ dunif(0, 20)

#-------------------------------------------------
# 2. Derived parameters
#-------------------------------------------------
# Population growth rate
for (t in 1:(nyears-1+t.pred)){
   lambda[t] <- Ntot[t+1] / Ntot[t]
   }

#-------------------------------------------------
# 3. The likelihoods of the single data sets
#-------------------------------------------------
# 3.1. Likelihood for population population count data (state-space model)
   # 3.1.1 System process
   for (t in 2:nyears+t.pred){
      mean1[t] <- f[t-1] / 2 * sjuv[t-1] * Ntot[t-1]
      N1[t] ~ dpois(mean1[t])
      Nad[t] ~ dbin(sad[t-1], Ntot[t-1])
      }
   for (t in 1:nyears+t.pred){
      Ntot[t] <- Nad[t] + N1[t]
      }

   # 3.1.2 Observation process
   for (t in 1:nyears){
      y[t] ~ dnorm(Ntot[t], tauy)
      }

# 3.2 Likelihood for capture-recapture data: CJS model (2 age classes)
# Multinomial likelihood
for (t in 1:2*(nyears-1)){
   m[t,1:nyears] ~ dmulti(pr[t,], r[t])
   }

# Calculate the number of released individuals
for (t in 1:2*(nyears-1)){
   r[t] <- sum(m[t,])
   }

# m-array cell probabilities for juveniles
for (t in 1:(nyears-1)){
   # Main diagonal
   q[t] <- 1-p[t]
   pr[t,t] <- sjuv[t] * p[t]
   # Above main diagonal
   for (j in (t+1):(nyears-1)){
      pr[t,j] <- sjuv[t]*prod(sad[(t+1):j])*prod(q[t:(j-1)])*p[j]
      } #j	
   # Below main diagonal
   for (j in 1:(t-1)){
      pr[t,j] <- 0
      } #j
   # Last column: probability of non-recapture
   pr[t,nyears] <- 1-sum(pr[t,1:(nyears-1)])
   } #t

# m-array cell probabilities for adults
for (t in 1:(nyears-1)){
   # Main diagonal
   pr[t+nyears-1,t] <- sad[t] * p[t]
   # Above main diagonal
   for (j in (t+1):(nyears-1)){
      pr[t+nyears-1,j] <- prod(sad[t:j])*prod(q[t:(j-1)])*p[j]
      } #j
   # Below main diagonal
   for (j in 1:(t-1)){
      pr[t+nyears-1,j] <- 0
      } #j
   # Last column
   pr[t+nyears-1,nyears] <- 1 - sum(pr[t+nyears-1,1:(nyears-1)])
   } #t

# 3.3. Likelihood for productivity data: Poisson regression
for (t in 1:(nyears-1)){
   J[t] ~ dpois(rho[t])
   rho[t] <- R[t]*f[t]
   }
}
",fill = TRUE)
sink()

# Give the number of future years for which population size shall be estimated
t.pred <- 5

# Bundle data
bugs.data <- list(m = m, y = y, J = J, R = R, nyears = dim(m)[2], t.pred = t.pred)

# Initial values
inits <- function(){list(mean.sjuv = runif(1, 0, 1), mean.sad = runif(1, 0, 1), mean.p = runif(1, 0, 1), 
                         mean.fec = runif(1, 0, 10), N1 = rpois(dim(m)[2]+ t.pred, 30), 
                         Nad = rpois(dim(m)[2]+ t.pred, 30), sigma.y = runif(1, 0, 10))}  

# Parameters monitored
parameters <- c("mean.sjuv", "mean.sad", "mean.p", "mean.fec", "N1", "Nad", "Ntot", "lambda", "sigma2.y")

# MCMC settings
ni <- 20000
nt <- 6
nb <- 5000
nc <- 3

# Call WinBUGS from R (BRT 1 min)
ipm.pred <- bugs(bugs.data, inits, parameters, "ipm-pred.bug", n.chains = nc, n.thin = nt, n.iter = ni, 
                 n.burnin = nb, debug = TRUE, bugs.directory = bugs.dir, working.directory = getwd())

# Summarize posteriors
print(ipm.pred, digits = 3)



# Produce Fig. 11-5
par(cex = 1.2)
lower <- upper <- numeric()
for (i in 1:15){
  lower[i] <- quantile(ipm.pred$sims.list$Ntot[,i], 0.025)
  upper[i] <- quantile(ipm.pred$sims.list$Ntot[,i], 0.975)
}
plot(ipm.pred$mean$Ntot, type = "b", ylim = c(10, max(upper)), ylab = "Population size", xlab = "Year", las = 1, 
     pch = 16, col = "blue", frame = F)
segments(1:15, lower, 1:15, upper, col = "blue")
points(y, type = "b", col = "black", lty = 2, pch = 16)
legend(x = 1, y = 80, legend = c("Counts", "Estimates"), pch = c(16, 16), col = c("black", "blue"), lty = c(2, 1), 
       bty = "n")

mean(ipm.pred$sims.list$Ntot[,15]<30)


