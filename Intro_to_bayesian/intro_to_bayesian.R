
#Working directory and base data 
setwd("C:/yourpath/workshops/Intro_to_bayesian")# Flo

BTdata=read.delim("BTData.txt", sep = " ", header=T, dec = "." )
#or data("BTdata", package = "MCMCglmm")

head(BTdata)
str(BTdata)
summary(BTdata) 

# Description of the data comes from MCMCglmm package : Blue Tit (Cyanistes caeruleus) data frame for a quantitative genetic experiment. 
# 828 rows and 7 columns, with variables tarsus length (tarsus) and colour (back) measured on 828 individuals (animal). 
# The mother of each is also recorded (dam) together with the foster nest (fosternest) in which the chicks were reared. The date on which the first egg in each nest hatched (hatchdate) is recorded together with the sex (sex) of the individuals.

# Convert factor to integer ####
#library(dplyr)

BTdata$sex_no <- as.integer((BTdata$sex))

BTdata$dam_no <- as.integer((BTdata$dam))
dam_ID <- distinct(BTdata, dam_no, dam) %>%
  arrange(dam_no)

# Install RStan ####
#install.packages("devtools")
library(devtools)
Sys.setenv(PATH = paste("C:/Rtools/bin", Sys.getenv("PATH"), sep=";"))
Sys.setenv(PATH = paste("C:/Rtools/mingw_64/bin", Sys.getenv("PATH"), sep=";"))

#install.packages("rstan")
#library(rstan)

# Run Stan ####
data_for_stan_model <- list(n_obs = nrow(BTdata), n_dam = nrow(dam_ID), tarsus = BTdata$tarsus, 
                            sex_no = BTdata$sex_no, dam_no = BTdata$dam_no)

results <- stan(file = "Intro_to_bayesian/bayesian_model.stan", data = data_for_stan_model, 
                iter = 500, chains = 5, warmup = 250)

#Alternate way now using brms ####
#install.packages("brms")
#Information about brms: https://github.com/paul-buerkner/brms
#or https://paul-buerkner.github.io/brms/

#Formula###

require(brms)

form_tars <-
  bf(formula = tarsus ~ sex + (1 | dam) )+  gaussian()

#priors
priors<-c(set_prior("normal(0, 1)",class = "Intercept"),
          set_prior("normal(0, 1)",class = "b")
          )


#- the sampler ####

start_time <- Sys.time()

# tarsus <- brm(form_tars,
#   data = BTdata,
#   prior = priors,
#   iter = 5000,
#   chains = 3,
#   cores = 3,
#   warmup = 2000,
#   thin = 10,
#   seed = 1234,
#   file = 'tarsus_model'
# )


end_time <- Sys.time()

timetarsus = end_time - start_time
save(timetarsus, file = 'timetarsus.rda')

#the diagnostics from the sampler #### 

a1=tarsus
summary(a1)

#install.packages("bayesplot")

require(bayesplot)
#traceplots and posterior density distributions
plot(a1, n = 5, ask = F)

#effective sample size : 
ratios_cp <- neff_ratio(a1)
mcmc_neff(ratios_cp, size = 2)
names(ratios_cp [ratios_cp< 0.1])
#  1- should worry only if worry about any neff/N are less than 0.1.
#  
#One reason why we have such high ratios of neff to N is that the No-U-Turn sampler used by rstan generally produces draws from the posterior distribution with much lower autocorrelations compared to draws obtained using other MCMC algorithms (e.g., Gibbs)
 

lp=log_posterior(a1)
np=nuts_params(a1)

mcmc_nuts_acceptance(x=np,lp=lp)
#2- Acceptance
mcmc_nuts_divergence(x=np,lp=lp)
#3- distribution of the log-posterior; divergences often indicate that some part of the posterior isn’t being explored and the plot confirms that 
# lp|Divergence is supposed to have lighter tails than lp|No divergence. 

# If there are only a few divergences we can often get rid of them by increasing the target acceptance rate (adapt_delta, the upper limit is 1), which has the effect of lowering the step size used by the sampler and allowing the Markov chains to explore more complicated curvature in the target distribution (it also takes more time).


mcmc_nuts_energy(x=np)
#4- The plot created by mcmc_nuts_energy shows overlaid histograms of the (centered) # marginal energy distribution πE and the first-differenced distribution πΔE, The two histograms ideally look the same (Betancourt, 2017)


mcmc_nuts_treedepth(x=np,lp=lp)
# default maximum tree depth in Stan = 10.
# NUTS is an intelligent method to select the number of steps to take in each iteration.
#  However, there is still a maximum number of steps that NUTS will try. If the sampler is often hitting the maximum number of steps, it means that the optimal number of steps to take in each iteration is higher than the maximum. 
#  While divergent transitions bias inference, a too-small maximum tree-depth only affects efficiency. The sampler is still exploring the posterior distribution, but the exploration will be slower and the autocorrelation higher (effective sample size lower) than if the maximum tree-depth were set higher.

rhats <- rhat(a1)
mcmc_rhat(rhats)

#R- hats closest to 1.00 is what we want. 

#checking for autocorrelation in population-level posteriors
ps=posterior_summary(a1)
posterior_cp <- as.array(a1)
data.frame(rownames(ps))

require(rstan)
stan_ac(a1$fit, pars=c(rownames(ps[1:5,])))


#more precisely for each chain:

mcmc_acf(posterior_cp, pars = c(rownames(ps[1:5,])), lags = 10)

#Positive autocorrelation is bad (it means the chain tends to stay in the same area between iterations) and you want it to drop quickly to zero with increasing lag. Negative autocorrelation is possible and it is useful as it indicates fast convergence of sample mean towards true mean.

# Posterior predictive checks
pp_check( a1,  nsamples = 100)
pp_check( a1,  nsamples = 100, type='stat')
pp_check( a1,   type='hist')




