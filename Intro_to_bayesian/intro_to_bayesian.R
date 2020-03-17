
#Working directory and base data 
setwd("C:/yourpath/workshops/Intro_to_bayesian")# Flo

BTdata=read.delim("BTData.txt", sep = " ", header=T, dec = "." )
#or data("BTdata", package = "MCMCglmm")

head(BTdata)
str(BTdata)
summary(BTdata)

# Convert factor to integer ####
library(dplyr)

BTdata$sex_no <- as.integer((BTdata$sex))

BTdata$dam_no <- as.integer((BTdata$dam))
dam_ID <- distinct(BTdata, dam_no, dam) %>%
  arrange(dam_no)

# Load Stan file ####
bayesian_model <- "C:/R_GitHub/workshops/Intro_to_bayesian/bayesian_model.stan"
stan_code <- readChar(bayesian_model, file.info(bayesian_model)$size)
cat(stan_code)

# Run Stan ####
library(rstan)

resStan <- stan(model_code = stan_code, data = BTdata,
                chains = 3, iter = 3000, warmup = 500, thin = 10)
