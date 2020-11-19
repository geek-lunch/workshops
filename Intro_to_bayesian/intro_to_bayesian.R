
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

# Install RStan ####
install.packages("devtools")
library(devtools)
Sys.setenv(PATH = paste("C:/Rtools/bin", Sys.getenv("PATH"), sep=";"))
Sys.setenv(PATH = paste("C:/Rtools/mingw_64/bin", Sys.getenv("PATH"), sep=";"))

install.packages("rstan")
library(rstan)

# Load Stan file ####
fit = stan("workshops/Intro_to_bayesian/bayesian_model.stan", model_name = "bayesian_model", data = list())
stan_code <- readChar(bayesian_model, file.info(bayesian_model)$size)
cat(stan_code)

# Run Stan ####
fit = stan(model_code = bayesian_model.stan, data = list(tarsus = BTdata$tarsus, n_obs = 828), iter = 5000)

data_for_stan_model <- list(n_obs = nrow(BTdata), n_dam = nrow(dam_ID), tarsus = BTdata$tarsus, sex_no = BTdata$sex_no, dam_no = BTdata$dam_no)
tarsus_model <- stan_model("Intro_to_bayesian/bayesian_model.stan")
result <- sampling(tarsus_model, data = data_for_stan_model)
