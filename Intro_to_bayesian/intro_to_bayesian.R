
#Working directory and base data 
setwd("C:/yourpath/workshops/Intro_to_bayesian")# Flo

btdata=read.delim("BTData.txt", sep = " ", header=T, dec = "." )
#or data("BTdata", package = "MCMCglmm")

head(btdata) 
str(btdata)
summary(btdata)
