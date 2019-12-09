setwd("C:\\Users\\Mathilde\\Google Drive\\PhD drive\\Field work\\Summer 2018\\LME course")
getwd()

# The data contain moisture measurements and another "created" environmental variable of a vegetation experiment. 
# Moisture measurements are replicated 5 times in a plot, while the other covariate only once per plot. 
# There is 26 plots divided in 3 blocks and with 4 treatments (C, I, IW and W). 
# Measurements are repeaded throughout the summer in different rounds and over 2 years

### Packages ####
require(lme4)
require(lmerTest)
require(tidyr)
require(ggplot2)

### Data exporation ####
data<-read.delim("moisture_geek_lunch_MLM.txt");tail(data)
str(data)
data$year<-as.factor(data$year)
data$round<-as.factor(data$round)
data$plot<-as.factor(data$plot)

hist(data$soil_moisture_5)

#long format
data<-gather(data=data,key=order,value=moisture,soil_moisture_1,soil_moisture_2,soil_moisture_3,soil_moisture_4,soil_moisture_5)
head(data)
hist(data$moisture)
hist(data$cov1)

cor.test(data$moisture,data$cov1) # wrong degree of freedom, study design replications not considered. 
plot(data$moisture,data$cov1) # wrong level of replication

# Exploring the within and between variation
data18<-data[data$year=="2018",] # Only the data from this year
data19<-data[data$year=="2019",] # Only the data from this year

basic.plot<-qplot(data=data18,   #need to have pop as FACTOR
                  y= moisture,
                  ylab="soil moisture",
                  x=Day_of_year,
                  xlab="Treatment",
                  main="")
basic.plot<-basic.plot+aes(color=as.factor(treatment))
basic.plot
linear.smooth.plot<-basic.plot+
  geom_smooth(method="loess", se=F)
print(linear.smooth.plot)

basic.plot<-qplot(data=data18,   #need to have pop as FACTOR
                  y= cov1,
                  ylab="covariate 1",
                  x=Day_of_year,
                  xlab="Treatment",
                  main="")
basic.plot<-basic.plot+aes(color=as.factor(treatment))
basic.plot
linear.smooth.plot<-basic.plot+
  geom_smooth(method="loess", se=F)
print(linear.smooth.plot)

#### Linear mixed-effects model ####
# accounting for replicated measure

# Restricted Maximum Likelihood REML or Maximum Likelihood ML? 
# For conduction model selection on the random effects, you need REML=TRUE. For conducting model selection on fixed-effects you need REML=F.
# Once you have selected you model, final estimates can be calculated with REML=TRUE because "REML accounts for the degrees of freedom lost by estimating the fixed effects, and makes a less biased estimation of random effects variances" 
# From a forum: "The REML method uses a mathematical trick to make the estimates for the variance parameters independent of the estimates for the fixed effects. 
# REML works by first getting regression residuals for the observations modeled by the fixed effects portion of the model, ignoring at this point any variance components.
# ML estimates are unbiased for the fixed effects but biased for the random effects, whereas the REML estimates are biased for the fixed effects and unbiased for the random effects." Here they certainly mean biases in the variance component. 

### ~ random effects ####
# possible random effect model selection: REML=TRUE and keep the same fixed effects.
# Here we don't really want to do a model selection on random effects as we have a pre-defined study design structuring our repeated measures
lme1r<-lmer(moisture~treatment+(1|block/plot/order)+(1|year)+(1|round),data=data,REML=TRUE) # relative to intercept
lme2r<-lmer(moisture~treatment+(1|block/plot/order)+(1|round),data=data,REML=TRUE) # relative to intercept
lme3r<-lmer(moisture~treatment+(1|block/plot/order),data=data,REML=TRUE) # relative to intercept
AIC(lme1r)
AIC(lme2r)
AIC(lme3r)

## estimating fixed effects. Here we are not doing a model selection on fixed effects (which would have required REML=F) but interested we want to extract the models'parameters, se we keep REML=T. 
lme1<-lmer(moisture~treatment+(1|block/plot/order)+(1|year)+(1|round),data=data,REML=T) # relative to intercept
# failed to converge but not that bad, try another other optimizer. Exact same results
lme1<-lmer(moisture~treatment+(1|block/plot/order)+(1|year)+(1|round),data=data,REML=T,control=lmerControl(optimizer="bobyqa")) # relative to intercept
summary(lme1)

### ~ fixed effects ####

# We can remove the intercept to easily see the effect size of each tratments, but DO NOT conduct model selection like this. 
lme2<-lmer(moisture~-1+treatment+(1|block/plot/order)+(1|year)+(1|round),data=data,REML=T) # removing the intercept to extract the means, do not use for significance
summary(lme2)
Beta<-lme2@beta
SE<-sqrt(diag(vcov(lme2))) 
AL.tab<-data.frame(treatment=c("C","I","IW","W"),beta=Beta,beta.se=1.96*SE,low=Beta-1.96*SE,high=Beta+1.96*SE) # CI and not SE anymore!
# be careful that your factors are order the same way as in the lmer output

par(mar=(c(5,5,4,2)+0.1),bty="n")
plot(AL.tab$beta~AL.tab$treatment,ylim=c(20,40),xlab="",ylab="",pch=16,cex=2,lwd = 2,axes=F)
axis(1,cex.axis=2,lwd=0,at=c(1,2,3,4),labels = c("C","I","IW","W"))
axis(2,cex.axis=1.5,lwd=2)
mtext("Moisture %",2,cex=2,line=2.5)
arrows(x0=c(1,2,3,4),y0=AL.tab$beta-AL.tab$beta.se,x1=c(1,2,3,4),y1=AL.tab$beta+AL.tab$beta.se,lwd=2,code=3, length=0.02, angle = 90,col="grey")

#### Extracting fixed effects for correlation ####
lme3<-lmer(moisture~treatment*round+(1|block/plot/order),data=data18,REML=T) # removing the intercept to extract the means, do not use for significance
summary(lme3)

# the interaction sign : instead of * can be used to easily extract the coefficients but should not be used in a model selection
lme4<-lmer(moisture~-1+treatment:round+(1|block/plot/order),data=data18,REML=T) # removing the intercept to extract the means, do not use for significance
summary(lme4)
Beta<-lme4@beta
SE<-sqrt(diag(vcov(lme4))) 
rep_round<-rep(unique(sort(data$round)),each=length(unique(data$treatment)))
rep_TR<-rep(unique(sort(data$treatment)),length(unique(data$round)))
exTab<-data.frame(round=rep_round,treatment=rep_TR,betaM=Beta,betaM.se=SE);head(exTab)
# Be very careful to associate the correct round and treatment to the correct Beta value! 

lme5<-lmer(cov1~-1+treatment:round+(1|block/plot),data=data18,REML=T) # removing the intercept to extract the means, do not use for significance
summary(lme5)
Beta_c<-lme5@beta
SE_c<-sqrt(diag(vcov(lme5))) 
exTab$betaC<-Beta_c
exTab$betaC.se<-SE_c
head(exTab)

cor.test(exTab$betaM,exTab$betaC) # We are doing better with the degree of freedom! 
plot(exTab$betaM,exTab$betaC,xlab="Moisture %",ylab="Cov1")

### Confidence intervals with bootstrap ####
## Function 
## for PARAMETRIC BOOTSTRAP to get parameters CI from mixed models, insted of SE

mySumm <- function(.) {
  c(beta  = lme4::fixef(.)                       # fixed effect coefficients
    # , ranv  = as.data.frame(lme4::VarCorr(.))$vcov  # random effect variances
  )
}
# Function to extract parameter estimate confidence intervals
CI.tab <- function(b, original, ind=length(b$t0), type="perc", conf=0.95) {
  
  btab0 <- t(sapply(as.list(seq(ind)), function(i) boot::boot.ci(b, index = i, conf = conf, type = type)$percent))
  btab  <- btab0[,4:5]
  rownames(btab) <- names(b$t0)
  a <- (1 - conf)/2
  a <- c(a, 1 - a)
  pct <- stats:::format.perc(a, 3)
  btab <- cbind(btab[,1], original, btab[,2])
  colnames(btab) <- c(pct[1], "original", pct[2])
  
  return(btab)
}

summary(lme1)
summary(lme2)
# Run bootstrap
boot1 <- lme4::bootMer(lme1, mySumm, nsim = 100, .progress = "txt", seed = 101) # ideally do it with min 1000 iterations nsim.
boot2 <- lme4::bootMer(lme2, mySumm, nsim = 100, .progress = "txt", seed = 101) # ideally do it with min 1000 iterations nsim.

CI.tab(boot1, mySumm(lme1))# all CI include 0, none are significantly different than the control
CI.tab(boot2, mySumm(lme2))# all CI include 0, none are significantly different than the control

