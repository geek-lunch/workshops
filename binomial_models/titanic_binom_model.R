
#Jan 2021 - Binomial models and the titanic dataset. 
#Geek-lunch lead by F. Déry. 
#
#Packages used : #titanic, glmmTMB, splines, broom, ggplot2, ggpubr, and the metapackage tidyverse.

#****USEFUL LINKS****:
#-  A detailed example by Ben Bolker : 
      # https://bbolker.github.io/mixedmodels-misc/ecostats_chap.html#culcita
#-  Code and models largely inspired from various notebooks found here :
      # https://www.kaggle.com/c/titanic/notebooks?competitionId=3136&sortBy=voteCount 
#-  about Splines (polynomial functions) and overfitting.  
      # https://stats.stackexchange.com/questions/49052/are-splines-overfitting-the-data 

#setwd("insert your path to workshops here") 

#install.packages("titanic") # or here : https://www.rdocumentation.org/packages/titanic/versions/0.1.0


#Steps to obtain modified dataset ####

#how to get the modified dataset from which I'm working (or skip by loading the dataset from github at next section)


titan=data.frame(titanic::titanic_train)
#let's look at it:
summary(titan)
unique(titan$Ticket);length(unique(titan$Ticket))
unique(titan$Embarked);length(unique(titan$Embarked))
unique(titan$Cabin);length(unique(titan$Cabin))

#let's create additional variable from info available:

require(tidyverse)
titan$title <- gsub('(.*, )|(\\..*)', '', titan$Name)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don',
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
misses <- c("Miss","Mlle", "Ms")

titan2 <- titan %>%
  mutate(
    sclass = factor(case_when(Pclass == 1 ~ "upper",
                        Pclass == 2 ~ "middle",
                        Pclass == 3 ~ "lower")),
    embarked = factor(case_when( Embarked == "C" ~ "Cherbourg",
                          Embarked == "Q" ~ "Queenstown",
                          Embarked == "S" ~ "Southampton")),
    title = factor(case_when (title %in% c("Miss","Mlle", "Ms") ~ "Miss",
                       title  %in% c("Mme","Mrs") ~ "Mrs",
                       title == "Mr" ~ "Mr",
                       title %in% rare_title ~ "rare_title",
                       title == "Master" ~ "Master"), levels=c("Miss", "Mrs", "Mr", "Master", "rare_title")),
    fare = Fare,
    gender = factor(Sex),
    age = Age,
    surv = Survived,
    id = PassengerId,
    name = Name)

titan2 = titan2 %>%
  select(surv, gender, age, title, sclass, fare, embarked, id, name)
titan2 = titan2[is.na(titan2$embarked)==F  & is.na(titan2$age)==F,]

saveRDS(titan2, "binomial_models/titanic_mod.rds")

#Load modified dataset and visualize important dataset####

titan2 <- readRDS("binomial_models/titanic_mod.rds")
require(tidyverse)

summary(titan2) ; glimpse(titan2)

require(ggplot2)
ggplot(titan2)+
  geom_density(aes(age, fill=gender), alpha=0.5)+
  facet_wrap(surv ~ .) +
  theme_bw()

ggplot(titan2)+
  geom_density(aes(age, fill=factor(surv)), alpha=0.5)+
  facet_wrap(gender ~ .) +
  theme_bw()

ggplot(titan2, aes(x=title, fill=factor(surv)))+
  geom_bar(position = "dodge")

ggplot(titan2, aes(x=title, y=age, fill=factor(surv)))+
  geom_boxplot(position = "dodge",varwidth = TRUE)

ggplot(titan2)+
  geom_density(aes(fare, fill=factor(surv)), alpha=0.5)+
  theme_bw()

#let'S clump fare 200 and over together.
titan2= titan2 %>% mutate(
  fare2=case_when(titan2$fare>200~ 200, 
                  titan2$fare<201~titan2$fare)
)
ggplot(titan2)+
  geom_density(aes(fare2, fill=factor(surv)), alpha=0.5)+
  theme_bw()
#____________________________________________________________________________________
#Build model from earlier notebooks on kaggle.com####
require(glmmTMB)

mod=glmmTMB(surv~ age +
              gender +
              fare2 +
              (1|title), data=titan2, family='binomial')
a1=mod
#Assumptions to check for a binomial model : 

#- 1. There is a Linear Relationship Between Explanatory Variables and the Logit of the Response Variable
#- 2. There are No Extreme Outliers
#- 3. The Observations are Independent (look at residuals and order of observations)
#- 4. There is No Multicollinearity Among Explanatory Variables

#____________________________________________________________________________________
#Check diagnostics ####
library(broom)

probabilities <- predict(a1, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)
#inverse logit: exp(x)/(1+exp(x))

#1a- verify linearity of predictors with logit if gender and age are not in interaction:
#- Select only numeric predictorsfrom the original data frame 
datad <- titan2 %>%
  dplyr::select(age, fare2, surv) 
predictors <- colnames(datad)
#- Bind the logit and tidying the data for plot
datad <- datad %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
#- Create the scatter plots
ggplot(datad[!datad$predictors=='surv',], aes(y=logit, x=predictor.value))+
  geom_point(size = 3, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_x")#predictor is linear= fine, if not, will need to verify a transformation

#Variables may be non-linear with logit due to a missing interaction, or may need a polynomial function, or a spline. 

#When it comes down to splines and polynomials, likelihood ratio tests can be used to asses which splines needs to be used, but while doing this, make sure you are not overfitting... see this excellent discussion : https://stats.stackexchange.com/questions/49052/are-splines-overfitting-the-data

#Additionnally, have a look to GAMs (generalized additive models), which relaxes the assumption about linearity. 

#Suppose one built a model with an interaction of gender and age, it would need to do that: 
modb=glmmTMB(surv~ age +
              gender +
              fare2 +
              (1|title), data=titan2, family='binomial')
a1=modb
probabilities <- predict(a1, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)
#1a- verify linearity of predictors with logit if there is an interaction with gender and age:
  #- Select only numeric predictors from the original data frame 
  datad <- titan2; datad$probabilities= probabilities
(predictors <- colnames(datad))
#- Bind the logit and tidying the data for plot
datad <- datad[datad$gender=="female",] %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  dplyr::select(age, fare2, surv, logit) %>% 
  gather(key = "predictors", value = "predictor.value", -logit)
#- Create the scatter plots
ggplot(datad[!datad$predictors=='surv' ,], aes(x= predictor.value, y=logit))+
  geom_point(size = 3, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_x")#predictor is linear= fine, if not, will need to verify a transformation. Would need to be repeated for males. 


#2- outliers (influential values) ... 

#For large sample sizes, a rough guideline is to consider 
# - Cook's distance values above 1 to indicate highly influential points 
# - Leverage values greater than 2 times the number of predictors divided by the sample size to indicate high leverage observations** (in this case, (2*3)/712=0.0084). 
    #**High leverage observations are ones which have predictor values very far from their averages, which can greatly influence the fitted model. 
    
#for glmmTMB, influence.mermod() doesn't work, but authors have hacked some packages and built a turn around, but it takes time. 
source(system.file("other_methods","influence_mixed.R", package="glmmTMB"))
ii <- influence_mixed(a1, groups=".case", ncores = (parallel::detectCores())-4) #takes some time, go check in binomial_models folder
outliers_plot <- car::infIndexPlot(ii)

#3- Are the Observations Independent (look at residuals and order of observations)
#a- plot standardized residuals
model.data <- broom::augment(a1) %>% 
  dplyr::mutate(index = 1:n())

ggplot(model.data, aes(x=index, y=.resid)) + 
  geom_point(aes(color = as.factor(surv)), alpha = .8, size=2) +
  theme_bw()

#b- Filter potential influential data points with abs(.std.res) > 3:
model.data %>% 
  filter(abs(.resid) > 3)#no influential data points

#4- verify multicollinearity; a VIF value that exceeds 5 or 10 indicates a problematic amount of collinearity.
performance::check_collinearity(a1) #seems ok! One could also look at correlogramms before modelling, between 
#_____________________________________________________________________________________
#Correcting for non-linearity with logit values ####

#here, we are assessing the possibility for a spline, but remember we should 
require(splines)

mod=glmmTMB(surv~ age +
              gender +
              fare2 +
              (1|title), data=titan2, family='binomial')
mod2=glmmTMB(surv~ poly(age, df=2) +
              gender +
              fare2 +
              (1|title), data=titan2, family='binomial')
mod3=glmmTMB(surv~ bs(age, 3) +
              gender +
              fare2 +
              (1|title), data=titan2, family='binomial')
mod4=glmmTMB(surv~ bs(age,4) +
              gender +
              fare2 +
              (1|title), data=titan2, family='binomial')
mod5=glmmTMB(surv~ bs(age,5) +
               gender +
               fare2 +
               (1|title), data=titan2, family='binomial')

anova(mod, mod2)# DOES NOT REALLY HELP ! we stop there if we are purist in testing the way splines should be tested. however, not everyone is purist...  
anova(mod2,mod3)#we would do this if poly(df=2) would have brought better fit. One also must make sure that deviance, likelihood etc. realy improves by adding a knot to a spline... otherwise you are overfitting. this must be assesed by looking at anova results, but also visually
anova(mod3, mod4)# (sic)
anova(mod, mod4)

#For this example, we are only gonna continue with a category afterward, as the non-linearity is mostly due to no interaction between age and gender. 


mod6=glmmTMB(surv~ poly(fare2, df=2) +
               gender +
               age +
               (1|title), data=titan2, family='binomial')
mod7=glmmTMB(surv~ bs(fare2, 3) +
               gender +
               age +
               (1|title), data=titan2, family='binomial')
mod8=glmmTMB(surv~ bs(fare2,4) +
               gender +
               age +
               (1|title), data=titan2, family='binomial')
mod9=glmmTMB(surv~ bs(fare2,5) +
               gender +
               age +
               (1|title), data=titan2, family='binomial')
anova(mod, mod6, mod7, mod8, mod9)# would need a poly(df=2) for fare2, but see notes above and the possibility about an interaction 


modb=glmmTMB(surv~ age +
                      gender +
                      poly(fare2, df=2) +
                      (1|title), data=titan2, family='binomial')

a1=modb# one would need to check that assumptions are still met. for the purpose of this geek-lunch, we are going fast. 

probabilities <- predict(a1, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

datad <- titan2 %>%
  dplyr::select(age, fare2, surv) 
predictors <- colnames(datad)
#- Bind the logit and tidying the data for plot
datad <- datad %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
#- Create the scatter plots
ggplot(datad[!datad$predictors=='surv',], aes(y=logit, x=predictor.value))+
  geom_point(size = 3, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_x")#not really better... Alternatives : categories, interactions, and really if needed, Gamms in gamm4 packages (uses lme4 in its back-end) ?

#Categories for age:
titan2 <- titan2 %>% 
  mutate(
    age_clust= factor(case_when(age<10 ~ "young",
                         age <18 & age >9 ~ "teen",
                         age >17 & age <60 ~ "grown-up",
                         age > 59 ~ "ancient"))
  )

modc=glmmTMB(surv~ age_clust +
                 gender +
                 poly(fare2, df=2) +
                 (1|title), data=titan2, family='binomial') #check back assumptions. In this case, one would need to do further checks for fare variables options. 
summary(modc)

#Ways to visualize results####

new.data=data.frame(expand.grid(age_clust=factor(c("ancient", "grown-up", "teen", "young")),
                     fare2=median(titan2$fare2),
                     gender=factor(c("female" , "male")),
                     title=NA,
                     surv=NA))
pred=predict(modc, new.data, type="link", se.fit = T, re.form = NA)

new.data$fit=pred$fit; new.data$se=pred$se.fit

#Age cluster
ageplot <- ggplot(new.data[new.data$gender=="female",], aes(y=(exp(fit)/(1+exp(fit))), x=age_clust))+
  
  geom_point(color='seagreen3', size=4)+
  
  geom_errorbar(aes(ymin=(exp(fit-se*1.96)/(1+exp(fit-se*1.96))),
                    ymax=(exp(fit+se*1.96)/(1+exp(fit+se*1.96)))),
                width=00, size=1, color='seagreen3') +
  
  scale_y_continuous(breaks=seq(from=0, to=1, by=.1), 
                     limits=c(-0.05,1.05), 
                     name="P(survival)")+
  scale_x_discrete(name="Age category")+
  theme(panel.background = element_rect(fill = "white",colour = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour="black", linetype="solid"),
        axis.line.y = element_line(colour="black", linetype="solid"),
        axis.text.y = element_text(color="black", size=9),
        axis.text.x = element_text(color="black", size=9), 
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))

#gender
genderplot <- ggplot(new.data[new.data$age_clust=="grown-up",], aes(y=(exp(fit)/(1+exp(fit))), x=gender))+
  
  geom_point(color='seagreen3', size=4)+
  
  geom_errorbar(aes(ymin=(exp(fit-se*1.96)/(1+exp(fit-se*1.96))),
                    ymax=(exp(fit+se*1.96)/(1+exp(fit+se*1.96)))),
                width=00, size=1, color='seagreen3') +
  
  scale_y_continuous(breaks=seq(from=0, to=1, by=.1), 
                     limits=c(-0.05,1.05), 
                     name="P(survival)")+
  scale_x_discrete(name="gender")+
  theme(panel.background = element_rect(fill = "white",colour = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour="black", linetype="solid"),
        axis.line.y = element_line(colour="black", linetype="solid"),
        axis.text.y = element_text(color="black", size=9),
        axis.text.x = element_text(color="black", size=9), 
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))


#Fare price
new.data=data.frame(expand.grid(age_clust=factor(c("ancient", "grown-up", "teen", "young")),
                                fare2=seq(from=0,200, by=1),
                                gender=factor(c("female" , "male")),
                                title=NA,
                                surv=NA))
new.data=new.data[new.data$age_clust=="grown-up" & new.data$gender=="male", ]

pred=predict(modc, new.data, type="link", se.fit = T, re.form = NA)
new.data$fit=pred$fit; new.data$se=pred$se.fit

fareplot <- ggplot(new.data, aes(y=(exp(fit)/(1+exp(fit))), x=fare2))+

  geom_ribbon(aes(ymin=(exp(fit-se*1.96)/(1+exp(fit-se*1.96))),
                    ymax=(exp(fit+se*1.96)/(1+exp(fit+se*1.96)))),
                 size=1, fill='seagreen3', alpha=0.5) +
  
  geom_line(aes(y=(exp(fit)/(1+exp(fit))), x=fare2))+
  
  geom_point(data=titan2[titan2$age_clust=="grown-up" & titan2$gender=="male", ], 
             aes(x=fare2, y=surv),
             color='black', size=2, shape=21)+
  
  scale_y_continuous(breaks=seq(from=0, to=1, by=.1), 
                     limits=c(-0.05,1.05), 
                     name="P(survival)")+
  scale_x_continuous(breaks=seq(from=0, to=200, by=25), 
                     limits=c(0,200),
                     name="Ticket price ($)", )+
  theme(panel.background = element_rect(fill = "white",colour = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour="black", linetype="solid"),
        axis.line.y = element_line(colour="black", linetype="solid"),
        axis.text.y = element_text(color="black", size=9),
        axis.text.x = element_text(color="black", size=9), 
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))

#Odds ratio plot


efdf = tidy(modc, conf.int = TRUE, exponentiate = TRUE) 
efdf=efdf[efdf$effect=="fixed" & !efdf$term %in% c("poly(fare2, df = 2)2", "poly(fare2, df = 2)1"),]  
efdf

oddsratioplot <- ggplot(efdf, aes(y = estimate, x = term)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 1.2) +
  geom_hline(yintercept = 1.0, linetype = "dotted", size = 1) +
  scale_y_log10(breaks = c(0, 0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10), 
                minor_breaks = NULL) +
  labs(y = "Odds ratio", x = "Effect") +
  coord_flip() +
  theme(panel.background = element_rect(fill = "white",colour = "white", size = 0.5, linetype = "solid"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour="black", linetype="solid"),
        axis.text.y = element_text(color="black", size=9),
        axis.text.x = element_text(color="black", size=9),
        axis.title.x = element_text(color="black", size=12, face = "bold"),
        axis.title.y = element_text(color="black", size=12, face = "bold"),
        legend.position = 'top',
        legend.title = element_blank(),
        legend.text= element_text(size=10),
        legend.key = element_blank(), 
        legend.background = element_blank())

require(ggpubr)
(combined <- ggarrange(oddsratioplot,
                       ageplot,
                       genderplot, 
                       fareplot,
                       labels = c("A)", "B)", "C)", "D)")))

ggsave(combined, filename = "binomial_models/predictions_plot.png", width = 6, height = 8,dpi="retina")
