
#setwd("insert your path here") 

#install.packages("titanic") # or here : https://www.rdocumentation.org/packages/titanic/versions/0.1.0

#code largely inspired from various notebooks found here :https://www.kaggle.com/c/titanic/notebooks?competitionId=3136&sortBy=voteCount

#Steps to obtain modified dataset ####

#how to get the modified dataset from which I'm working (or skip by loading the dataset from github at next section)


#titan=data.frame(titanic::titanic_train)
#let's look at it:
# summary(titan)
# unique(titan$Ticket);length(unique(titan$Ticket)) 
# unique(titan$Embarked);length(unique(titan$Embarked)) 
# unique(titan$Cabin);length(unique(titan$Cabin)) 
# 
# #let's create additional variable from info available:
# 
# require(tidyverse)
# titan$title <- gsub('(.*, )|(\\..*)', '', titan$Name)
# rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don',
#                 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
# misses <- c("Miss","Mlle", "Ms")
# 
# titan2 <- titan %>%
#   mutate(
#     sclass = factor(case_when(Pclass == 1 ~ "upper",
#                         Pclass == 2 ~ "middle",
#                         Pclass == 3 ~ "lower")),
#     embarked = factor(case_when( Embarked == "C" ~ "Cherbourg",
#                           Embarked == "Q" ~ "Queenstown",
#                           Embarked == "S" ~ "Southampton")),
#     title = factor(case_when (title %in% c("Miss","Mlle", "Ms") ~ "Miss",
#                        title  %in% c("Mme","Mrs") ~ "Mrs",
#                        title == "Mr" ~ "Mr",
#                        title %in% rare_title ~ "rare_title",
#                        title == "Master" ~ "Master"), levels=c("Miss", "Mrs", "Mr", "Master", "rare_title")),
#     fare = Fare,
#     sex = factor(Sex),
#     age = Age,
#     surv = Survived,
#     id = PassengerId,
#     name = Name)
# 
# titan2 = titan2 %>%
#   select(surv, sex, age, title, sclass, fare, embarked, id, name)
# titan2 = titan2[is.na(titan2$embarked)==F  & is.na(titan2$age)==F,]
# 
# saveRDS(titan2, "binomial_models/titanic_mod.rds")

#load modified dataset and visualize important dataset####

titan2 <- readRDS("binomial_models/titanic_mod.rds")
require(tidyverse)

summary(titan2) ; glimpse(titan2)

require(ggplot2)
ggplot(titan2)+
  geom_density(aes(age, fill=sex), alpha=0.5)+
  facet_wrap(surv ~ .) +
  theme_bw()

ggplot(titan2)+
  geom_density(aes(age, fill=factor(surv)), alpha=0.5)+
  facet_wrap(sex ~ .) +
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

#build model from earlier notebooks on kaggle.com####
require(glmmTMB)

mod=glmmTMB(surv~ age +
              sex +
              fare2 +
              (1|title), data=titan2, family='binomial')

a1=mod


#Check diagnostics ####
library(broom)

probabilities <- predict(a1, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)
#inverse logit: exp(x)/(1+exp(x))

#a- verify linearity of predictors:
#- Select only numeric predictorsfrom the original data frame 
datad <- titan2 %>%
  dplyr::select(age, fare2, surv) 
predictors <- colnames(datad)
#- Bind the logit and tidying the data for plot
datad <- datad %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
#- Create the scatter plots
ggplot(datad[!datad$predictors=='surv',], aes(logit, predictor.value))+
  geom_point(size = 3, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")#predictor is linear= fine, if not, will need to verify a transformation

#b- outliers (influential values)
source(system.file("other_methods","influence_mixed.R", package="glmmTMB"))
#ii <- influence_mixed(a1, groups=".case", ncores = (parallel::detectCores())-4) #takes some time, go check in binomial_models folder
png("binomial_models/ouliers_plot.png", width = 8, height = 11, res = 300, units = "in")
outliers_plot <- car::infIndexPlot(ii)
dev.off()

#c- plot standardized residuals
model.data <- broomExtra::augment(a1) %>% 
  mutate(index = 1:n())

ggplot(model.data, aes(index, .resid)) + 
  geom_point(aes(color = as.factor(surv)), alpha = .8, size=2) +
  theme_bw()

#d- Filter potential influential data points with abs(.std.res) > 3:
model.data %>% 
  filter(abs(.resid) > 3)#no influential data points

#e- verify multicollinearity; a VIF value that exceeds 5 or 10 indicates a problematic amount of collinearity.
performance::check_collinearity(a1)#seems ok!


#one way to correct for non-linearity of predictors: likelihood ratio tests and polynomial functions. 
require(splines)

mod=glmmTMB(surv~ age +
              sex +
              fare2 +
              (1|title), data=titan2, family='binomial')
mod2=glmmTMB(surv~ poly(age, df=2) +
              sex +
              fare2 +
              (1|title), data=titan2, family='binomial')
mod3=glmmTMB(surv~ bs(age, 3) +
              sex +
              fare2 +
              (1|title), data=titan2, family='binomial')
mod4=glmmTMB(surv~ bs(age,4) +
              sex +
              fare2 +
              (1|title), data=titan2, family='binomial')
mod5=glmmTMB(surv~ bs(age,5) +
               sex +
               fare2 +
               (1|title), data=titan2, family='binomial')

anova(mod, mod2, mod3, mod4, mod5)# would need a bs(4) for age 

mod6=glmmTMB(surv~ poly(fare2, df=2) +
               sex +
               age +
               (1|title), data=titan2, family='binomial')
mod7=glmmTMB(surv~ bs(fare2, 3) +
               sex +
               age +
               (1|title), data=titan2, family='binomial')
mod8=glmmTMB(surv~ bs(fare2,4) +
               sex +
               age +
               (1|title), data=titan2, family='binomial')
mod9=glmmTMB(surv~ bs(fare2,5) +
               sex +
               age +
               (1|title), data=titan2, family='binomial')
anova(mod, mod6, mod7, mod8, mod9)# would need a poly(df=2) for fare2


modb=glmmTMB(surv~ bs(age,4) +
                      sex +
                      poly(fare2, df=2) +
                      (1|title), data=titan2, family='binomial')

a1=modb# one would need to check that assumptions are still met. for the purpose of this geek-lunch, we are going fast. 

datad <- titan2 %>%
  dplyr::select(age, fare2, surv) 
predictors <- colnames(datad)
#- Bind the logit and tidying the data for plot
datad <- datad %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
#- Create the scatter plots
ggplot(datad[!datad$predictors=='surv',], aes(logit, predictor.value))+
  geom_point(size = 3, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")#not really better... Alternatives : Gamms in gamm4 packages (uses lme4 in its back-end) ? or categories?. 

#Categories for age:
titan2 <- titan2 %>% 
  mutate(
    age_clust= factor(case_when(age<10 ~ "young",
                         age <18 & age >9 ~ "teen",
                         age >17 & age <60 ~ "grown-up",
                         age > 59 ~ "ancient"))
  )

modc=glmmTMB(surv~ age_clust +
                 sex +
                 poly(fare2, df=2) +
                 (1|title), data=titan2, family='binomial') #check back assumptions. In this case, one would need to do further checks for fare variables options. 
summary(modc)

#figures

new.data=data.frame(expand.grid(age_clust=factor(c("ancient", "grown-up", "teen", "young")),
                     fare2=median(titan2$fare2),
                     sex=factor(c("female" , "male")),
                     title=NA,
                     surv=NA))
pred=predict(modc, new.data, type="link", se.fit = T, re.form = NA)

new.data$fit=pred$fit; new.data$se=pred$se.fit

#Age cluster
ageplot <- ggplot(new.data[new.data$sex=="female",], aes(y=(exp(fit)/(1+exp(fit))), x=age_clust))+
  
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

#Sex
sexplot <- ggplot(new.data[new.data$age_clust=="grown-up",], aes(y=(exp(fit)/(1+exp(fit))), x=sex))+
  
  geom_point(color='seagreen3', size=4)+
  
  geom_errorbar(aes(ymin=(exp(fit-se*1.96)/(1+exp(fit-se*1.96))),
                    ymax=(exp(fit+se*1.96)/(1+exp(fit+se*1.96)))),
                width=00, size=1, color='seagreen3') +
  
  scale_y_continuous(breaks=seq(from=0, to=1, by=.1), 
                     limits=c(-0.05,1.05), 
                     name="P(survival)")+
  scale_x_discrete(name="Sex")+
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
                                sex=factor(c("female" , "male")),
                                title=NA,
                                surv=NA))
new.data=new.data[new.data$age_clust=="grown-up" & new.data$sex=="male", ]

pred=predict(modc, new.data, type="link", se.fit = T, re.form = NA)
new.data$fit=pred$fit; new.data$se=pred$se.fit

fareplot <- ggplot(new.data, aes(y=(exp(fit)/(1+exp(fit))), x=fare2))+

  geom_ribbon(aes(ymin=(exp(fit-se*1.96)/(1+exp(fit-se*1.96))),
                    ymax=(exp(fit+se*1.96)/(1+exp(fit+se*1.96)))),
                 size=1, fill='seagreen3', alpha=0.5) +
  
  geom_line(aes(y=(exp(fit)/(1+exp(fit))), x=fare2))+
  
  geom_point(data=titan2[titan2$age_clust=="grown-up" & titan2$sex=="male", ], 
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
        axis.text.x = element_text(color="black", size=9),#, angle=90,vjust=.4
        axis.title.x = element_text(color="black", size=12, face = "bold"),
        axis.title.y = element_text(color="black", size=12, face = "bold"),
        legend.position = 'top',
        legend.title = element_blank(),
        legend.text= element_text(size=10),
        legend.key = element_blank(), 
        legend.background = element_blank())
require(ggpubr)
(combined <- ggarrange(oddsratioplot, ageplot, sexplot, fareplot,labels = c("A)", "B)", "C)", "D)")))
ggsave(combined, filename = "binomial_models/predictions_plot.png", width = 6, height = 8,dpi="retina")
