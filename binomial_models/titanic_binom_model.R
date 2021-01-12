
#setwd("insert your path here")

install.packages("titanic") # or here : https://www.rdocumentation.org/packages/titanic/versions/0.1.0

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

#build model from earlier notebooks####
require(glmmTMB)

mod=glmmTMB(surv~ age +
              sex +
              fare +
              (1|title), data=titan2, family='binomial')

a1=mod

library(broom)

probabilities <- predict(a1, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)
#inverse logit: exp(x)/(1+exp(x))

#a- verify linearity of predictors:
#- Select only numeric predictorsfrom the original data frame 
datad <- titan2 %>%
  dplyr::select(age, fare, surv) 
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
ii <- influence_mixed(a1, groups=".case", ncores = (parallel::detectCores())-4)#takes some time
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
              fare +
              (1|title), data=titan2, family='binomial')
mod2=glmmTMB(surv~ poly(age, df=2) +
              sex +
              fare +
              (1|title), data=titan2, family='binomial')
mod3=glmmTMB(surv~ bs(age, 3) +
              sex +
              fare +
              (1|title), data=titan2, family='binomial')
mod4=glmmTMB(surv~ bs(age,4) +
              sex +
              fare +
              (1|title), data=titan2, family='binomial')
mod5=glmmTMB(surv~ bs(age,5) +
               sex +
               fare +
               (1|title), data=titan2, family='binomial')

anova(mod, mod2, mod3, mod4, mod5)# would need a bs(4) for age 

mod2=glmmTMB(surv~ poly(fare, df=2) +
               sex +
               age +
               (1|title), data=titan2, family='binomial')
mod3=glmmTMB(surv~ bs(fare, 3) +
               sex +
               age +
               (1|title), data=titan2, family='binomial')
mod4=glmmTMB(surv~ bs(fare,4) +
               sex +
               age +
               (1|title), data=titan2, family='binomial')
mod5=glmmTMB(surv~ bs(fare,5) +
               sex +
               age +
               (1|title), data=titan2, family='binomial')
anova(mod, mod2, mod3, mod4, mod5)# would need a bs(3) for fare
anova(mod2, mod3)

modb=glmmTMB(surv~ bs(age,4) +
                      sex +
                      bs(fare, 3) +
                      (1|title), data=titan2, family='binomial')

a1=modb
datad <- titan2 %>%
  dplyr::select(age, fare, surv) 
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
  facet_wrap(~predictors, scales = "free_y")#not really better... Alternatives : Gamms ? or categories. 

#Categories:
titan2 <- titan2 %>% 
  mutate(
    age_clust= factor(case_when(age<10 ~ "young",
                         age <18 & age >9 ~ "teen",
                         age >17 & age <60 ~ "grown-up",
                         age > 59 ~ "ancient"))
  )

modc=glmmTMB(surv~ age_clust +
                 sex +
                 bs(fare, 3) +
                 (1|title), data=titan2, family='binomial')
summary(modc)

#figures
