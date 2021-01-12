
#setwd("insert your path here")

install.packages("titanic") # or here : https://www.rdocumentation.org/packages/titanic/versions/0.1.0

#code largely inspired from various notebooks found here :https://www.kaggle.com/c/titanic/notebooks?competitionId=3136&sortBy=voteCount


#how to get the modified dataset from which I'm working (or skip by loading the dataset from github at next section)


titan=data.frame(titanic::titanic_train)
#let's look at it:
summary(titan)
unique(titan$Ticket);length(unique(titan$Ticket)) 
unique(titan$Embarked);length(unique(titan$Embarked)) 
unique(titan$Cabin);length(unique(titan$Cabin)) 

#let's create additional variable from info available:

require(tidyverse)
#Homemade modified dataset (skip if you load from github) ####

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
    #                        title == "Master" ~ "Master")),
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

#load modified dataset####

titan2 <- readRDS("binomial_models/titanic_mod.rds")
summary(titan2) ; glimpse(titan2)
titan2 %>% dplyr::select_if(is.numeric) %>% 
ggplot(datad[!datad$predictors=='kyl',], aes(logit, predictor.value))+
  geom_point(size = 3, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
