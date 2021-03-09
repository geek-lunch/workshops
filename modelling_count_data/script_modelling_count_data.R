#Reorganising data
##Packages
library(readxl)
library(dplyr)

##Download data
data <- read_xlsx("modelling_count_data/data_to_organize.xlsx")

##Organize the current dataframe
data <- mutate(data, Site = as.factor(Site),
               Plot = as.factor(Plot))  # Changes those variables from num/chr to factor.

data <- data %>%
  mutate(Height_class = cut(Height_cm,
                        breaks = c(1, 10, 30, Inf),
                        labels = c("1-10", "11-30", ">30")))  # Adds a column to your dataframe where the height class
                                                              # is specified (1-10 cm, 16-30 cm, or more than 30 cm).
data_summary <- data %>%
  group_by(Site, Plot, Height_class) %>%
  summarize(n_line = n())  # Summarizes your dataframe by site, plot and height class.

##Create empty dataframe
empty_df <- expand.grid(
  Site = levels(data$Site),
  Plot = levels(data$Plot),
  Height_class = levels(data$Height_class)
) # Creates an empty dataframe with one column for each of the variable of interest.
  # As those variables are factors, you have to specify the possible levels for each of them.

##Fill in the blanks of the empty dataframe with your data
reorganized_data <- right_join(data_summary, empty_df,
                               by = c("Site", "Plot", "Height_class"))  # Return all rows from empty_df, and all columns
                                                                        # from data_summary and empty_df. Rows in data_summary 
                                                                        # with no match in empty_df will have NA values in the new columns.

names(reorganized_data)[names(reorganized_data) == "n_line"] <- "freq"  # Changes the name of the column
                                                                        # "n_line" to "freq".


reorganized_data$freq[is.na(reorganized_data$freq)] = 0  # Changes all NAs in the freq column to 0.


#Choosing the right distribution
##Packages
library(glmmTMB)
library(lme4)
library(ggplot2)

##Data
data(Owls)  # This dataframe is included in the glmmTMB package.
str(Owls)  # The dependent variable, SiblingNegotiation, represents the number of calls emitted by chicks in a nest awaiting food, 
           # depending on their level of hunger (FoodTreatment), the sex of the parent foraging (SexParent), and the arrival time 
           # of that parent (ArrivalTime). Since repeated measurements have been taken on each nest, Nest is a random effect. 
           # Finally, since the number of chicks (BroodSize) varies in each nest, this variable will be used as offset in order to 
           # model the number of calls per chick.

##Histogram
ggplot(Owls, aes(x = SiblingNegotiation)) +
  geom_histogram(color = "white") +
  scale_y_continuous(expand = c(0, 0))  # Histogram of the dependent variable

##Model
###Poisson distribution
owls_p <- glmer(SiblingNegotiation ~ FoodTreatment * SexParent + ArrivalTime +
                  (1|Nest) + offset(logBroodSize), data = Owls, family = poisson)  # Model with Poisson distribution

chi2 <- sum(residuals(owls_p, type = "pearson")^2)  # Calculating Chi2 value to test if there is a dispersion problem
chi2 / df.residual(owls_p)  # Calculating the dispersion coefficient
1 - pchisq(chi2, df = df.residual(owls_p))  # Calculating the p-value

  # Note here that the Chi2 test is unilateral, so it only tests for overdispersion. However, underdispersion is less common and generally
  # has less influence on the model estimates. A "normal" dispersion coefficient is around 1 : a lower coefficient would be a sign of
  # underdispersion, while a greater one would indicate overdispersion. A p-value lower than 0.05 means that your data are significantly
  # overdispersed. A p-value really close to 1 can indicate over-parameterization.
  # In the present case, the dispersion coefficient is approx. 5.5, and the p-value is almost 0. This indicates that we have overdispersion.
  # To address this problem, we will rerun the model, but with a negative binomial distribution.

###Negative binomial distribution
owls_nb <- glmmTMB(SiblingNegotiation ~ FoodTreatment * SexParent + ArrivalTime +
                     (1|Nest) + offset(logBroodSize),
                   family = nbinom2, data = Owls)  # Model with negative binomial distribution

chi2 <- sum(residuals(owls_nb, type = "pearson")^2)  # Calculating Chi2 value to test if there is a dispersion problem
chi2 / df.residual(owls_nb)  # Calculating the dispersion coefficient
1 - pchisq(chi2, df = df.residual(owls_p))  # Calculating the p-value

  # The dispersion coefficient is near 1 and the p-value is > 0.05, so the negative binomial distribution seams to do the trick! 
  # We can now take a look at the model estimates

summary(owls_nb)

  # The quasi-Poisson distribution can handle both under- and over-dispersion. We can rerun the model with this distribution just for
  # fun, and also to know how to use it if you ever have underdispersion in your data. However, this distribution can't be handled by
  # the lme4 nor glmmTMB packages. We will have to use a custom function to adjust the coefficient table by multiplying the standard 
  # error by the square root of the dispersion factor^2 and recomputing the Z- and p-values accordingly

###Quasi-poisson distribution
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail=FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}  # This is a custom function to calculate overdispersion. It will be used is the next function.
   # You can use this function to test for overdispersion [overdisp_fun(your_model)], but I don't
   # like to use it because it doesn't work with model calculated with glmmTMB.

quasi_table <- function(model, ctab = coef(summary(model)),
                        phi = overdisp_fun(model)["ratio"]) {
  qctab <- within(as.data.frame(ctab),
                  {   `Std. Error` <- `Std. Error` * sqrt(phi)
                  `z value` <- Estimate/`Std. Error`
                  `Pr(>|z|)` <- 2 * pnorm(abs(`z value`), lower.tail = FALSE)
                  })
  return(qctab)
}  # This is the custom function to adjust your estimates.

printCoefmat(quasi_table(owls_p), digits = 3)  # This applies the quasi_table function to your estimates calculated with 
                                               # the Poisson distribution.

  # As you can see, the estimates are not very different when using the negative binomial or quasi-poisson distribution. However,
  # as the quasi_table function only produces a table of the adjusted estimates, it may be easier to work with the glmmTMB function
  # using the negative binomial distribution, which creates an object.
  # If the data were underdispersed, you can use both quasi-Poisson and generalized Poisson distributions.

###Generalized Poisson distribution (DO NOT USE, because those data ARE NOT UNDERDISPERSED. This is just an example.)
owls_genpois <- glmmTMB(SiblingNegotiation ~ FoodTreatment * SexParent + ArrivalTime +
                          (1|Nest) + offset(logBroodSize),
                        family = genpois(link = "log"), data = Owls)


#Testing for an excess of zeros
##Packages
library(glmmTMB)
library(dplyr)
library(ggplot2)

##Model visualization with 95% confidence intervals
sim_owls_nb <- simulate(owls_nb, nsim = 1000, re.form = NULL, newdata = Owls)  # Data simulation based on the nb model

owls_pred <- mutate(Owls, pred = predict(owls_nb, type = "response"),
                    q025 = apply(sim_owls_nb, 1, quantile, probs = 0.025),
                    q975 = apply(sim_owls_nb, 1, quantile, probs = 0.975)) %>%
  arrange(pred)  # Calculates the confidence intervals

ggplot(owls_pred, aes(x = 1:nrow(owls_pred), y = pred, ymin = q025, ymax = q975)) +
  geom_ribbon(alpha = 0.3) +
  geom_line() +
  geom_point(aes(y = SiblingNegotiation))  # Produces a graph with the intervals in gray, a line representing the simulated mean
                                           # and the real data as black dots. As you can see, even if the CI contains the large
                                           # majority of the dots, it seems too narrow on the left of the graph and too large
                                           # on the right.

## Numerical method to test if there is an excess of zero
nb_zeros <- apply(sim_owls_nb, 2, function(x) sum(x == 0))  # Calculates how many zeros are simulated with the nb model.
c(quantile(nb_zeros, probs = 0.025), quantile(nb_zeros, probs = 0.975))  # CI 2.5% and 97.5% based on the simulation.

sum(Owls$SiblingNegotiation == 0)  # The real number of zeros in your data.

  # As you can see, the model simulates between approx. 85 and 130 zeros, but your data actually contains 156 zeros. This
  # indicates that the model doesn't simulate enough zeros. To have a model adjusted to your data, you need to consider this
  # excess of zeros in your model.

##Model considering the excess of zeros
owls_zinb <- glmmTMB(SiblingNegotiation ~ FoodTreatment * SexParent + ArrivalTime +
                       (1|Nest) + offset(logBroodSize),
                     family = nbinom2, ziformula = ~1, data = Owls)  # Adding "ziformula = ~1" creates a model in two parts.
                                                                     # The first part has a p0 probability, where we have
                                                                     # a structural zero (y = 0). With the remaining 1-p0 probability,
                                                                     # y follows a Poisson or negative binomial distribution which can,
                                                                     # of course, also produce occasionally some zeros.

summary(owls_zinb)

AIC(owls_nb)
AIC(owls_zinb)  # As you can see, the zero-inflated negative binomial model offers a greater adjustment based on the AIC.

###Comparing the nb and the zinb models on the same graph
sim_owls_nb <- simulate(owls_nb, nsim = 1000, re.form = NULL, newdata = Owls)  # Data simulation based on the nb model
sim_owls_zi <- simulate(owls_zinb, nsim = 1000, re.form = NULL, newdata = Owls)  # Data simulation based on the zinb model
owls_pred <- mutate(Owls, pred = predict(owls_nb, type = "response"),
                    q025 = apply(sim_owls_nb, 1, quantile, probs = 0.025),
                    q975 = apply(sim_owls_nb, 1, quantile, probs = 0.975),
                    pred_zi = predict(owls_zinb, type = "response"),
                    q025_zi = apply(sim_owls_zi, 1, quantile, probs = 0.025),
                    q975_zi = apply(sim_owls_zi, 1, quantile, probs = 0.975)) %>%
  arrange(pred)  # # Calculates the confidence intervals for the two distributions

ggplot(owls_pred, aes(x = 1:nrow(owls_pred), y = pred, ymin = q025, ymax = q975)) +
  geom_ribbon(alpha = 0.5, fill = "#1b9e77") +
  geom_ribbon(aes(ymin = q025_zi, ymax = q975_zi), alpha = 0.3, fill = "#d95f02") +
  geom_line(color = "#1b9e77", size = 1) +
  geom_line(aes(y = pred_zi), color = "#d95f02", size = 1) +
  geom_point(aes(y = SiblingNegotiation), alpha = 0.5)  # Produces a graph with the nb CI and mean in green, and the zinb CI
                                                        # (brownish when over the nb CI) and mean in orange. The zinb model  
                                                        # seems more adjusted to the real data (black dots), with a larger CI
                                                        # on the right of the graph, and a narrower CI on the left.

