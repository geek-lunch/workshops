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
                        labels = c("1-10", "11-30", ">30")))  # Add a column to your dataframe where the height class
                                                              # is specified (1-10 cm, 16-30 cm, or more than 30 cm).
data_summary <- data %>%
  group_by(Site, Plot, Height_class) %>%
  summarize(n_ligne = n())  # Summurizes your dataframe by site, plot and height class.

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

names(reorganized_data)[names(reorganized_data) == "n_ligne"] <- "freq"  # Changes the name of the column
                                                                         # n_ligne to freq.


reorganized_data$freq[is.na(reorganized_data$freq)] = 0  # CHanges all NAs value in the freq column to 0.

