## ANALYSES AND PLOTS SCRIPT ##

# Loading required packages

library('data.table')
library('dplyr')
library('lme4')
library('lmerTest')
library('tidyverse')
library('readr')
library('tidyr')
library('FSA')
library('ggplot2')
library('plotly')
library('scales')


# Remember to set your working directory!

## STARTING WITH ANALYSES ON THE CATEGORISED SAMPLE ##

# Loading in the data

categorisedDF <- fread("FINAL_Categorised.csv")

# Removing Z Scores that are NA

categorised <- categorisedDF %>% drop_na(FisherZScores)

# Establishing number of rows in dataframe

final_row <- nrow(categorised)

# Creating an empty vector for final categorisation decision

CatDecisionFinal <- vector(mode = "numeric", length = final_row)

# Converting the focal/non-focal Decision

for(i in 1:final_row) {
  
  CatCategoryDecision <- categorised$decision[i]
  
  if (CatCategoryDecision == "1") {
    CatDecisionFinal[i] <- 0
  } 
  
  else if (CatCategoryDecision == "2") {
    CatDecisionFinal[i] <- 0
  } 
  else {
    CatDecisionFinal[i] <- 1
  }
}

categorised$DecisionBinary <- CatDecisionFinal

# Rescaling my years to be from 0 to 20

library(metacoder)
realYearStart0 <- rescale(categorised$realYear, to = c(0, 20), hard_bounds = TRUE)

# Adding this back in to the dataframe

categorised$realYearStart0 <- realYearStart0

# Running a Random Effects ANOVA

CatNullModel <- lmer("FisherZScores ~ 1 + (1 | journalID)", data = categorised)

summary(CatNullModel)

# Calculating an ICC

CatNullICC <- performance::icc(CatNullModel)

# Running my real equation

CatModel <- lmer("FisherZScores ~ 1 + realYearStart0 + statistic + realYearStart0 * DecisionBinary + (1 | journalID)", data = categorised)

summary(CatModel)

confint(FixedCatModel, "beta_", level = 0.95,
	method = c("profile", "Wald", "boot"),
	nsim = 1000,
        boot.type = c("perc","basic","norm"),
        FUN = NULL, quiet = FALSE,
	oldNames = TRUE)

# Calculating an ICC

CatModelICC <- performance::icc(CatModel)

print(CatModelICC)


## NOW RUNNING ANALYSES ON THE COMPLETE DATABASE ##

# Reading in the file

completeDF <- fread("FINAL_Complete.csv")

# Removing Z Scores that are NA

complete <- completeDF %>% drop_na(FisherZScores)

# Rescaling my years to be from 0 to 20

library(metacoder)
realYearStart0 <- rescale(complete$realYear, to = c(0, 38), hard_bounds = TRUE)

# Adding this back in to the dataframe

complete$realYearStart0 <- realYearStart0

# Running a Random Effects ANOVA

CompNullModel <- lmer("FisherZScores ~ 1 + (1 | journalID)", data = complete)

summary(CompNullModel)

# Calculating an ICC

CompNullICC <- performance::icc(CatNullModel)

print(CompNullICC)

# Running my real equation

CompModel <- lmer("FisherZScores ~ 1 + realYearStart0 + statistic + (1 | journalID)", data = complete)

summary(CompModel)

confint(CompModel, "beta_", level = 0.95,
        method = c("boot"),
        nsim = 500,
        boot.type = c("perc","basic","norm"),
        FUN = NULL, quiet = FALSE,
        oldNames = TRUE)

# Calculating an ICC

CompModelICC <- performance::icc(CompModel)

print(CompModelICC)

## NOW GENERATING DESCRIPTIVE STATISTICS ##

# Descriptives on years in categorised sample

summary(categorised$realYear)

# And complete database

summary(complete$realYear)

# Summarising the decisions where 0 is focal and 1 is non-focal

table1 <- table(categorised$DecisionBinary)

table1

prop.table(table1)

# Crosstabulating year and decision

table2 <- table(categorised$realYear, categorised$DecisionBinary)
crosstabs <-as.data.frame(table2)
table2

# Checking proportions of statistics in categorised sample

stats_table <-table(categorised$statistic)

stats_table

prop.table(stats_table)

# And for the complete

stats_table1 <-table(complete$statistic)

stats_table1

prop.table(stats_table1)

# Cross-tabulating year and statistic 

table3 <- table(categorised$realYear, categorised$statistic)
crosstabs2 <-as.data.frame(table3)
table3

table4_comp <- table(complete$realYear, complete$statistic)
crosstabs_comp <-as.data.frame(table4_comp)
table4_comp
```

## AND RUNNING ASSUMPTIONS CHECKS ##

# The assumption of linearity in the categorised sample

plot(resid(CatModel),categorised$realYear, xlab = "Categorised Sample Model Residuals", ylab = "Years of Publication")

# And in the complete database

plot(resid(CompModel),complete$realYear, xlab = "Complete Database Model Residuals", ylab = "Years of Publication")

# The assumption of homogeneity of residual variance in the categorised sample

categorised$ModelResids<- residuals(CatModel) # extracts the residuals and places them in a new column in our original data table
categorised$AbsModelResids <-abs(categorised$ModelResids) # creates a new column with the absolute value of the residuals
categorised$ModelResidsSq <- categorised$AbsModelResids^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.Model.F <- lm(ModelResidsSq ~ journalID, data=categorised) #ANOVA of the squared residuals
anova(Levene.Model.F) #displays the results

# And in the complete database

complete$ModelResids<- residuals(CompModel)
complete$AbsModelResids <-abs(complete$ModelResids) 
complete$ModelResidsSq <- complete$AbsModelResids^2 
Levene.Model.F.Comp <- lm(ModelResidsSq ~ journalID, data=complete) 
anova(Levene.Model.F.Comp) 

# And visually inspecting this assumption

plot(CatModel)
plot(CompModel)

# The assumption of linearity on the categorised sample

require("lattice")

qqmath(CatModel) # Outlier identification is turned off bc there are so many groups

# And in the complete database

qqmath(CompModel) #id: identifies values that may be exerting undue influence on the model (i.e. outliers)

## GENERATING PLOTS ##

# Plotting decision by year

names(crosstabs) <- c("Year", "Category", "Frequency")

crosstabsplot1 <- ggplot(crosstabs, aes(x=Year, y=Frequency, fill=Category)) +
  geom_col(position="dodge")+ xlab("Publication Year") + ylab("Category Frequency") + theme_classic()

crosstabsplot1 + scale_fill_discrete(name = "Category", labels = c("Focal", "Non-Focal")) 

# Plotting statistic type by year (categorised)

names(crosstabs2) <- c("Year", "Statistic", "Frequency")

crosstabsplot2 <- ggplot(crosstabs2, aes(x=Year, y=Frequency, fill=Statistic)) +
  geom_col(position="dodge")+ xlab("Publication Year") + ylab("Statistic Frequency") + theme_classic()

crosstabsplot2 + scale_fill_discrete(name = "Category", labels = c("F-statistics", "r-statistics", "t-statistics"))

crosstabsplot2 + scale_x_discrete(breaks = seq(1998, 2018, by = 5))

# Plotting statitsic type by year (complete database)

names(crosstabs_comp) <- c("Year", "Statistic", "Frequency")

crosstabsplot3 <- ggplot(crosstabs_comp, aes(x=Year, y=Frequency, fill=Statistic)) +
  geom_col(position="dodge")+ xlab("Publication Year") + ylab("Statistic Frequency") + theme_classic() 

crosstablesplot4 <- crosstabsplot3 + scale_fill_discrete(name = "Category", labels = c("F-statistics", "r-statistics", "t-statistics")) 

crosstablesplot4 + scale_x_discrete(breaks = seq(1985, 2018, by = 5))

# Plot of my model (categorised)

aggregatedCatData <-as.data.frame(aggregate(FisherZScores ~ realYear+journalID, categorised, mean))
intercept_parameter <- fixef(CatModel)['(Intercept)']
year_parameter <- fixef(CatModel)['realYearStart0']

ggplot(aggregatedCatData, aes(x = realYear, y = FisherZScores, colour = as.factor(journalID))) + xlab("Publication Year") + ylab("Fisher Z Transformed Correlation Coefficients") + theme_classic()+ theme(legend.position="none")+geom_jitter()+geom_abline(intercept = intercept_parameter + min(aggregatedCatData$realYear)*-year_parameter, slope = year_parameter)

# And complete

aggregatedCompData <-as.data.frame(aggregate(FisherZScores ~ realYear+journalID, complete, mean))
intercept_parameter <- fixef(CompModel)['(Intercept)']
year_parameter <- fixef(CompModel)['realYearStart0']

ggplot(aggregatedCompData, aes(x = realYear, y = FisherZScores, colour = as.factor(journalID))) + xlab("Publication Year") + ylab("Fisher Z Transformed Correlation Coefficients") + theme_classic()+theme(legend.position="none")+geom_jitter()+geom_abline(intercept = intercept_parameter + min(aggregatedCompData$realYear)*-year_parameter, slope = year_parameter)
