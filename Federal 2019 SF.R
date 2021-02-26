# Federal Government 2019 Salary SF

## Loading the Libraries
library(tidyverse)
library(boot)
library(outliers) #detect outliers
library(qcc) # change detection (CUSUM)
library(stringi)
library(tm)
library(Hmisc)
library(tidyr)
library(stringr)
library(rvest)
library(caret)
library(caTools)
library(MASS)
library(lme4)
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(wordcloud2) #creative visualizations
library(randomForest) #random forest
library(rpart) #decision trees
library(rpart.plot) #decision tree plot
library(gridExtra)  # plot arrangement


## Setting the Working Directory
setwd('./Government Public Salary')

## Reading the CSV File
federal_2019 <- read_csv('./sf2019.csv')
head(federal_2019)
str(federal_2019)
glimpse(federal_2019)


## Dropping the Unknown Names

federal_2019 <- federal_2019[federal_2019$Name!="NAME WITHHELD BY AGENCY" & 
                               federal_2019$Name!="NAME WITHHELD BY OPM", ]
### federal_2019 <- federal_2019[federal_2019$Location != "REDACTED"] #That is 1,014,764 rows
## Seperating the name columns to first and last names
federal_2019 <- federal_2019 %>%
  separate(Name, c("Last Name", "First Name"))

## Cleaning up the currency format

federal_2019 <- federal_2019 %>%
  mutate_if(~any(str_detect(., '^\\$'), na.rm = TRUE),
            ~as.numeric(str_replace_all(., '[$,]', '')))

## Combining the two columns into one as pay grade

federal_2019 <- unite(federal_2019, "Pay Grade", c("Pay Plan", "Grade"), remove=FALSE)

## View the dataset again

head(federal_2019)
str(federal_2019)
glimpse(federal_2019)

## Dropping variables to eliminate redundancy, unneeded columns, and privacy concerns
federal_2019 <- subset(federal_2019, select = -c(Bonus, FY, `Pay Plan`, Location, `Last Name`))


## View the dataset again

head(federal_2019)
str(federal_2019)
glimpse(federal_2019)
view(federal_2019)
summary(federal_2019)

## Descriptive statistics of federal employees
as.numeric(mean(federal_2019$Salary, na.rm=TRUE))
as.numeric(median(federal_2019$Salary, na.rm = TRUE))
as.numeric(var(federal_2019$Salary, na.rm = TRUE))
as.numeric(sd(federal_2019$Salary))
as.numeric(min(federal_2019$Salary))
as.numeric(max(federal_2019$Salary))


## Looking for employees who make over $200k

sf_over200 <- federal_2019 %>%
  arrange(desc(Salary))%>%
  filter(Salary > 200000)

view(sf_over200)
count(sf_over200)

## Employees over $250k

sf_over250 <- federal_2019 %>%
  arrange(desc(Salary))%>%
  filter(Salary > 250000)

view(sf_over250)
count(sf_over250)


## Sample Mean
sal.mean = with(federal_2019, mean(Salary))
sal.mean

## Bootstrap

B = 10000
n = nrow(federal_2019)
boot.samples = matrix(sample(federal_2019$Salary, size = B * n, replace = TRUE),
                      B, n)
boot.statistics = apply(boot.samples, 1, mean)

## Density Plot for Federal Gov't Salaries

ggplot(data.frame(meanTime = boot.statistics),aes(x=meanTime)) +
  geom_histogram(binwidth=0.25,aes(y=..density..)) +
  geom_density(color="red")


## Standard Deviation of Error
sal.se = sd(boot.statistics)
sal.se

## Confidence Interval
me = ceiling(10 * 2 * sal.se)/10
round(sal.mean, 1) + c(-1, 1) * me

## Function for Bootstrap

boot.mean = function(x,B,binwidth=NULL) {
  n = length(x)
  boot.samples = matrix( sample(x,size=n*B,replace=TRUE), B, n)
  boot.statistics = apply(boot.samples,1,mean)
  se = sd(boot.statistics)
  if ( is.null(binwidth) )
    binwidth = diff(range(boot.statistics))/30
  p = ggplot(data.frame(x=boot.statistics),aes(x=x)) +
    geom_histogram(aes(y=..density..),binwidth=binwidth) + geom_density(color="red")
  plot(p)
  interval = mean(x) + c(-1,1)*2*se
  print( interval )
  return( list(boot.statistics = boot.statistics, interval=interval, se=se, plot=p) )
}


## Salary Distribution Round 2

out = with(federal_2019, boot.mean(Salary, B = 10000))
out$interval

## For Loop Version

n = length(federal_2019$Salary)
B = 1000
result = rep(NA, B)
for (i in 1:B) {
  boot.sample = sample(n, replace = TRUE)
  result[i] = mean(federal_2019$Salary[boot.sample])
}
with(federal_2019, mean(Salary) + c(-1, 1) * 2 * sd(result))

## Difference in Means

with(federal_2019, summary(Salary))

## Using the Boot Package Version
my.mean = function(x, indices) {
  return( mean( x[indices] ) )
}
salry.boot = boot(federal_2019$Salary, my.mean, 10000)
salry.boot

## Confidence Interval
boot.ci(salry.boot)

## Z-Score
fed19_zscore <- federal_2019$Salary - mean(federal_2019$Salary)/sd(federal_2019$Salary)
fed19_zscore

## One Sample T-Test Federal Government
set.seed(254)
federal19sf_t.test <- t.test(federal_2019$Salary)
federal19sf_t.test

## Economist Salary Overall

economist_all <- federal_2019 %>%
  filter(Occupation == "ECONOMIST")

## Salary Summary Statistics
summary(economist_all$Salary)

## Boxplot
boxplot(economist_all$Salary)

## Histogram
hist(economist_all$Salary)

## Color Template

my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")

## Most common job titles in the Federal Government in SF

federal_2019 %>%
  count(Occupation, sort = TRUE) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(Occupation = reorder(Occupation, n)) %>%
  ggplot() +
  geom_col(aes(Occupation, n), fill = my_colors[4]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Title Count") +
  ggtitle("Most Common Roles for Federal Government in SF") +
  coord_flip()

## Generating the Wordcloud
fg_wordcounts <- federal_2019 %>%
  count(Occupation, sort = TRUE) 

wordcloud2(fg_wordcounts[1:100, ], size = .75)


## Most Common Agency in the Federal Government in SF

federal_2019 %>%
  count(Agency, sort = TRUE) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(Agency = reorder(Agency, n)) %>%
  ggplot() +
  geom_col(aes(Agency, n), fill = my_colors[1]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Title Count") +
  ggtitle("Most Common Agencies for Federal Government in SF") +
  coord_flip()

## Generating the Wordcloud
agency_wordcounts <- federal_2019 %>%
  count(Agency, sort = TRUE) 

wordcloud2(agency_wordcounts[1:18, ], size = 1.5)


## Partition the dataset to the train and test
set.seed(951)
trainingRowIndex <- sample(1:nrow(federal_2019), 0.8*nrow(federal_2019))  # selecting 80% for training dataset
trainData <- federal_2019[trainingRowIndex, ]  # model training data
testData  <- federal_2019[-trainingRowIndex, ]   # test data

lm_federal2019 <- lm(Salary ~ Grade, data=trainData)  # build the model
federal2019_pred <- predict(lm_federal2019, testData)  # predict distance

summary (lm_federal2019)  # model summary
AIC (lm_federal2019)  # Calculate akaike information criterion

actuals_preds <- data.frame(cbind(actuals=testData$Salary, predicteds=federal2019_pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 74.39% using salary variable
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  

min_max_accuracy #min max accuracy
mape 

## Random forest
prop.table(table(trainData$Salary)) #salary event for train data
prop.table(table(testData$Salary)) #salary event for test data

## Creating the random forest model
federal2019_rf <- randomForest(Salary ~ Grade, data=trainData, 
                         ntree=1000, proximity=TRUE, importance=TRUE) 
federal2019_rf

## Plotting the random forest model
plot(federal2019_rf, main="Random Forest Regression Model", color="darkgreen", size=3)

## Gathering the summary results
summary(federal_2019rf) # detailed summary of splits

## Creating the regression random forest trees

federal2019_rftree <- rpart(Salary ~ Grade, method = "anova", data=trainData)
printcp(federal2019_rftree) # display the results
plotcp(federal2019_rftree) # visualize cross-validation results
summary(federal2019_rftree) # detailed summary of splits


## Decision Trees

#table(federal_2019$Salary) #to obtain binary values for death event
#table(federal_2019$Grade) #to obtain binary values for smoking

## Fitting decision tree by classification
#federal2019_decision_fit <- rpart(Salary ~ Grade,
#                                 method="class", data=federal_2019)

#printcp(federal2019_decision_fit) # display the results
#plotcp(federal2019_decision_fit) # visualize cross-validation results
#summary(federal2019_decision_fit) # detailed summary of splits

## Plotting the classification tree
#plot(federal2019_decision_fit, uniform=TRUE,
#     main="Heart Failure Classification Tree")
#text(federal2019_decision_fit, use.n=TRUE, all=TRUE, cex=.8)

## Pruning the tree
#federal_prune<- prune(federal2019_decision_fit, 
#                    cp=federal2019_decision_fit$cptable[which.min(federal2019_decision_fit$cptable[,"xerror"]),"CP"])

## Plot the classification pruned tree
#plot(federal_prune, uniform=TRUE,
#     main="Heart Failure Pruned Decision Tree")
#text(federal_prune, use.n=TRUE, all=TRUE, cex=.8)



## Only Looking at IRS Sector

irs_2019 <- federal_2019 %>%
  filter(Agency == "INTERNAL REVENUE SERVICE")

## Checking the variables again and viewing the data only for IRS Employees

head(irs_2019)
str(irs_2019)
glimpse(irs_2019)
view(irs_2019)

## Filtering to Certain positions to do distributions

economist_pos <- irs_2019 %>%
  filter(Occupation == "ECONOMIST")
economist_pos

senior_pos <- irs_2019 %>%
  filter(Occupation == "SENIOR ECONOMIST")
senior_pos

## Histogram of all distributions on pay grade
ggplot(irs_2019, aes(`Pay Grade`)) + 
  geom_histogram(color = "red",bg="blue" ,stat='count') + 
  ggtitle("Distributions of Pay Grades") 

## Histogram of all distributions on salary
ggplot(irs_2019, aes(Salary)) + 
  geom_histogram(color = "darkblue",bg="orange", stat='count') + 
  ggtitle("Distributions of Salary") 

## Scatterplot of all incomes based on income
ggplot(irs_2019, aes(`Pay Grade`, Salary)) + 
  geom_point(color="darkred") + ggtitle("Pay Grade Distribution")

## Linear Regression

irs_2019.mod <- lm(Salary ~ `Pay Grade`, # regression formula
                   data=irs_2019) # data set
## Summarize and print the results
summary(irs_2019.mod) # show regression coefficients table

## Assoication with the two variables

summary(lm(Salary ~ `Pay Grade`, data = irs_2019))

## Logistic Regression Model

irs_lr1 <-lmer(Salary ~ 1 + (1|`Pay Grade`),
               data=irs_2019, REML = FALSE)
summary(irs_lr1)

## One Sample T-test

set.seed(0)
irs19sf_t.test <- t.test(irs_2019$Salary)
irs19sf_t.test

## Two Sample T-test

set.seed(1598)
irs19_tsamp <- t.test(irs_2019$Salary, irs_2019$Grade, paired = TRUE)
irs19_tsamp

## Looking at the most common roles in the IRS

irs_2019 %>%
  count(Occupation, sort = TRUE) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(Occupation = reorder(Occupation, n)) %>%
  ggplot() +
  geom_col(aes(Occupation, n), fill = my_colors[3]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Title Count") +
  ggtitle("Most Common Roles for IRS") +
  coord_flip()

## Generating the Wordcloud
irs_wordcounts <- irs_2019 %>%
  count(Occupation, sort = TRUE) 

wordcloud2(irs_wordcounts[1:30, ], size = .75)

