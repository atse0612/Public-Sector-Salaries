# Federal Government 2018 Salary SF

## Loading the Libraries
library(tidyverse)
library(boot)
library(stringi)
library(tm)
library(Hmisc)
library(tidyr)
library(stringr)
library(rvest)
library(caret)
library(caTools)
library(lme4)
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(wordcloud2) #creative visualizations


## Setting the Working Directory
setwd('./Government Public Salary')

## Reading the CSV File
federal_2018 <- read_csv('./sf2018.csv')
head(federal_2018)
str(federal_2018)
glimpse(federal_2018)


## Dropping the Unknown Names

federal_2018 <- federal_2018[federal_2018$Name!="NAME WITHHELD BY AGENCY" & 
               federal_2018$Name!="NAME WITHHELD BY OPM", ]
### federal_2018 <- federal_2018[federal_2018$Location != "REDACTED"] #That is 1,014,764 rows
## Seperating the name columns to first and last names
federal_2018 <- federal_2018 %>%
  separate(Name, c("Last Name", "First Name"))

## Cleaning up the currency format

federal_2018 <- federal_2018 %>%
  mutate_if(~any(str_detect(., '^\\$'), na.rm = TRUE),
            ~as.numeric(str_replace_all(., '[$,]', '')))

## Combining the two columns into one as pay grade

federal_2018 <- unite(federal_2018, "Pay Grade", c("Pay Plan", "Grade"), remove=FALSE)

## View the dataset again

head(federal_2018)
str(federal_2018)
glimpse(federal_2018)

## Dropping variables to eliminate redundancy, unneeded columns, and privacy concerns
federal_2018 <- subset(federal_2018, select = -c(Bonus, FY, `Pay Plan`, Location, `Last Name`))


## View the dataset again

head(federal_2018)
str(federal_2018)
glimpse(federal_2018)
view(federal_2018)
summary(federal_2018)

## Descriptive statistics of federal employees
as.numeric(mean(federal_2018$Salary, na.rm=TRUE))
as.numeric(median(federal_2018$Salary, na.rm = TRUE))
as.numeric(var(federal_2018$Salary, na.rm = TRUE))
as.numeric(sd(federal_2018$Salary))
as.numeric(min(federal_2018$Salary))
as.numeric(max(federal_2018$Salary))


## Looking for employees who make over $200k

sf_over200 <- federal_2018 %>%
  arrange(desc(Salary))%>%
  filter(Salary > 200000)

view(sf_over200)
count(sf_over200)


## Sample Mean
sal.mean = with(federal_2018, mean(Salary))
sal.mean

## Bootstrap

B = 1000
n = nrow(federal_2018)
boot.samples = matrix(sample(federal_2018$Salary, size = B * n, replace = TRUE),
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

out = with(federal_2018, boot.mean(Salary, B = 1000))
out$interval

## For Loop Version

n = length(federal_2018$Salary)
B = 1000
result = rep(NA, B)
for (i in 1:B) {
  boot.sample = sample(n, replace = TRUE)
  result[i] = mean(federal_2018$Salary[boot.sample])
}
with(federal_2018, mean(Salary) + c(-1, 1) * 2 * sd(result))

## Difference in Means

with(federal_2018, summary(Salary))

## Using the Boot Package Version
my.mean = function(x, indices) {
  return( mean( x[indices] ) )
}
salry.boot = boot(federal_2018$Salary, my.mean, 10000)
salry.boot

## Confidence Interval
boot.ci(salry.boot)

## Z-Score
fed18_zscore <- federal_2018$Salary - mean(federal_2018$Salary)/sd(federal_2018$Salary)
fed18_zscore

## One Sample T-Test Federal Government
set.seed(254)
federal18sf_t.test <- t.test(federal_2018$Salary)
federal18sf_t.test

## Color Template

my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")

## Most common job titles in the Federal Government in SF

federal_2018 %>%
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
fg_wordcounts <- federal_2018 %>%
  count(Occupation, sort = TRUE) 

wordcloud2(fg_wordcounts[1:100, ], size = .75)


## Most Common Agency in the Federal Government in SF

federal_2018 %>%
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
agency_wordcounts <- federal_2018 %>%
  count(Agency, sort = TRUE) 

wordcloud2(agency_wordcounts[1:18, ], size = 1.5)


## Only Looking at IRS Sector

irs_2018 <- federal_2018 %>%
  filter(Agency == "INTERNAL REVENUE SERVICE")

## Checking the variables again and viewing the data only for IRS Employees

head(irs_2018)
str(irs_2018)
glimpse(irs_2018)
view(irs_2018)

## Filtering to Certain positions to do distributions

economist_pos <- irs_2018 %>%
  filter(Occupation == "ECONOMIST")
economist_pos

senior_pos <- irs_2018 %>%
  filter(Occupation == "SENIOR ECONOMIST")
senior_pos

## Histogram of all distributions on pay grade
ggplot(irs_2018, aes(`Pay Grade`)) + 
  geom_histogram(color = "red",bg="blue" ,stat='count') + 
  ggtitle("Distributions of Pay Grades") 

## Histogram of all distributions on salary
ggplot(irs_2018, aes(Salary)) + 
  geom_histogram(color = "darkblue",bg="orange", stat='count') + 
  ggtitle("Distributions of Salary") 

## Scatterplot of all incomes based on income
ggplot(irs_2018, aes(`Pay Grade`, Salary)) + 
  geom_point(color="darkred") + ggtitle("Pay Grade Distribution")

## Linear Regression

irs_2018.mod <- lm(Salary ~ `Pay Grade`, # regression formula
                   data=irs_2018) # data set
## Summarize and print the results
summary(irs_2018.mod) # show regression coefficients table

## Assoication with the two variables

summary(lm(Salary ~ `Pay Grade`, data = irs_2018))

## Logistic Regression Model

irs_lr1 <-lmer(Salary ~ 1 + (1|`Pay Grade`),
               data=irs_2018, REML = FALSE)
summary(irs_lr1)

## One Sample T-test

set.seed(0)
irs18sf_t.test <- t.test(irs_2018$Salary)
irs18sf_t.test

## Two Sample T-test

set.seed(1598)
irs18_tsamp <- t.test(irs_2018$Salary, irs_2018$Grade, paired = TRUE)
irs18_tsamp

## Looking at the most common roles in the IRS

irs_2018 %>%
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
irs_wordcounts <- irs_2018 %>%
  count(Occupation, sort = TRUE) 

wordcloud2(irs_wordcounts[1:30, ], size = .75)

