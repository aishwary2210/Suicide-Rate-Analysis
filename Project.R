library(tidyverse) 
library(knitr) 
library(kableExtra) 
library(treemap) 
library(ggthemes) 
library(highcharter)
library(summarytools)
library(corrplot)
library(formattable)
library(ggcorrplot)


# Loading the packages
options(warn = -1)
packags <- c("tidyverse", "knitr", "kableExtra","ggthemes", "treemap", "highcharter", "summarytools", "ggcorrplot", "knitr","formattable")
purrr::walk(packags, library, character.only = T, quietly = T)



# Importing dataset

data <-read.csv("/Users/ash/Desktop/RBS/SEM 2/Multivariate Analysis/Suicide Rate Analysis.csv") 

# Data summary
# We are using str() & head() function to inspect and have a brief overwiew of the dataset.
str(data)
summary(data)
head(data)

# No of Columns in the dataset. 
length(data[,-1])

# Cleaning Data
# droppig NA values from suicide nos collumn.
clean_data <- data %>% 
filter(suicides_no != "NA" & suicides_no!=0) 

# Checking for the missing values in each collumns. 
colSums(is.na(clean_data))

# Cleaning HDI collumn. 
clean_data$HDI.for.year <- NULL

# Changing collumn name. 
colnames(data)[colnames(data) == "ï..country"] <- "country"

# Data exploration

#Nearly 70% of the data is missing. 
sum(is.na(data$HDI.for.year))/length(data$HDI.for.year) * 100

# Qualitative Variable frequencies 

# No of occurences of each generation in the dataset.
data %>% group_by(generation) %>% 
summarize(nb = n()) %>% kable () %>%
kable_styling(bootstrap_options = "striped", full_width = F)

# X generation and silent are the most popular.
# Generation Z is the smallest group.
hcbar(x = data$generation, name = "Génération") %>% 
hc_add_theme(hc_theme_economist())

# By Age Groups 
# Age groups are equally distributed.
hcbar(x = data$age, name = " ge") %>% 
hc_add_theme(hc_theme_economist())

# By Sex
# Both are equally distributed
hcbar(x = data$sex, name = "Sexe") %>% 
hc_add_theme(hc_theme_economist())

# By year
hcbar(x = as.character(data$year), name = "Years") %>% 
hc_add_theme(hc_theme_economist())

# Suicide rates by Sex and Age group
# For all age groups suicide rate is higher for men than women.
# This means 'sex' variable differentiates the population of dataset. 

data %>% group_by(year,sex, age) %>% 
summarize(moy_suicide = mean(suicides.100k.pop)) %>% 
ggplot(aes(x= year, y= moy_suicide)) + 
geom_line(aes(color = sex), size=1.1) + facet_wrap(~age, scale = "free_y") + 
ylab("Suicides (mean)") + ggtitle("Evolution of suicide rate per sex and age categories")

#Visualization
sort(data$age)
plot(data[,1], data[,5],main = "suicide/country", xlab="", ylab="suicide-no")
plot(data[,12], data[,5],main = "suicide/generation", xlab="", ylab="suicide-no")


# Tests

# 1. Analysis Of Variance

temp <- data %>% group_by(year,sex, age) %>% 
summarize(moy_suicide = mean(suicides.100k.pop))
fit <- aov(formula = moy_suicide~age+sex, data = temp)
summary(fit)
# There's a Statistical significance between the two groups men and women. 
# This difference is stronger than the age group.
# (Look at the F value, by default R doesn’t print numbers < to 2e-16).


# 2. Co-relation between pairs of quantitative variables. 

options(repr.plot.height = 4, repr.plot.res = 180, repr.plot.width = 6)
data[,sapply(data, is.numeric)] %>% 
cor(use = "complete.obs") %>% ggcorrplot(hc.order = TRUE, type = "lower", lab = TRUE)
# Human development index correlates positively with the per capita GDP (0.77), 
# which means that these two variables tend to go in the same direction.

# The number of suicide is positively correlated with the population (country).
# The reason for this correlation is that it is a time series: the number of suicide increases with the growth of the population which itself increases with time, 
# the same goes for the GDP, which increases every year.


# 3. T-test
data_1<-transform(data, age = as.numeric(age))
t.test(data_1$age,data_1$sucides_no, var.equal = TRUE, paired=FALSE)
t.test(data_1$gdp_per_capita,data_1$sucides_no, var.equal = TRUE, paired=FALSE)



