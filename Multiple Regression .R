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

data <-read.csv("/Users/ash/Desktop/RBS/SEM 2/Multivariate Analysis/Suicide Rate Analysis.csv") 



names(data)[1]= "country"
data

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
as.numeric(data$age)
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
data_1

-----
  # PCA
  head(data)

cor(data[,c(5,6,11)])

suicide_pca <- prcomp(data[,c(6,5,11)],scale=TRUE)
suicide_pca
summary(suicide_pca)

(eigen_data<- suicide_pca$sdev^2)
names(eigen_data) <- paste("PC",1:3,sep="")
eigen_data

sumlambdas <- sum(eigen_data)
sumlambdas

propvar <- eigen_data/sumlambdas
propvar

cumvar_suicide <- cumsum(propvar)
cumvar_suicide

matlambdas <- rbind(eigen_data,propvar,cumvar_suicide)
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
round(matlambdas,4)

summary(suicide_pca)
suicide_pca$rotation
print(suicide_pca)

# Sample scores stored in suicide_pca$x
suicide_pca$x

# Identifying the scores by their survival status
suicidetyp_pca <- cbind(data.frame(data$population),suicide_pca$x)
suicidetyp_pca


# Means of scores for all the PC's 
tabmeansPC <- aggregate(suicidetyp_pca[,2:4],by=list(population=data$population),mean)
tabmeansPC
tabmeansPC <- tabmeansPC[rev(order(tabmeansPC$population)),]
tabmeansPC
tabfmeans <- t(tabmeansPC[,-1])
tabfmeans
colnames(tabfmeans) <- t(as.vector(tabmeansPC[1]))
tabfmeans

# Standard deviations of scores for all the PC's 
tabsdsPC <- aggregate(suicidetyp_pca[,2:4],by=list(population=data$population),sd)
tabfsds <- t(tabsdsPC[,-1])
colnames(tabfsds) <- t(as.vector(tabsdsPC[1]))
tabfsds

# T Tests
t.test(PC1~data$sex,data=suicidetyp_pca)
t.test(PC2~data$sex,data=suicidetyp_pca)

# F ratio tests
var.test(PC1~data$sex,data=suicidetyp_pca)
var.test(PC2~data$sex,data=suicidetyp_pca)


# In 1986

# PCA (Principle Component Analysis) Biplot
library(FactoMineR)
pca_data <- data %>% group_by(country, year) %>% summarize(nb_occurence = n(), sum_gdp = sum(gdp_per_capita....), 
                                                           population = sum(population), suicide = sum(suicides.100k.pop),
                                                           gdp_real = sum_gdp/nb_occurence, gdp_per_capita = gdp_real/population) 
pca_1986 <- pca_data %>% filter(year == 1986) %>% select(country, population, suicide, gdp_per_capita) %>% 
  data.frame(row.names = "country")

res_1986 <- PCA(pca_1986, scale.unit = T)

library(factoextra)

fviz_pca_biplot(res_1986, labelsize = 4, repel = T,title = "PCA - Biplot 1986") + theme_economist()


# The first dimension explains 53% of the variability among the countries. It is positively influenced by the 3 variables, 
# but GDP per capita has a stronger influence on this first dimension.   
# The second dimension explains 29% of information and is strongly influenced positively by the population and negatively by suicide.
# These two axes allow us to say that a country whose coordinates are located to the right of this graph is a rich country. Similarly a country located at the top is a populated country. 
# If the country is located at the bottom of the graph the higher is the proportion of suicide in that country.  

# In 2015  

#Let's see how the situation of countries has changed in 29 years.

pca_2015 <- pca_data %>% 
  filter(year == 2015) %>% select(country, population, suicide) %>% 
  data.frame(row.names = "country")

res_2015 <- PCA(pca_2015, scale.unit = T)

fviz_pca_biplot(res_2015, labelsize = 4, repel = T,title = "PCA - Biplot 2015") + theme_economist()

# Compared to the results obtained in 1986, the two dimensions have about the same importance. 
#Suicide is not as important as it was in 1986. The direcion of arrows has changed: the more a country is on the right, the higher its suicide rate.
#The countries on the top right are countries with a large population and a high suicide rate. 
#While the countries at the bottom have a small population but a very high GDP per capita.  





# Multiple Regression Analysis. 

#Correlation between pairs of variables
data[,sapply(data, is.numeric)] %>%  cor(use = "complete.obs")
fit1 <- lm(suicides_no~sex+population+suicides.100k.pop+country+HDI.for.year+generation, data=data)
summary(fit1)

# country has most p-value among variables so we can remove it
fit2 <- lm(suicides_no~sex+population+suicides.100k.pop+HDI.for.year+generation, data=data)
anova(fit1, fit2)

#The results of anova shows that p-value>0.05 so we can accept null hypothesis(country is not important) 
coefficients(fit2)

library(GGally)
confint(fit2,level=0.95)
anova(fit2)
vcov(fit2)
cov2cor(vcov(fit2))
temp <- influence.measures(fit2)
temp
View(temp)

#diagnostic plots
plot(fit2)

# Assessing Outliers
outlierTest(fit2)
qqPlot(fit2, main="QQ Plot")
leveragePlots(fit2) # leverage plots
plot(fit2)

# Influence Plot
library(mvinfluence)

influencePlot(fit2, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit2, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(fit2)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)


# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(fit2)
summary(gvmodel)
fit2
summary(fit2)
fit3 <- fit2
fit4 <- lm(suicides_no~sex+population+suicides.100k.pop+HDI.for.year+generation, data=data)
fit4

# compare models
anova(fit3, fit4)
step <- stepAIC(fit2, direction="both")
step$anova # display results

library(leaps)
leaps<-regsubsets(suicides_no~sex+population+suicides.100k.pop+HDI.for.year+generation, data=data, nbest=10)
# view results
summary(leaps)

# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps)
plot(leaps,scale="r2")
#subsets(leaps, statistic="rsq")

# All Subsets Regression
plot(leaps,scale="bic")
summary(leaps)
?regsubsets
summary(leaps)
View(leaps)
leaps
coef(leaps,1:5)

# Calculate Relative Importance for Each Predictor
library(relaimpo)
calc.relimp(fit2 ,type=c("lmg","last","first","pratt"),rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(fit2, b = 1000, type = c("lmg", "last", "first", "pratt"), rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result
#https://rpubs.com/davoodastaraky/mtRegression
summary(fit2)

#Ok we got our standardize value. As we saw above the most valuable predictor of this model is gdp_for_year. We can interpreted that the more increase in GDP per year the more suicide rate would increase. Assuming this dataset or sample is a great representation of population.
#The second one is suicides_per_100k_pop. 
#And the third one it gdp_per_capital.
#It's telling us that the more GDP per capital is decreased the more suicide rate will increase. 



