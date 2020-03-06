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
----
# Clustering

  data<-transform(data, sex = as.numeric(sex))
data<-transform(data, age = as.numeric(age))
data<-transform(data, country = as.numeric(country))
data<-transform(data, country.year = as.numeric(country.year))
data<-transform(data, generation  = as.numeric(generation))
data<-na.omit(data)
data

str(data)
#single
hclustfunc <- function(x, method = "single", dmeth = "euclidean") {    
  hclust(dist(x, method = dmeth), method = method)}
dist.nn <- hclustfunc(data, method = "single")
plot(dist.nn, hang=-1,xlab="Object",ylab="Distance",
     main="Dendrogram. Nearest neighbor linkage")

#Default - Complete
hclustfunc1 <- function(x, method = "complete", dmeth = "euclidean") {    
  hclust(dist(x, method = dmeth), method = method)}
dist.fn <- hclustfunc1(data, method = "complete")
plot(dist.fn,hang=-1,xlab="Object",ylab="Distance",
     main="Dendrogram. Farthest neighbor linkage")


#Average
data<-transform(data, gdp_for_year....  = as.numeric(gdp_for_year....))
matstd.data <- scale(data)

# Creating a (Euclidean) distance matrix of the standardized data
dist.data <- dist(matstd.data, method="euclidean")


#Summary of Each Data Point
sum <- data %>% select(-country, -age, -sex, -generation) %>% summary()
sum


#scaling sr1 & euclidean distance of sr1 observations
dist_sr1 <- dist(scale(data))

#visualizing pair-wise distances
hist(dist_sr1)


#linkage analysis of clusters
hclust_data <- hclust(dist_sr1, method = "complete")

library(dendextend)

data_dendrogram <- as.dendrogram(hclust_data)

data_dendrogram_color <- color_branches(data_dendrogram, h = 8)

#extract cluster group
cluster <- cutree(hclust_data, k=4)

#add cluster group values to observations
sr2 <- data %>% mutate(cluster = cluster)

#Dendogram of Cluster
plot(data_dendrogram_color)


#clustering , Kmeans clustering
cluster1_country <- data %>%
  filter(cluster ==  2) %>%
  ggplot(aes(y = suicides_no, x = `gdp_for_year....`, color = `country`)) +
  geom_point()+
  scale_y_log10() +
  scale_x_log10() +
  ggtitle("Cluster 1 GDP ~ No. of Suicides by Country")

plot(cluster1_country)

#Victims in Cluster 1 are from Japan, Ukraine, and the United States.
#Ukraine victims have the lowest GDP for Year and Suicide Numbers of the three.
#They are all Male.
#Their ages range from 15-74, with the age group 35-54 having the greatest value in Number of Suicides.


#visualizing clusters and their regression line
#sr2_plot <- ggplot(sr2, aes(y=suicides_no, x=`gdp_for_year....`, color = factor(cluster))) +
# geom_point() +
#geom_smooth(method = "lm", se = FALSE)

#sr2_plot


cluster2_age <- sr2 %>% 
  filter(cluster ==  3) %>%
  ggplot(aes(y = suicides_no, x = `gdp_for_year....`, color = `age`)) +
  geom_point() +
  scale_y_log10() +
  ggtitle("Cluster 2 GDP ~ No. of Suicides by Age")

plot(cluster2_age)

#Victims in Cluster 2 are all from the United States.
#They are all Male.
#Their ages range from 35-74, with the age group 35-54 having the greatest number of observations.


cluster3_country <- sr2 %>%
  filter(cluster ==  4) %>%
  ggplot(aes(y = suicides_no, x = `gdp_for_year....`, color = `generation`)) +
  geom_point()+
  scale_y_log10() +
  scale_x_log10() +
  ggtitle("Cluster 3 GDP ~ No. of Suicides by Generation")


plot(cluster3_country)

#Victims in Cluster 3 are all from the United States.
#They are composed of Males and Females, however, the majority of victims in this cluster are Female.
#Their ages range from 15-75+, with the age group 24-34 having the greatest number of suicides between 2010 and 2014.
#Millenials have the greatest number of suicides between the year 2011 and 2014
#Number of Suicides for each age group have not changed much since year 2000.


(kmeans1.data <- kmeans(matstd.data,2,nstart = 10))
clus1 <- matrix(names(kmeans1.data$cluster [kmeans1.data$cluster == 1]), 
                ncol=1, nrow=length(kmeans1.data$cluster[kmeans1.data$cluster == 1]))
colnames(clus1) <- "Cluster 1"



(kmeans2.data <- kmeans(matstd.data,2,nstart = 10))
clus2 <- matrix(names(kmeans2.data$cluster [kmeans2.data$cluster == 2]), 
                ncol=1, nrow=length(kmeans2.data$cluster[kmeans2.data$cluster == 2]))
colnames(clus2) <- "Cluster 2"


(kmeans3.data <- kmeans(matstd.data,2,nstart = 10))
clus3 <- matrix(names(kmeans2.data$cluster [kmeans2.data$cluster == 3]), 
                ncol=1, nrow=length(kmeans3.data$cluster[kmeans1.data$cluster == 3]))
colnames(clus3) <- "Cluster 3"

list(clus1,clus2,clus3)














