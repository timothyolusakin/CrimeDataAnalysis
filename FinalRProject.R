###############Setting the working directory####################
setwd("D:/Projects/Assignment/Data Analysis and Visualisation")

################Loading the library###########################
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidygeocoder)
library(janitor)
library(corrplot)
library(RColorBrewer)
library(psych)
library(e1071)
library(skimr)
library(factoextra)
library(class)
install.packages('caret') 
install.packages('e1071')
install.packages('factoextra')
########################################################################
######################Loading the data##################################
######################################################################

################Load the file into a variable as a list################
fileList <- list.files(
  pattern="*.csv$",
  recursive=TRUE,
  full.name=TRUE,
)
fileList

#############bind to a csv####################################
df <-do.call(rbind, lapply(fileList, function(f){
  dat.fl <- read.csv(f) %>% 
    mutate(filename = f)
}))
glimpse(df)

#####################################################################
####################Cleaning the data###############################
####################################################################

##################Extract text from right hand side################
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

##################Get the year out###############################
df_new <-df %>% 
  mutate(Year = substr(substrRight(filename, 8), 1, 4)) %>%
  mutate(filename = tolower(filename)) %>% 
  mutate(filename =  gsub("principal_offence_category_", "", filename)) %>% 
  separate(filename, c("Month", "Year2"), sep = "_") %>% 
  mutate(Year = gsub(".csv", "", Year)) %>% 
  mutate(Month = substring(Month, 8)) %>% 
  select(-Year2) %>% 
  rename(location = X)
glimpse(df_new)
################Seperate the year from the file names#################
df_new$Month <- str_split(df_new$Month, "/", simplify = TRUE)[,3]
head(df_new)

############################Removal of outlier######################
df_removepercentage <- df_new %>%
  select(-starts_with("Percentage"))
df_removepercentage <-  df_removepercentage[ !(df_removepercentage$location == 'National'), ]
head(df_removepercentage)

#################################Transforming the columns###############
rm_comma <- function(x){
  gsub(",","",x)
}
df1 <- as.data.frame(lapply(df_removepercentage,rm_comma))
forceint <- df1[,-c(1,27, 28)]
selectedcolumns <- df1[,c(1,27,28)]
toint <- sapply(forceint, as.numeric)
cleanedata <- cbind(toint,selectedcolumns)
rm("toint","selectedcolumns","forceint","df1")
glimpse(cleanedata)

########################Cleaning column names#######################
colnames(cleanedata)<-gsub("Number.of.","",colnames(cleanedata))
cleanedata <- cleanedata %>%
  clean_names()
glimpse(cleanedata)

####################################################################
###################Feature Engineering#############################
###################################################################

#######################Adding quarter column########################
cleanedata$date <-  match(str_to_title(cleanedata$month),month.name)
cleanedata$Quarter <- ceiling(as.numeric(cleanedata$date) / 3)
glimpse(cleanedata)

#######################Convert into long data########################
maindata <-  cleanedata[ !(cleanedata$location == 'National'), ] 
summary(maindata)
colnames(maindata)<-gsub("Number.of.","",colnames(maindata))
maindata <- maindata %>%
  clean_names()
maindatalong <- maindata %>%
  gather(crime_type,count_of_crime_type,homicide_convictions:admin_finalised_unsuccessful)
head(maindatalong)

#######################Adding a succesful Column########################
maindatalong <- maindatalong %>%
  mutate(is_successful = case_when(
    !(endsWith(crime_type, "_unsuccessful")) ~ "succesful",
    endsWith(crime_type, "_unsuccessful") ~ "not succesful"
  ))
head(maindatalong)

####################################################################
####################Data Exploration############################
###################################################################

ggplot(maindatalong,aes(x = crime_type, y= count_of_crime_type)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(vjust =0.5,angle=90))+
  labs(title = 'trying out', x ="Sum of crime", y = "type of crime")

ggplot(maindatalong, aes(year, count_of_crime_type))+
  geom_bar(position="stack", stat="identity", aes(fill=is_successful))+
  labs(title="count of succsful conviction rates", x="Year", y="rate of convictions")+
  expand_limits(x = c(0, NA), y = c(0, NA)) +
  scale_y_continuous(labels = scales::label_number_si()) 

year_by_2014 <- maindatalong %>% 
  filter(year == "2014")
year_by_2015 <- maindatalong %>% 
  filter(year == "2015")
year_by_2016 <- maindatalong %>% 
  filter(year == "2016")
ggplot(data = year_by_2014, mapping=aes(quarter, count_of_crime_type)) + 
  geom_bar(mapping=aes(fill = is_successful), stat = "identity")+
  expand_limits(x = c(0, NA), y = c(0, NA)) +
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(title="count of succesful conviction rates",x="quarter 0f 2014",y="amount of convictions")
ggplot(data = year_by_2015, mapping=aes(quarter, count_of_crime_type)) + 
  geom_bar(mapping=aes(fill = is_successful), stat = "identity")+
  expand_limits(x = c(0, NA), y = c(0, NA)) +
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(title="count of succesful conviction rates",x="quarter of 2015",y="amount of convictions")

ggplot(data = year_by_2014, mapping=aes(location, count_of_crime_type)) + 
  geom_bar(mapping=aes(fill = is_successful), stat = "identity")+
  expand_limits(x = c(0, NA), y = c(0, NA)) +
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(title="count of succesful conviction rates",x="location of 2014",y="amount of convictions")+
  theme(axis.text.x = element_text(vjust =0.5,angle=90))
ggplot(data = year_by_2015, mapping=aes(location, count_of_crime_type)) + 
  geom_bar(mapping=aes(fill = is_successful), stat = "identity")+
  expand_limits(x = c(0, NA), y = c(0, NA)) +
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(title="count of succesful conviction rates",x="location of 2015",y="amount of convictions")+
  theme(axis.text.x = element_text(vjust =0.5,angle=90))

succesful_conviction <- maindatalong %>%
  filter(is_successful =="succesful") 
ggplot(data = succesful_conviction, mapping=aes(location, count_of_crime_type)) + 
  geom_bar(position="dodge",mapping=aes(fill = year), stat = "identity")+
  expand_limits(x = c(0, NA), y = c(0, NA)) +
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(title="count of succesful conviction per location and year",x="location",y="amount of convictions")+
  theme(axis.text.x = element_text(vjust =0.5,angle=90))

unsuccesful_conviction <- maindatalong %>%
  filter(is_successful =="not succesful") 
ggplot(data = unsuccesful_conviction, mapping=aes(location, count_of_crime_type)) + 
  geom_bar(position="dodge",mapping=aes(fill = year), stat = "identity")+
  expand_limits(x = c(0, NA), y = c(0, NA)) +
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(title="count of unsuccesful conviction per location and year",x="location",y="amount of convictions")+
  theme(axis.text.x = element_text(vjust =0.5,angle=90))

ggplot(data = succesful_conviction, mapping=aes(location, count_of_crime_type)) + 
  geom_bar(position="dodge",mapping=aes(fill = as.character(quarter)), stat = "identity")+
  expand_limits(x = c(0, NA), y = c(0, NA)) +
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(title="count of succesful conviction per quarter across location",x="location",y="amount of convictions")+
  theme(axis.text.x = element_text(vjust =0.5,angle=90))+
  guides(fill=guide_legend(title = "Quarter"))

ggplot(data = unsuccesful_conviction, mapping=aes(location, count_of_crime_type)) + 
  geom_bar(position="dodge",mapping=aes(fill = as.character(quarter)), stat = "identity")+
  expand_limits(x = c(0, NA), y = c(0, NA)) +
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(title="count of unsuccesful conviction per quarter across location",x="location",y="amount of convictions")+
  theme(axis.text.x = element_text(vjust =0.5,angle=90))+
  guides(fill=guide_legend(title = "Quarter"))

####################################################################
####################Hypothesis####################################
###################################################################

########################Hypothesis 1###############################
metropolitanarea <- maindata %>% filter(location == "Metropolitan and City")
head(metropolitanarea)

t.test(metropolitanarea$drugs_offences_convictions, metropolitanarea$sexual_offences_convictions)

t.test(metropolitanarea$drugs_offences_convictions, metropolitanarea$sexual_offences_convictions, var.equal = TRUE)
t.test(metropolitanarea$drugs_offences_convictions, mu = 2)
t.test(metropolitanarea$sexual_offences_convictions, mu = 2, alternative = 'greater')

wilcox.test(metropolitanarea$drugs_offences_convictions, metropolitanarea$sexual_offences_convictions)
wilcox.test(metropolitanarea$drugs_offences_convictions, exact = FALSE)

cor.test(metropolitanarea$drugs_offences_convictions, metropolitanarea$sexual_offences_convictions, method = "pearson")
cor.test(metropolitanarea$drugs_offences_convictions, metropolitanarea$sexual_offences_convictions, method = "spearman")
cor.test(metropolitanarea$drugs_offences_convictions, metropolitanarea$sexual_offences_convictions, method = "kendall")

ggplot(metropolitanarea, aes(x=drugs_offences_convictions, y=sexual_offences_convictions)) +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm, se=FALSE)

########################Hypothesis 2###############################
gloucestershirearea <- maindatalong %>% filter(is_successful == "not succesful" & location == "Gloucestershire")
hyp2 <- gloucestershirearea %>% 
  group_by(year,quarter) %>%
  summarise(perquater = sum(count_of_crime_type, na.rm = TRUE))
hyp2$yearquarter <- paste( hyp2$quarter, hyp2$year)
hyp2 <- as.data.frame(hyp2)
head(gloucestershirearea)

t.test(hyp2$quarter,hyp2$perquater)
t.test(hyp2$quarter,hyp2$perquater, var.equal = TRUE)
t.test(hyp2$quarter, mu = 2)
t.test(hyp2$perquater, mu = 2, alternative = 'greater')

wilcox.test(hyp2$quarter, hyp2$perquater)
wilcox.test(hyp2$quarter, exact = FALSE)

cor.test(hyp2$quarter,hyp2$perquater, method = "pearson")
cor.test(hyp2$quarter,hyp2$perquater, method = "spearman")
cor.test(hyp2$quarter,hyp2$perquater, method = "kendall")

ggplot(hyp2,aes(x=yearquarter, y=perquater)) +
  geom_point(alpha=0.5)+
  geom_smooth()


############################################################################
################################Machine Learning############################
############################################################################

###############################Linear regression############################
set.seed(125)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(maindata$sexual_offences_convictions, main="sexual offence convictions", sub=paste("Outlier rows: ", boxplot.stats(maindata$sexual_offences_convictions)$out))
boxplot(maindata$drugs_offences_convictions, main="drug offence convictions", sub=paste("Outlier rows: ", boxplot.stats(maindata$drugs_offences_convictions)$out)) 

par(mfrow=c(1, 2))
plot(density(maindata$sexual_offences_convictions), main="Density Plot: sexual convictions", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(maindata$sexual_offences_convictions), 2)))
polygon(density(maindata$sexual_offences_convictions), col="red")
plot(density(maindata$drugs_offences_convictions), main="Density Plot: drug convictions", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(maindata$drugs_offences_convictions), 2)))
polygon(density(maindata$drugs_offences_convictions), col="red")

cor(maindata$sexual_offences_convictions, maindata$drugs_offences_convictions)
lmheight = lm(sexual_offences_convictions ~ drugs_offences_convictions, data = maindata)
summary(lmheight)
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(maindata), 0.8*nrow(maindata))  # row indices for training data
trainingData <- maindata[trainingRowIndex, ]  # model training data
testData  <- maindata[-trainingRowIndex, ]   # test data
lmMod <- lm(sexual_offences_convictions ~ drugs_offences_convictions, data=trainingData)
drugconvictions <- predict(lmMod, testData)
summary (lmMod)
AIC (lmMod)
BIC(lmMod)

#################################Classification############################
maindata$year <- as.numeric(maindata$year)
nor <-function(x) { (x -min(x))/(max(x)-min(x)) }
maindatanorm <- as.data.frame(lapply(maindata[,c(1:25)], nor))
class_train <- maindatanorm[trainingRowIndex,]
class_test  <- maindatanorm[-trainingRowIndex, ]
maindata_target_category <- maindata[trainingRowIndex,26]
maindata_test_category <- maindata[-trainingRowIndex,26]
library(class)
pr <- knn(class_train,class_test,cl=maindata_target_category,k=18)
tab <- table(pr,maindata_test_category)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
##################################Clustering####################################
cluster_data <- maindata %>%
  select(-contains("unsuccessful"))
cluster <- cluster_data[,1:13]
head(cluster)
library(caret)
dummy <- dummyVars("~.",data = cluster)
newdata <- data.frame(predict(dummy,newdata=cluster))
km = kmeans(newdata,3, nstart=25)
head(km)
library(factoextra)
fviz_cluster(km, data = newdata,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"),
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw()
)