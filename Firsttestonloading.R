#List a csv files in all subdirectories
setwd("D:/Projects/Assignment/Data Analysis and Visualisation")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("ggmaP")
install.packages("tidygeocoder")

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggmap)
library(tidygeocoder)

fileList <- list.files(
  pattern="*.csv$",
  recursive=TRUE,
  full.name=TRUE,
)

#read and bind all files
df <-do.call(rbind, lapply(fileList, function(f){
  dat.fl <- read.csv(f) %>% 
    mutate(filename = f)
}))

#Extract text from right hand side
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#See unique file names
df$filename %>% 
  unique()

#Get the year out
df_new <-df %>% 
  mutate(Year = substr(substrRight(filename, 8), 1, 4)) %>%
  mutate(filename = tolower(filename)) %>% 
  mutate(filename =  gsub("principal_offence_category_", "", filename)) %>% 
  separate(filename, c("Month", "Year2"), sep = "_") %>% 
  mutate(Year = gsub(".csv", "", Year)) %>% 
  mutate(Month = substring(Month, 8)) %>% 
  select(-Year2) %>% 
  rename(location = X)

#Seperate the year from the file names 
df_new$Month <- str_split(df_new$Month, "/", simplify = TRUE)[,3]
df_new$Month

#remove the percentage columns as they are not really needed
df_removepercentage <- df_new %>%
  select(-starts_with("Percentage"))

#Remove commas from data that has the comma
rm_comma <- function(x){
  gsub(",","",x)
}
df1 <- as.data.frame(lapply(df_removepercentage,rm_comma))
df1

#remove the columns that can be affected by forcing columns to int 
forceint <- df1[,-c(1,27, 28)]
selectedcolumns <- df1[,c(1,27,28)]
#Force Columns to be integers 
toint <- sapply(forceint, as.numeric)

#Joining the columns that were removed back together 
cleanedata <- cbind(toint,selectedcolumns)

rm("toint","selectedcolumns","forceint")

#Get Date out of the months 
cleanedata$date <-  match(str_to_title(cleanedata$Month),month.name)

#convert the months to quarters 
cleanedata$Quarter <- ceiling(as.numeric(cleanedata$date) / 3)

try <- c("March","December")
match(try, month.name)

by_location <- df_new %>% arrange(location)
by_location

by_quarter <- cleanedata %>% arrange(Quarter)

by_year <- df_new %>% arrange(Year)
by_year

by_month <- df_new %>% arrange(Month)
by_month

rm_columnrename <- function(x){
  gsub("Number.of.","",x)
  gsub(".","",x)
}

cleanedata <- cleanedata %>%
  rename_with(rm_columnrename)

by_month <- sapply(by_month, as.numeric)

by_location1 <- filter(df_new, df_new$location == df_new$location)

#removing all the integers to check for correlation among
selectdataforcor <- cleanedata[,-c(26,27, 28)]

#check corellation using all the correlation method in r and giving reason why thy are 
c("pearson", "kendall", "spearman")
cor(selectdataforcor, method ="kendall")


summarisedbyquater <- cleanedata %>% 
  group_by(cleanedata$Quarter) %>% 
  summarise(across(where(is.numeric), list(sum = sum)))

ggplot(summarisedbyquater, aes(summarisedbyquater$cleanedata$Quarter)) + geom_line()


ggplot(summarisedbyquater) +
  geom_histogram(aes(x = -(summarisedbyquater$cleandata$Quarter), color = Quarter)) 


#getting geolocations of the regions
#convert the Greater manchester area to Just Manchester https://statisticsglobe.com/replace-particular-value-in-data-frame-in-r
cleanedata$location[cleanedata$location == "GreaterManchester"] <- "Manchester"
regions <- unique(cleanedata[,26])
regions_df <- as.data.frame(regions) 
regions_df <- regions_df[-1,] # remove the national from the list
locations_df <- geo_osm(regions_df)#adding the latitude and longitude to various areas in the region https://www.supplychaindataanalytics.com/geocoding-spatial-data-with-tidygeocoder-in-r/
#mapping the latitude and longitude to its own region in the clean data set https://stackoverflow.com/questions/59990820/r-is-there-a-pandas-function-map-in-r
cleanedata <- transform(cleanedata, lat = locations_df$lat[match(cleanedata$location, locations_df$address)]) 
cleanedata <- transform(cleanedata, long = locations_df$long[match(cleanedata$location, locations_df$address)])

#Removing all the national data as it is an addititon of all the regions combined https://stackoverflow.com/questions/22655060/remove-rows-conditionally-from-a-data-table-in-r
maindata <-  cleanedata[ !(cleanedata$location == 'National'), ] 

#quick visualization of the longitude and latitude with scatter plot
ggplot() +
  geom_point(data = maindata, aes(x = long, y = lat), alpha = 1)

every1stquarter <- maindata[ (maindata$Quarter == '1'), ] 
every1stquarter

#summarising the data and finding the sum by the quater to know how crime rate increases over the quarter https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group
#maindata %>%
 # group_by(Quarter,Year) %>%
  #summarise(
   #         Number.of.Offences.Against.The.Person.Convictions = sum(Number.of.Offences.Against.The.Person.Convictions),
    #        Number.of.Burglary.Convictions = sum(Number.of.Burglary.Convictions),
     #       Number.of.Robbery.Convictions = sum(Number.of.Robbery.Convictions)
      #      )
#Drop the year 2017 because it seems to be incomplete for every quarter and can't be fully utilised as we would be making many assumptions and would create many outliers
maindata %>%
  group_by(Quarter,Year) %>%
  summarise(
    Number.of.Offences.Against.The.Person.Convictions = sum(Number.of.Offences.Against.The.Person.Convictions),
    Number.of.Burglary.Convictions = sum(Number.of.Burglary.Convictions),
    Number.of.Robbery.Convictions = sum(Number.of.Robbery.Convictions)
  )

#Not going to be using the 2017 data as it is incomplete and inconsistent and would lead to a lot of outliers and when trying to solve for can lead to inaccurate information

