#Jake Gluck - Capital News Service 

# Welcome to the Capital News Service pedestrian clustering analysis script.  

#This clears the enviorment of variables
rm(list=ls())

# install dbscan package, which is the clustering algo we'll use to identify pedestrian crash hotspots
install.packages("fpc")
install.packages("tidyverse")

# load library
library(fpc)
library(tidyverse)


# Import Maryland State Police vehicle crash data.  Raw data is here: https://github.com/Capital-News-Service/maryland-crash-statistics, with data dictionary. These were originally stored as Excel files with multipel sheets.  We've stripped out those sheets into separate CSVs and then read in.  We're just reading in crash tables and person tables for each quarter.  Note that we're removing certain fields on read in to ensure that 2016 and 2017 fields match 2015 fields. 
#get data from each quarter and resize 2016 and 2017 tables to match 2015

crash71p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Pedestrian\\2017.Q1.CRASH.csv", header = TRUE)
crash71 <- crash71p[c(1:11,13:37)]

person71p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Pedestrian\\2017.Q1.PERSON.csv", header = TRUE)
person71 <- person71p[c(1:20,22:28)]

crash72p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Pedestrian\\2017.Q2.CRASH.csv", header = TRUE)
crash72 <- crash72p[c(1:11,13:37)]

person72p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Pedestrian\\2017.Q2.PERSON.csv", header = TRUE)
person72 <- person72p[c(1:20,22:28)]

crash61p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Pedestrian\\2016.Q1.CRASH.csv", header = TRUE)
crash61 <- crash61p[c(1:11,13:37)]

person61p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Pedestrian\\2016.Q1.PERSON.csv", header = TRUE)
person61 <- person61p[c(1:20,22:28)]

crash62p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Pedestrian\\2016.Q2.CRASH.csv", header = TRUE)
crash62 <- crash62p[c(1:11,13:37)]

person62p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Pedestrian\\2016.Q2.PERSON.csv", header = TRUE)
person62 <- person62p[c(1:20,22:28)]

crash63p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Pedestrian\\2016.Q3.CRASH.csv", header = TRUE)
crash63 <- crash63p[c(1:11,13:37)]

person63p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Pedestrian\\2016.Q3.PERSON.csv", header = TRUE)
person63 <- person63p[c(1:20,22:28)]

crash64p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Pedestrian\\2016.Q4.CRASH.csv", header = TRUE)
crash64 <- crash64p[c(1:11,13:37)]

person64p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Pedestrian\\2016.Q4.PERSON.csv", header = TRUE)
person64 <- person64p[c(1:20,22:28)]

crash51 <- read.csv(file="C:\\Users\\jagluck\\Documents\\Pedestrian\\2015.Q1.CRASH.csv", header = TRUE)
person51 <- read.csv(file="C:\\Users\\jagluck\\Documents\\Pedestrian\\2015.Q1.PERSON.csv", header = TRUE)

crash52 <- read.csv(file="C:\\Users\\jagluck\\Documents\\Pedestrian\\2015.Q2.CRASH.csv", header = TRUE)
person52 <- read.csv(file="C:\\Users\\jagluck\\Documents\\Pedestrian\\2015.Q2.PERSON.csv", header = TRUE)

crash53 <- read.csv(file="C:\\Users\\jagluck\\Documents\\Pedestrian\\2015.Q3.CRASH.csv", header = TRUE)
person53 <- read.csv(file="C:\\Users\\jagluck\\Documents\\Pedestrian\\2015.Q3.PERSON.csv", header = TRUE)

crash54 <- read.csv(file="C:\\Users\\jagluck\\Documents\\Pedestrian\\2015.Q4.CRASH.csv", header = TRUE)
person54 <- read.csv(file="C:\\Users\\jagluck\\Documents\\Pedestrian\\2015.Q4.PERSON.csv", header = TRUE)


#remove wrongly sized tables from enviornment
rm(crash61p)
rm(crash62p)
rm(crash63p)
rm(crash64p)
rm(crash71p)
rm(crash72p)
rm(person72p)
rm(person71p)
rm(person61p)
rm(person62p)
rm(person63p)
rm(person64p)




#combine all quarters into one table for person and one table for crash
#282828 records in crash
#623691 records in person
person <- rbind(person51,person52,person53,person54,person61,person62,person63,person64,person71,person72)
crash <- rbind(crash51,crash52,crash53,crash54,crash61,crash62,crash63,crash64,crash71,crash72)

#remove quarter tables from enviornment
rm(crash51)
rm(crash52)
rm(crash53)
rm(crash54)
rm(person51)
rm(person52)
rm(person53)
rm(person54)
rm(crash61)
rm(crash62)
rm(crash63)
rm(crash64)
rm(crash71)
rm(crash72)
rm(person72)
rm(person71)
rm(person61)
rm(person62)
rm(person63)
rm(person64)

# Check the range of values in PERSON_TYPE - total of 11982 P
person_type_count <- person %>%
  group_by(PERSON_TYPE) %>%
  summarise(count=n()) 
View(person_type_count)

#Filter out only pedestrians from our person table
person <- subset(person, PERSON_TYPE=="P")

# joins crash and person tables into one table
combine <- merge(crash, person, by="REPORT_NO")

# The one flaw we've identified after merging this is a record MCP21950009 that appears twice in the data, once as a 2015 crash, once as a 2017 crash.  The person number was also repeated twice, resulting in four extra record after the combine.  This code here is what we used to identify the problematic record.   

person_count <- combine %>%
  group_by(REPORT_NO, PERSON_ID) %>%
  summarise(count=n()) 

# Remove three copies of this problematic record. This results in us having, in the combined file, 11980 records, which is why it's 2 less records in combine than in person. 
combine <- combine %>% distinct()

combine <- combine %>%
  filter(REPORT_NO != 'MCP21950009')

#sorts out only pedestrian accidents
pedestrian_large <- subset(combine, PERSON_TYPE=="P")

#create subset for state and us highways
us_roads <- subset(pedestrian_large, ROUTE_TYPE_CODE=="US")
state_roads <- subset(pedestrian_large, ROUTE_TYPE_CODE=="MD")

#combine US and state roads into one table
us_and_stater = rbind(us_roads, state_roads)


#extracts only fields we want
pedestrian <- pedestrian_large[c(1,3,12,13,14,20,21,22,23,27,34,35,36,39,44,46,56,57)]
us_and_stater = us_and_stater[c(1,3,12,13,14,20,21,22,23,27,34,35,36,39,44,46,56,57)]

#removes row names
rownames(pedestrian) <- c()
rownames(us_and_stater) <- c()

#remove baltimore data
us_and_state_nobmore <- subset(us_and_stater, COUNTY_NO!=24)

#write data to csv file, this is for the us and state no baltimore
write.csv(pedestrian, file = "us_and_state_nobmore.csv")

#write data to csv file, this is for the entire file
#write.csv(pedestrian, file = "pedestrian.csv")

#write data to csv file, this is for just US and MD roads
#write.csv(us_and_stater, file = "pedestrian_state_and_us.csv")


###Clustering###

# set plots bg-color
par(bg="grey80")

# import your point data
data <-us_and_state_nobmore[c(12,13)]

# create a working object
data2=data

# plot points
plot(data2)

# run DBSCAN clustering
ds <- dbscan(data2, eps=.005, MinPts=5, showplot=1)

# create a vector with the cluster prediction
pre <- predict(ds,data2)

# join the prediction to your base data-object
exp <- data.frame(data,pre)

#this code is for selecting an individual cluster
#exp <- subset(exp, pre>87)
#exp <- subset(exp, pre<89)

#add information back to cluster
cluster = merge(us_and_state_nobmore, exp, by= c("LATITUDE", "LONGITUDE"))

#remove duplicate rows
cluster = unique(cluster)

#remove noise points
cluster <- subset(cluster, pre!=0)

# write to csv
write.table(cluster, "clusters.csv", sep=",", row.names = F)


