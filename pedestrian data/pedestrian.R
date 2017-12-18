#Jake Gluck - Capital News Service 

# Welcome to the Capital News Service pedestrian clustering analysis script.  

#This clears the enviorment of variables
rm(list=ls())

# install dbscan package, which is the clustering algo we'll use to identify pedestrian crash hotspots
install.packages("fpc")
install.packages("tidyverse")
install.packages("sqldf")

# load library
library(fpc)
library(tidyverse)
library(sqldf)


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

select <-person %>%
  group_by(REPORT_NO) %>%
  filter(PERSON_TYPE != "D")

select = sqldf("SELECT * from person
      GROUP BY PERSON.REPORT_NO
      HAVING (PERSON.PERSON_TYPE != 'D')")

# Check the range of values in PERSON_TYPE - total of 11982 P
person_type_count <- select %>%
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

# Remove three copies of this problematic record. This results in us having, in the combined file, 11981 records.

combine <- combine %>%
  filter(row_number() != 9575) %>%
  filter(row_number() != 9576) %>%
  filter(row_number() != 9577) 

# Before filtering for SHA-only roads, look at range of ROUTE_TYPE_CODES.  Note that Of the 11,981 pedestrian crashes, 3835 have no route type given.  An analysis of these null value records found that, though there was some geographic information, there was simply not enough information provided in the data set to say with any certainty that the crashes occured on state, county, US or other roads, or private parking lots. Some of them appeared to have been located on or near US or state roads.  Note that this means that the true number of pedestrian crashes on US and Maryland roads is likely to be higher, but that we can't say that with certainty.  Our analysis must be couched as "at least" and acknowledge these flaws.    

road_type_count <- combine %>%
  group_by(ROUTE_TYPE_CODE) %>%
  summarise(count=n()) 

no_route_code <- combine %>%
  filter(ROUTE_TYPE_CODE == "")

no_route_code_mainroad <- no_route_code %>%
  subset(COUNTY_NO!=24) %>%
  subset(MAINROAD_NAME != "")


# Create subset for state and us highways.  Note that there were 103 records for IS (Insterstate Route), but a decision was made to leave those out becuase of questions about SHA vs other agency control of different parts of those roads.  
us_roads <- subset(combine, ROUTE_TYPE_CODE=="US") # 2495 people in crashes
state_roads <- subset(combine, ROUTE_TYPE_CODE=="MD") # 609 people in crashes

#combine US and state roads into one table
us_and_stater = rbind(us_roads, state_roads)


#extracts only fields we want
pedestrian <- combine[c(1,3,12,13,14,20,21,22,23,27,34,35,36,39,44,46,56,57)]
us_and_stater <- us_and_stater[c(1,3,12,13,14,20,21,22,23,27,34,35,36,39,44,46,56,57)]

#removes row names
rownames(pedestrian) <- c()
rownames(us_and_stater) <- c()

#remove Baltimore city data because SHA doesn't maintain roads in Baltimore City
us_and_state_nobmore <- subset(us_and_stater, COUNTY_NO!=24)

#write data to csv file, this is for the us and state no baltimore
write.csv(pedestrian, file = "us_and_state_nobmore.csv")

#write data to csv file, this is for the entire file
write.csv(pedestrian, file = "pedestrian.csv")

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

# run DBSCAN clustering.  It takes two core arguments.  The first is MinPts, which is the minimum number of points in a cluster.  The second is the epsilon neighborhood (EPS), which is a proxy for density.  It says that for a point to be included, it must be within .005 (accounting for degrees lat and long) of core points of that cluster. FOR MORE: https://www.aaai.org/Papers/KDD/1996/KDD96-037.pdf
ds <- dbscan(data2, eps=.005, MinPts=5, showplot=1)

# create a vector with the cluster prediction
pre <- predict(ds,data2)

# join the prediction to your base data-object
exp <- data.frame(data,pre)

#this code is for selecting an individual cluster
#exp <- subset(exp, pre>87)
#exp <- subset(exp, pre<89)


#remove noise points that are not located in any cluster. That gives us 1157 persons in clusters
exp <- subset(exp, pre!=0)

# write to csv
write.table(exp, "clusters.csv", sep=",", row.names = F)


