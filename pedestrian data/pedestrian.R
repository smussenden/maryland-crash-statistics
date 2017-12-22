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

crash71p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Github\\maryland-crash-statistics\\raw\\csv\\2017.Q1.CRASH.csv", header = TRUE)
crash71 <- crash71p[c(1:11,13:37)]

person71p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Github\\maryland-crash-statistics\\raw\\csv\\2017.Q1.PERSON.csv", header = TRUE)
person71 <- person71p[c(1:20,22:28)]

crash72p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Github\\maryland-crash-statistics\\raw\\csv\\2017.Q2.CRASH.csv", header = TRUE)
crash72 <- crash72p[c(1:11,13:37)]

person72p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Github\\maryland-crash-statistics\\raw\\csv\\2017.Q2.PERSON.csv", header = TRUE)
person72 <- person72p[c(1:20,22:28)]

crash61p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Github\\maryland-crash-statistics\\raw\\csv\\2016.Q1.CRASH.csv", header = TRUE)
crash61 <- crash61p[c(1:11,13:37)]

person61p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Github\\maryland-crash-statistics\\raw\\csv\\2016.Q1.PERSON.csv", header = TRUE)
person61 <- person61p[c(1:20,22:28)]

crash62p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Github\\maryland-crash-statistics\\raw\\csv\\2016.Q2.CRASH.csv", header = TRUE)
crash62 <- crash62p[c(1:11,13:37)]

person62p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Github\\maryland-crash-statistics\\raw\\csv\\2016.Q2.PERSON.csv", header = TRUE)
person62 <- person62p[c(1:20,22:28)]

crash63p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Github\\maryland-crash-statistics\\raw\\csv\\2016.Q3.CRASH.csv", header = TRUE)
crash63 <- crash63p[c(1:11,13:37)]

person63p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Github\\maryland-crash-statistics\\raw\\csv\\2016.Q3.PERSON.csv", header = TRUE)
person63 <- person63p[c(1:20,22:28)]

crash64p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Github\\maryland-crash-statistics\\raw\\csv\\2016.Q4.CRASH.csv", header = TRUE)
crash64 <- crash64p[c(1:11,13:37)]

person64p <- read.csv(file="C:\\Users\\jagluck\\Documents\\Github\\maryland-crash-statistics\\raw\\csv\\2016.Q4.PERSON.csv", header = TRUE)
person64 <- person64p[c(1:20,22:28)]

crash51 <- read.csv(file="C:\\Users\\jagluck\\Documents\\Github\\maryland-crash-statistics\\raw\\csv\\2015.Q1.CRASH.csv", header = TRUE)
person51 <- read.csv(file="C:\\Users\\jagluck\\Documents\\Github\\maryland-crash-statistics\\raw\\csv\\2015.Q1.PERSON.csv", header = TRUE)

crash52 <- read.csv(file="C:\\Users\\jagluck\\Documents\\Github\\maryland-crash-statistics\\raw\\csv\\2015.Q2.CRASH.csv", header = TRUE)
person52 <- read.csv(file="C:\\Users\\jagluck\\Documents\\Github\\maryland-crash-statistics\\raw\\csv\\2015.Q2.PERSON.csv", header = TRUE)

crash53 <- read.csv(file="C:\\Users\\jagluck\\Documents\\Github\\maryland-crash-statistics\\raw\\csv\\2015.Q3.CRASH.csv", header = TRUE)
person53 <- read.csv(file="C:\\Users\\jagluck\\Documents\\Github\\maryland-crash-statistics\\raw\\csv\\2015.Q3.PERSON.csv", header = TRUE)

crash54 <- read.csv(file="C:\\Users\\jagluck\\Documents\\Github\\maryland-crash-statistics\\raw\\csv\\2015.Q4.CRASH.csv", header = TRUE)
person54 <- read.csv(file="C:\\Users\\jagluck\\Documents\\Github\\maryland-crash-statistics\\raw\\csv\\2015.Q4.PERSON.csv", header = TRUE)


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

# Now we need to pull only people involved in accidents with a driver, there are a few cases where pedestrians are logged but there was no driver, such as two bikes hitting each other

# create a new object to protect person
newperson <- person 
  

#melt data to create one new column for each person type, if there is any value 1 or greater that indicates a person of that type is present in that record, which are populated by the injury severity code because it is always grater than one, if they are not that person type that column recieves value 0
newperson <- spread(person, PERSON_TYPE, INJ_SEVER_CODE, fill = 0)

#change all injury severity codes greater than 1 to 1. Now the colums are binarys indicating if they were that person type
newperson$D[newperson$D > 1] <- 1
newperson$O[newperson$O > 1] <- 1
newperson$P[newperson$P > 1] <- 1

#Group the data by report number and sum the person type column values, now each column has the total number of how many of each person type was involved in every accident
#At this point there are 275662
newsperson_new <- newperson %>%
  group_by(REPORT_NO) %>%
  summarise(drv=sum(D),
            occ=sum(O),
            ped=sum(P)
            ) 

#Now there are 274771 - remove 891 cases with no driver 
newsperson_new <- subset(newsperson_new, drv>0)

#get only pedestrian cases there are 10692
newsperson_new <- subset(newsperson_new, ped>0)


#merge back with data set there are 24835 people
new_person <- merge(newsperson_new, person, by="REPORT_NO")


#Filter out only pedestrians from our person table, now there is 11325
new_person <- subset(new_person, PERSON_TYPE=="P")

# joins crash and person tables into one table, this is now a table of one record per pedestrian involved in a crash with a motor vehicle
combine <- merge(crash, new_person, by="REPORT_NO")

# The one flaw we've identified after merging this is a record MCP21950009 that appears twice in the data, once as a 2015 crash, once as a 2017 crash.  The person number was also repeated twice, resulting in four extra record after the combine.  This code here is what we used to identify the problematic record.   

person_count <- combine %>%
  group_by(REPORT_NO, PERSON_ID) %>%
  summarise(count=n()) 

# Remove three copies of this problematic record. This results in us having, in the combined file, 11327 records.

combine <- combine %>%
  filter(row_number() != 9023) %>%
  filter(row_number() != 9022) %>%
  filter(row_number() != 9021) 

# Before filtering for SHA-only roads, look at range of ROUTE_TYPE_CODES.  Note that Of the 11,324 pedestrian hit in crashes, 3576 have no route type given.  An analysis of these null value records found that, though there was some geographic information, there was simply not enough information provided in the data set to say with any certainty that the crashes occured on state, county, US or other roads, or private parking lots. Some of them appeared to have been located on or near US or state roads.  Note that this means that the true number of pedestrian crashes on US and Maryland roads is likely to be higher, but that we can't say that with certainty.  Our analysis must be couched as "at least" and acknowledge these flaws.    

road_type_count <- combine %>%
  group_by(ROUTE_TYPE_CODE) %>%
  summarise(count=n()) 

no_route_code <- combine %>%
  filter(ROUTE_TYPE_CODE == "")

no_route_code_mainroad <- no_route_code %>%
  subset(COUNTY_NO!=24) %>%
  subset(MAINROAD_NAME != "")


# Create subset for state and us highways.  Note that there were 102 records for IS (Insterstate Route), but a decision was made to leave those out becuase of questions about SHA vs other agency control of different parts of those roads.  
us_roads <- subset(combine, ROUTE_TYPE_CODE=="US") # 2397 people in crashes
state_roads <- subset(combine, ROUTE_TYPE_CODE=="MD") # 581 people in crashes

#combine US and state roads into one table
us_and_stater = rbind(us_roads, state_roads)


#extracts only fields we want
pedestrian <- combine
us_and_stater <- us_and_stater

#removes row names
rownames(pedestrian) <- c()
rownames(us_and_stater) <- c()

#remove Baltimore city data because SHA doesn't maintain roads in Baltimore City
us_and_state_nobmore <- subset(us_and_stater, COUNTY_NO!=24)

#write data to csv file, this is for the us and state no baltimore
write.csv(us_and_state_nobmore, file = "us_and_state_nobmore.csv")

#write data to csv file, this is for the entire file
write.csv(pedestrian, file = "pedestrian.csv")

#write data to csv file, this is for just US and MD roads
#write.csv(us_and_stater, file = "pedestrian_state_and_us.csv")


###### Analysis #######

#total number of crashes on state and us roads between 2015 and the first half of 2017, there are 2281
total_crashes <- us_and_state_nobmore %>%
  group_by(REPORT_NO) %>%
  summarise(count=n())

nrow(total_crashes)

#number of pedestrians hit by vehicles on state and us roads between 2015 and the first half of 2017, there are 2402
nrow(us_and_state_nobmore)

#number of pedestrians killed by vehicles on state and us roads between 2015 and the first half of 2017, there are 147
killed <- us_and_state_nobmore %>%
  filter(INJ_SEVER_CODE == 5)

write.table(killed, "killed.csv", sep=",", row.names = F)

#number of pedestrians seriously injured by vehicles on state and us roads between 2015 and the first half of 2017, there are 1366
injured <- us_and_state_nobmore %>%
  filter(INJ_SEVER_CODE == 4 | INJ_SEVER_CODE == 3)

#number of bicyclists killed by vehicles on state and us roads between 2015 and the first half of 2017, there are 14

bicyclists <- us_and_state_nobmore %>%
  filter(PED_TYPE_CODE == 2)

bike_killed <- bicyclists %>%
  filter(INJ_SEVER_CODE == 5)

#number of bicyclists seriously injured by vehicles on state and us roads between 2015 and the first half of 2017, there are 14

bike_injured<- bicyclists %>%
  filter(INJ_SEVER_CODE == 4 | INJ_SEVER_CODE == 3)

###Clustering###

# set plots bg-color
par(bg="grey80")

# import your point data
data <-us_and_state_nobmore[c(35,36)]

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


#remove noise points that are not located in any cluster. That gives us 1113 persons in clusters
exp <- subset(exp, pre!=0)

#count the number in each cluster to make sure there is at least 5
exp_count <- exp %>%
  group_by(pre) %>%
  summarise(count=n())

#remove cluster 54, it only has 4 points
exp <- subset(exp, pre!=54)

# write to csv
write.table(exp, "clusters.csv", sep=",", row.names = F)


### CLUSTER ANALYSIS ##########


#here we gathered information about one small cluster around georgia and august drive. This approach may not work on a larger dataset, because if two accidents occured at the exact sam coordinates it would creat a duplicate row. However, this cluster only had 7 pedestrians so we could see it was not the case.

#Small cluster
small_cluster <- exp %>%
  filter(pre ==94)

small_cluster <- inner_join(small_cluster, us_and_state_nobmore, by=c('LONGITUDE', 'LATITUDE'))
rownames(small_cluster) <- c()
small_cluster <- distinct(small_cluster)

#oc cluster
oc_cluster <- exp %>%
  filter(pre ==61 | pre == 62 | pre == 63 | pre == 13) 


oc_cluster <- inner_join(oc_cluster, us_and_state_nobmore, by=c('LONGITUDE', 'LATITUDE'))
rownames(oc_cluster) <- c()
oc_cluster <- distinct(oc_cluster)

#branch cluster
branch_cluster <- exp %>%
  filter(pre ==65) 


branch_cluster <- inner_join(branch_cluster, us_and_state_nobmore, by=c('LONGITUDE', 'LATITUDE'))
rownames(branch_cluster) <- c()
branch_cluster <- distinct(branch_cluster)

# write to csv
write.table(branch_cluster, "branch.csv", sep=",", row.names = F)



