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

crash71p <- read.csv(file="raw/csv/2017.Q1.CRASH.csv", header = TRUE)
crash71 <- crash71p[c(1:11,13:37)]

person71p <- read.csv(file="raw/csv/2017.Q1.PERSON.csv", header = TRUE)
person71 <- person71p[c(1:20,22:28)]

crash72p <- read.csv(file="raw/csv/2017.Q2.CRASH.csv", header = TRUE)
crash72 <- crash72p[c(1:11,13:37)]

person72p <- read.csv(file="raw/csv/2017.Q2.PERSON.csv", header = TRUE)
person72 <- person72p[c(1:20,22:28)]

crash61p <- read.csv(file="raw/csv/2016.Q1.CRASH.csv", header = TRUE)
crash61 <- crash61p[c(1:11,13:37)]

person61p <- read.csv(file="raw/csv/2016.Q1.PERSON.csv", header = TRUE)
person61 <- person61p[c(1:20,22:28)]

crash62p <- read.csv(file="raw/csv/2016.Q2.CRASH.csv", header = TRUE)
crash62 <- crash62p[c(1:11,13:37)]

person62p <- read.csv(file="raw/csv/2016.Q2.PERSON.csv", header = TRUE)
person62 <- person62p[c(1:20,22:28)]

crash63p <- read.csv(file="raw/csv/2016.Q3.CRASH.csv", header = TRUE)
crash63 <- crash63p[c(1:11,13:37)]

person63p <- read.csv(file="raw/csv/2016.Q3.PERSON.csv", header = TRUE)
person63 <- person63p[c(1:20,22:28)]

crash64p <- read.csv(file="raw/csv/2016.Q4.CRASH.csv", header = TRUE)
crash64 <- crash64p[c(1:11,13:37)]

person64p <- read.csv(file="raw/csv/2016.Q4.PERSON.csv", header = TRUE)
person64 <- person64p[c(1:20,22:28)]

crash51 <- read.csv(file="raw/csv/2015.Q1.CRASH.csv", header = TRUE)
person51 <- read.csv(file="raw/csv/2015.Q1.PERSON.csv", header = TRUE)

crash52 <- read.csv(file="raw/csv/2015.Q2.CRASH.csv", header = TRUE)
person52 <- read.csv(file="raw/csv/2015.Q2.PERSON.csv", header = TRUE)

crash53 <- read.csv(file="raw/csv/2015.Q3.CRASH.csv", header = TRUE)
person53 <- read.csv(file="raw/csv/2015.Q3.PERSON.csv", header = TRUE)

crash54 <- read.csv(file="raw/csv/2015.Q4.CRASH.csv", header = TRUE)
person54 <- read.csv(file="raw/csv/2015.Q4.PERSON.csv", header = TRUE)


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
#282828 records in crash - confirmedSM
#623691 records in person - confirmedSM
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

# Now we need to pull only people involved in accidents with a driver. There are a few cases where pedestrians are logged but there was no driver, such as two bikes hitting each other.

# create a new object to protect original person table
newperson <- person 

#melt data to create one new column for each person type. If there is any value 1 or greater in a given person_type column, that indicates a person of that type is present in that record. (It's pulling the injury severity code for each record and dropping it there. If there was not a person type, that column recieves value 0.  Note that when we do this, the PERSON_
newperson <- spread(person, PERSON_TYPE, INJ_SEVER_CODE, fill = 0)

#change all injury severity codes greater than 1 to 1. Now the colums are binarys indicating if they were of that person type, or not. 
newperson$D[newperson$D > 1] <- 1
newperson$O[newperson$O > 1] <- 1
newperson$P[newperson$P > 1] <- 1

#Group the data by report number and sum the person type column values. Now each column has the total number of how many of each person type was involved in every accident.  
#At this point there are 275662 observations (1 per crash)
crash_persontype <- newperson %>%
  group_by(REPORT_NO) %>%
  summarise(drv=sum(D),
            occ=sum(O),
            ped=sum(P)
            ) 

#Now let's remove any cases where there is no driver, which would take pedestrian-only accidents out of the mix. This leaves us with 274,771 records 
crash_persontype <- subset(crash_persontype, drv>0)

#Now, from the subset where we have only crashes with a driver, let's make sure we get only cases involving a pedestrian. This leaves us with 10,692 records.
crash_persontype <- subset(crash_persontype, ped>0)

#Now that we have a data set of case numbers only where the case involved a pedestrian and a driver(s), let's merge it back with our person table, so that we are left with a data set with one record per person for each of those pedestrian-driver crashes. This is the "person" table we'll use going forward, and it has 24,835 records, one per person involved in crashes. Note that at this point it includes drivers, pedestrians and occupants. 
filter_person <- merge(crash_persontype, person, by="REPORT_NO")

#Filter out only pedestrians from our new "person" table, so we're only left with pedestrians. We have 11,325 records (people) in this table.  
filter_person <- subset(filter_person, PERSON_TYPE=="P")

# Join crash and person tables into one table. This is now a table of one record per pedestrian involved in a crash with a motor vehicle, with data from both the "person" table and the crash table. This gives us 11,327 records, which is two more than our person table, which indicates an issue.
combine <- merge(crash, filter_person, by="REPORT_NO")

# Let's figure out why.  This code here is what we used to identify the problematic record.    The one flaw we've identified after merging this is a record MCP21950009 that appears twice in the data, once as a 2015 crash, once as a 2017 crash.  The person number was also repeated twice, resulting in four extra record after the combine. 

person_count <- combine %>%
  group_by(REPORT_NO, PERSON_ID) %>%
  summarise(count=n()) 

# Remove three copies of this problematic record, using reference row numbers.   This results in us having, in the combined file, 11324 records, which is correct (because we took out both copies of 2015 crash, and one copy of 2017).

combine <- combine %>%
  filter(row_number() != 9023) %>%
  filter(row_number() != 9022) %>%
  filter(row_number() != 9021) 

# Now filter out Baltimore city records, because Baltimore city roads are not maintained by SHA.  That leaves us with 7,864 records (people hit by cars)
combine <- combine %>%
  filter(COUNTY_NO != 24)

# Before filtering for SHA-only roads, look at range of ROUTE_TYPE_CODES.  Note that of the 7,864 pedestrian hit in crashes, 1915 have no route type given.  

road_type_count <- combine %>%
  group_by(ROUTE_TYPE_CODE) %>%
  summarise(count=n()) 

# These are the results.  
#MD (Maryland route) 2010
#US (US route) 392
#Null (Missing data) 1915
#IS (Interstate) 88
#CO (County route) 2670
#MU (Municipal route) 666
#GV (US government route) 14
#OP (Other public route) 75
#RP (Ramp) 9
#SR (Other nonSHA MDOT maintained state road) 23
#UU (No idea) 2

# This is to do a visual inspection of all the null value for route type, figure out what we need to do about them. Note that there are 1915 of them.
no_route_code <- combine %>%
  filter(ROUTE_TYPE_CODE == "")

# This is to look at only those with a mainroad name given. We get 144 records.  Ultimately, we made a determination to exclude these -- and all null values -- from our analysis, because we could not confirm with any real certainty their location. 
no_route_code_mainroad <- no_route_code %>%
  subset(MAINROAD_NAME != "")


# Create subset for state and us highways.  Note that there were 88 records for IS (Insterstate Route), but a decision was made to leave those out becuase of questions about SHA vs other agency control of different parts of those roads.  
us_roads <- subset(combine, ROUTE_TYPE_CODE=="US") # 392 people 
state_roads <- subset(combine, ROUTE_TYPE_CODE=="MD") # 2010 people

#combine US and state roads into one table - 2402 observations (people hit by cars)
us_and_state_no_bmore <- rbind(us_roads, state_roads)

#removes row names
rownames(combine) <- c()
rownames(us_and_state_no_bmore) <- c()

#write data to csv file, this is for the us and state roads no baltimore
write.csv(us_and_state_no_bmore, file = "us_and_state_nobmore.csv")

#write data to csv file, this is for the entire file
write.csv(combine, file = "all_roads_nobmore.csv")



###### Analysis #######

#total number of crashes (not people, crashes) on state and us roads (no Baltimore city, no interstate) between 2015 and the first half of 2017: 2281
total_crashes <- us_and_state_no_bmore %>%
  group_by(REPORT_NO) %>%
  summarise(count=n())
nrow(total_crashes)

#number of pedestrians hit by vehicles on state and us roads (no Baltimore city, no interstate) between 2015 and the first half of 2017: 2402
nrow(us_and_state_no_bmore)

#number of pedestrians in each injury severity category state and us roads (no Baltimore city, no interstate) between 2015 and the first half of 2017
total_injury_group <- us_and_state_no_bmore %>%
  group_by(INJ_SEVER_CODE) %>%
  summarise(count=n())
#1=291
#2=598
#1+2=889
#3=991
#4=375
#3+4=1366
#5=147



#number of pedestrians killed by vehicles on state and us roads  (no Baltimore city, no interstate) between 2015 and the first half of 2017: 147
killed <- us_and_state_no_bmore %>%
  filter(INJ_SEVER_CODE == 5)
nrow(killed)

# In case we want to visualize this 147 killed
write.table(killed, "killed.csv", sep=",", row.names = F)

#number of pedestrians seriously injured by vehicles on state and us roads  (no Baltimore city, no interstate) between 2015 and the first half of 2017: 1366
injured <- us_and_state_no_bmore %>%
  filter(INJ_SEVER_CODE == 4 | INJ_SEVER_CODE == 3)
nrow(injured)

#number of bicyclists (total) hit by vehicles on state and us roads (no Baltimore city, no interstate) between 2015 and the first half of 2017: 509

bicyclists <- us_and_state_no_bmore %>%
  filter(PED_TYPE_CODE == 2)
nrow(bicyclists)

#number of bicyclists killed by vehicles on state and us roads (no Baltimore city, no interstate) between 2015 and the first half of 2017: 14

bike_killed <- bicyclists %>%
  filter(INJ_SEVER_CODE == 5)
nrow(bike_killed)

#number of bicyclists seriously injured by vehicles on state and us roads (no Baltimore city, no interstate) between 2015 and the first half of 2017: 295

bike_injured<- bicyclists %>%
  filter(INJ_SEVER_CODE == 4 | INJ_SEVER_CODE == 3)
nrow(bike_injured)

###Clustering. Let's group these crashes into clusters###

# set plots bg-color
par(bg="grey80")

# import your point data
data <-us_and_state_no_bmore[c(35,36)]

# create a working object
data2<-data

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


#remove noise points that are not located in any cluster. That gives us 1113 persons in clusters (about half as many as there are total 2,402)
exp <- subset(exp, pre!=0)

#count the number in each cluster to make sure there is at least 5
exp_count <- exp %>%
  group_by(pre) %>%
  summarise(count=n())

#remove cluster 54, it only has 4 points
exp <- subset(exp, pre!=54)

#count the number of clusters: 100
nrow(exp_count)

# write to csv
write.table(exp, "clusters.csv", sep=",", row.names = F)


### CLUSTER ANALYSIS ##########

# Count of the number of people hit in Wheaton cluster, which is pre 93: Total of 42
wheaton_count <- exp %>%
  filter(pre == 93) %>%
  group_by(pre) %>%
  summarise(count=n())
View(wheaton_count)
# Count of the number of people hit in Aspen Hill cluster, which is pre 91: Total of 23
aspen_count <- exp %>%
  filter(pre == 91) %>%
  group_by(pre) %>%
  summarise(count=n())
View(aspen_count)

# Count of the number of people hit in August Ave. cluster, which is pre 94: Total of 7
august_count <- exp %>%
  filter(pre == 94) %>%
  group_by(pre) %>%
  summarise(count=n())
View(august_count)

# Count of the number of people hit in downtown Silver Spring cluster, which is pre 16: Total of 45

ss_count <- exp %>%
  filter(pre == 16) %>%
  group_by(pre) %>%
  summarise(count=n())
View(ss_count)

# Count of the number of people hit in four Ocean City clusters, which are 61,62,63, 13): Total of 83

oc_count <- exp %>%
  filter(pre == 61 | pre == 62 | pre == 63 | pre == 13) %>%
  group_by(pre) %>%
  summarise(count=n())
oc_count_total <- oc_count %>%
  summarise(sum=sum(count))
View(oc_count_total)


# To check total number of people injured and killed in each cluster, we need to join our file of clusters by geo-cordinates back with master data set (which contains injury severity code).
# Note: it's CRITICAL after doing the join to manually inspect each of these files, because the fact that we're joining by geocordinates means that, in the off chance two distinct accidents have the same geocode, it will create extra records.  If there are multiople people hit in same accident (and thus same geocode), we can account for that by removing dups.  But it wont catch two accidents with same geocode. 

#Wheaton Cluster Injury Analysis
#Filter out just the Wheaton people from exp - 42 records
wheaton_cluster <- exp %>%
  filter(pre ==93)
#Join it back with main dataset - 44 records, which is two extra
wheaton_cluster_join <- inner_join(wheaton_cluster, us_and_state_no_bmore, by=c('LONGITUDE', 'LATITUDE'))
#Remove rownames so next step will work
rownames(wheaton_cluster_join) <- c()
#Select only distinct records to eliminate duplicates created by joining in cases where more than one person was hit in a single accident - 42 records, correct
wheaton_cluster_join <- distinct(wheaton_cluster_join)
# Group by injury severity and count
wheaton_injury_count <- wheaton_cluster_join %>%
  group_by(INJ_SEVER_CODE) %>%
  summarise(count=n())
View(wheaton_injury_count)
# 1+2 (not severely injured) = 21
# 3+4 (severely injured) = 20
# 5 (killed) = 1

# Count of roads where wheaton cluster accidents occured. 22 on georgia, 20 on three other roads. 
wheaton_cluster_join_count <- wheaton_cluster_join %>%
  group_by(MAINROAD_NAME) %>%
  summarise(count=n())
  


#Aspen Cluster Injury Analysis
#Filter out just the Aspen people from exp - 23 records
aspen_cluster <- exp %>%
  filter(pre ==91)
#Join it back with main dataset - 23 records, which is two extra
aspen_cluster_join <- inner_join(aspen_cluster, us_and_state_no_bmore, by=c('LONGITUDE', 'LATITUDE'))
#Remove rownames so next step will work
rownames(aspen_cluster_join) <- c()
#Select only distinct records to eliminate duplicates created by joining in cases where more than one person was hit in a single accident - 23 records, correct
aspen_cluster_join <- distinct(aspen_cluster_join)
# Group by injury severity and count
aspen_injury_count <- aspen_cluster_join %>%
  group_by(INJ_SEVER_CODE) %>%
  summarise(count=n())
View(aspen_injury_count)
# 1+2 (not severely injured) = 10
# 3+4 (severely injured) = 13
# 5 (killed) = 0

#August Cluster Injury Analysis
#Filter out just the August people from exp - 7 records
august_cluster <- exp %>%
  filter(pre ==94)
#Join it back with main dataset - 11 records, which is four extra
august_cluster_join <- inner_join(august_cluster, us_and_state_no_bmore, by=c('LONGITUDE', 'LATITUDE'))
#Remove rownames so next step will work
rownames(august_cluster_join) <- c()
#Select only distinct records to eliminate duplicates created by joining in cases where more than one person was hit in a single accident - 7 records, correct
august_cluster_join <- distinct(august_cluster_join)
# Group by injury severity and count
august_injury_count <- august_cluster_join %>%
  group_by(INJ_SEVER_CODE) %>%
  summarise(count=n())
View(august_injury_count)
# 1+2 (not severely injured) = 4
# 3+4 (severely injured) = 3
# 5 (killed) = 0

#Silver Spring Cluster Injury Analysis
#Filter out just the Silver Sprping people from exp - 45 records
ss_cluster <- exp %>%
  filter(pre ==16)
#Join it back with main dataset -45 records
ss_cluster_join <- inner_join(ss_cluster, us_and_state_no_bmore, by=c('LONGITUDE', 'LATITUDE'))
#Remove rownames so next step will work
rownames(ss_cluster_join) <- c()
#Select only distinct records to eliminate duplicates created by joining in cases where more than one person was hit in a single accident - 45 records, correct
ss_cluster_join <- distinct(ss_cluster_join)
# Group by injury severity and count
ss_injury_count <- ss_cluster_join %>%
  group_by(INJ_SEVER_CODE) %>%
  summarise(count=n())
View(ss_injury_count)
# 1+2 (not severely injured) = 19
# 3+4 (severely injured) = 25
# 5 (killed) = 1

#OC Cluster Injury Analysis
#Filter out just the oc people from exp - 83 records
oc_cluster <- exp %>%
  filter(pre ==61 | pre == 62 | pre == 63 | pre == 13) 
#Join it back with main dataset 97 records (too many)
oc_cluster_join <- inner_join(oc_cluster, us_and_state_no_bmore, by=c('LONGITUDE', 'LATITUDE'))
#Remove rownames so next step will work
rownames(oc_cluster_join) <- c()
#Select only distinct records to eliminate duplicates created by joining in cases where more than one person was hit in a single accident -83 records, correct
oc_cluster_join <- distinct(oc_cluster_join)
# Group by injury severity and count
oc_injury_count <- oc_cluster_join %>%
  group_by(INJ_SEVER_CODE) %>%
  summarise(count=n())
View(oc_injury_count)
# 1+2 (not severely injured) = 22
# 3+4 (severely injured) = 59
# 5 (killed) = 2

#Branch Cluster Injury Analysis
#Filter out just the branch people from exp -24 records
branch_cluster <- exp %>%
  filter(pre ==65) 
#Join it back with main dataset 28 records (too many)
branch_cluster_join <- inner_join(branch_cluster, us_and_state_no_bmore, by=c('LONGITUDE', 'LATITUDE'))
#Remove rownames so next step will work
rownames(branch_cluster_join) <- c()
#Select only distinct records to eliminate duplicates created by joining in cases where more than one person was hit in a single accident -24 records, correct
branch_cluster_join <- distinct(branch_cluster_join)
# Group by injury severity and count
branch_injury_count <- branch_cluster_join %>%
  group_by(INJ_SEVER_CODE) %>%
  summarise(count=n())
View(branch_injury_count)
# 1+2 (not severely injured) = 7
# 3+4 (severely injured) = 15
# 5 (killed) = 2


BRANCH is 65
#Small cluster
oc_cluster <- exp %>%
  filter(pre ==61 | pre == 62 | pre == 63 | pre == 13) 


oc_count <- exp %>%
  filter(pre == 61 | pre == 62 | pre == 63 | pre == 13) %>%
  group_by(pre) %>%
  summarise(count=n())
oc_count_total <- oc_count %>%
  summarise(sum=sum(count))
View(oc_count_total)
