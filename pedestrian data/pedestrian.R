rm(list=ls())

# install dbscan package
install.packages("fpc")

# load library
library(fpc)




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


#remove wrongly sized tables
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




#combine all quarters
crash <- rbind(crash51,crash52,crash53,crash54,crash61,crash62,crash63,crash64,crash71,crash72)
person <- rbind(person51,person52,person53,person54,person61,person62,person63,person64,person71,person72)

# joins crash and person tables
combine <- merge(crash, person, by="REPORT_NO")

#sorts out only pedestrian accidents
pedestrian_large <- subset(combine, PERSON_TYPE=="P")

#create subset for state and us highways
us_roads <- subset(pedestrian_large, ROUTE_TYPE_CODE=="US")
state_roads <- subset(pedestrian_large, ROUTE_TYPE_CODE=="MD")
us_and_stater = rbind(us_roads, state_roads)


#extracts only fields we want
pedestrian <- pedestrian_large[c(1,3,12,13,14,20,21,22,23,27,34,35,36,39,46,56,57)]
us_and_stater = us_and_stater[c(1,3,12,13,14,20,21,22,23,27,34,35,36,39,46,56,57)]
#removes row names
rownames(pedestrian) <- c()
rownames(us_and_stater) <- c()
#write data to csv file
#write.csv(pedestrian, file = "pedestrian.csv")

#write data to csv file
#write.csv(us_and_stater, file = "pedestrian_state_and_us.csv")




# set plots bg-color
par(bg="grey80")

# import your point data
data <-us_and_stater[c(12,13)]

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

exp2 <- subset(exp, pre>83)
exp2 <- subset(exp2, pre<87)
exp3 <- subset(exp, pre<23)
exp3 <- subset(exp3, pre>21)
exp4 = rbind(exp2, exp3)

cluster = merge(us_and_stater, exp4, by= c("LATITUDE", "LONGITUDE"))
#remove duplicate rows
cluster = unique(cluster)
# export
write.table(cluster, "oc.csv", sep=",", row.names = F)


