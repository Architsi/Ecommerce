library(stringr)
library(dplyr)
library(lubridate)
library(RJDBC)
library(reconstructr)
library(data.table)
library(party)
library(rpart)
library(partykit)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

options("scipen"=999, "digits"=19)

#save.image("AL Prediction master.Rdata")
# load("AL Prediction master.Rdata")

#######Setting up connection with AWS Redshift ###########

rs <- read.table(file = "awsrs.txt", sep = ",", stringsAsFactors = FALSE)
driver <- JDBC(driverClass = "...", 
               classPath = "C:/R/.....", identifier.quote="`")
oldurl <- rs$V1
conn <- dbConnect(driver, oldurl)
rm(rs)
###########################################################


#Data import
visitors <- dbGetQuery(conn,"select * from ARchit_al_visit_retain")
bookers <- dbGetQuery(conn,"select * from ARchit_al_bookers_retain")

#timestamp casting
visitors$ts_visit <- ymd_hms(visitors$dt)
bookers$ts_book <- ymd_hms(bookers$dt)

visitors <- data.table(visitors)
bookers <- data.table(bookers)

#deleting dt
visitors[,"dt" := NULL]
bookers[,"dt" := NULL]

bookers1 <- bookers[,c("uid","ts_book")]
rm(bookers)
#calculating the first visit timestamp for all visitors
visitors$first_dt <- visitors[,(first_dt = min(ts_visit)),by="uid" ]

#left joining visitors with bookers
visitors_new <- visitors[bookers1, on = "uid", ts_book>ts_visit, allow.cartesian = TRUE]

#table from redshift - for engagement length

all_visitors <- dbGetQuery(conn,"select * from archit_al_bookers_new1")

#timestamp modification
all_visitors$first_visit_time <- ymd_hms(all_visitors$first_visit_time)
all_visitors$last_visit_time <- ymd_hms(all_visitors$last_visit_time)


############# Feature Engineering ############

#unique days
all_visitors$eng_length_day <- 
  difftime(all_visitors$last_visit_time,all_visitors$first_visit_time,units = "days")

all_visitors$eng_length_hrs <- 
  difftime(all_visitors$last_visit_time,all_visitors$first_visit_time,units = "hours")

all_visitors <- data.table(all_visitors)
frequency2 <- data.table(frequency2)


visitors_eng_freq <- merge(all_visitors,frequency2,all.x = TRUE)
#Frequency

frequency1 <- dbGetQuery(conn,"select uid,dt from ARchit_al_visit_retain")
frequency1$dt <- ymd_hms(frequency1$dt)


frequency2 <- frequency1 %>% group_by(uid) %>%
  summarise(freq=length(unique(date(dt))))



#Session duration and number of sessions

all_visitors_session1 <- sessionise(frequency1,dt,uid,threshold = 1800)
all_visitors_session1 <- filter(all_visitors_session1, uid!=0)

all_visitors_session1 <- transform(all_visitors_session1,id=as.numeric(factor(all_visitors_session1$session_id)))

# all_visitors_session1 <- all_visitors_session1[,-3]
all_visitors_session1 <- data.table(all_visitors_session1)

#session count
all_visitors_session2 <- all_visitors_session1[,(number_Sessions <- length(unique(id))),by="uid"]

#session_time
all_visitors_session3 <- all_visitors_session1[,list(number_Sessions <- length(unique(id)),
                                                 session_time <- sum(na.omit(time_delta))),by="uid"]

all_visitors_session4 <- transform(all_visitors_session3,kk<-(all_visitors_session3$V2/all_visitors_session3$V1))

#table with sess time and number of sessions
all_visitors_session4 <-
  cbind(all_visitors_session3,all_visitors_session3$V2/all_visitors_session3$V1)

colnames(all_visitors_session4) <- c("uid","number_sessions","session_time","avg_session_time")

predict_final <- merge(all_visitors_session4,visitors_eng_freq,by="uid",all.x = TRUE)
predict_final <- predict_final[,-c(3)]



predict_final1 <- filter(predict_final,is.na(conversion) == FALSE)


################## Decision tree with default settings ################

output.tree <- ctree(
  conversion ~ number_sessions + avg_session_time + as.numeric(eng_length_days) + freq, 
  data = predict_final1)

plot(output.tree)

data.table(predict_final1)[,(dd=length(predict_final1$conversion)),by="conversion"]

# using rpart

fit <- rpart(conversion ~ number_sessions + avg_session_time + as.numeric(eng_length_hrs) + freq
             , data=predict_final1)

printcp(fit)
plot(fit)
plot(as.party(fit))
rpart.plot(fit)
prp(fit)
fancyRpartPlot(fit)
