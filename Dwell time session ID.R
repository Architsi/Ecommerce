setwd("C:/R")
library(dplyr)
df1 <- read.csv(file="c:/R/..", header=TRUE, sep=",")

file <- read.csv(file.choose(),colClasses='character',header=FALSE,sep="\t")
file$uid <- as.character(file$uid)
colnames(file) <- c("dt","uid")
library(dplyr)
library(data.table)
library(lubridate)

file$dt <- ymd_hms(file$dt)
file <- data.table(file)
file <- file[order(uid, dt)]

setkey(file,uid,dt)
file$sessID3 <- 0
file[, sessID3:= as.numeric(sessID3)]
file[,sessID3:= cumsum(c(0,as.numeric(diff(dt),units="secs")>30)),by=uid]


file$tdiff <- 0
file[, tdiff := difftime(dt, shift(dt, fill=dt[1L]), units="secs"), by=list(uid)]
file1 <- subset(file,tdiff<30)


file2 <- file1 %>% group_by(uid,sessID3) %>% summarise(session_time = sum(tdiff))
file2 <- subset(file2,session_time>=25)
write.csv(file2,"file2.txt")
