setwd("C:/R")
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
library("rgl")
library("car")
library(boot)
library(broom)
library(factoextra)
options("scipen"=999, "digits"=19)

#setting connection
rs <- read.table(file = "awsrs.txt", sep = ",", stringsAsFactors = FALSE)

driver <- JDBC(driverClass = "com.amazon.redshift.jdbc42.Driver", 
               classPath = "C:/R/RedshiftJDBC42-1.2.1.1001.jar", identifier.quote="`")
oldurl <- rs$V1
conn <- dbConnect(driver, oldurl)
rm(rs)

###################### Recency ########################

rfm_table <- dbGetQuery(conn,"select * from ARchit_al_bookers_retain")
# timestamp modification
rfm_table$dt <- ymd_hms(rfm_table$dt)
#ordering
rfm_table <- rfm_table[order(rfm_table$dt),]
#recency calculation
rfm_table <- rfm_table %>% group_by(uid) %>% mutate(rec1= round(((dt) - lag(dt))))
rfm_table$rec1 <- round(rfm_table$rec1/60)
#removing 0s
rfm_table <- data.table(filter(rfm_table, uid!=0))
#recency and uid
recency_table1 <- rfm_table[,(dt=max(dt)), by="uid"]
#join with rfm_table
recency_table2 <- merge(recency_table1,rfm_table,by.x = c("uid","V1"),
                        by.y = c("uid","dt") ,all.x = TRUE)
recency_table2 <- recency_table2[,-c(2:9)]
rm(recency_table1)
rm(rfm_table)

#final recency table
recency_table2$rec1[is.na(recency_table2$rec1)] <- 0


#################### Frequency and Monetary ##############################

rfm_base <- dbGetQuery(conn,"select * from Archit_AL_seg_retain")

#outlier detection
monetary_outliers <- boxplot.stats(rfm_base$monetory)$out
frequency_outliers <- boxplot.stats(rfm_base$frequency)$out
#Removing outliers
i=0
for (i in 1:length(monetary_outliers)){
  rfm_base<- filter(rfm_base,rfm_base$monetory != monetary_outliers[i])
}
i=0
for (i in 1:length(frequency_outliers)){
  rfm_base<- filter(rfm_base,rfm_base$frequency != frequency_outliers[i])
}

#UIDs having frequeency less than 7 

rfm_base <- filter(rfm_base,rfm_base$frequency<7)

#mapping R values
rfm_base1 <- merge(rfm_base,recency_table2,all.x=TRUE)
#removing NA
rfm_base1$rec1[is.na(rfm_base1$rec1)] <- 0
rfm_base1$monetory1 <- round(rfm_base1$monetory/rfm_base1$frequency)
#Scaling(Z-scores)
rfm_base2 <- filter(rfm_base1,rec1>0)
cc <- (rfm_base2[,c(2,6)])
cc$rec1=as.numeric(cc$rec1)
cc=scale(cc)
cc[is.na(cc)] <- 0

#Optimum number of Clusters
#fviz_nbclust(cc, kmeans, method = "wss")

set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- cc
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 100 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#Kmeans

km.out <- kmeans(cc,centers = 6,iter.max = 100)
km.out


cluster4[,(x = length(uid)), by="u1"]

#3d plot
colors <- c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1','tan3','black')


scatter3d(x = as.numeric(cc_final$frequency),
          y = as.numeric(cc_final$monetory),
          z = as.numeric(cc_final$rec1),
          groups = as.factor(cc_final$cluster),
          xlab = "Frequency",
          ylab = "Monetary",
          zlab = "Recency",
          surface.col = colors,
          axis.scales = FALSE,
          surface = TRUE, # produces the horizonal planes through the graph at each level of monetary value
          fit = "smooth",
          # ellipsoid = TRUE, # to graph ellipses uses this command and comment out "surface = TRUE"
          grid = TRUE,
          axis.col = c("white", "white", "white"),
          bg.col= "black")


dbWriteTable(conn, "Archit_al_cluster", cc_final)


