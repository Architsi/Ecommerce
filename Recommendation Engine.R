#load("recommendation_engine.Rdata")
setwd("C:/R")
load("recommendation_engine.Rdata")
library("tree")
library("RJDBC")
library("readxl")
library("stringr")
library("reshape2")
library("tidyr")
library("dplyr")
library("mclust")
library(igraph)
options("scipen"=999, "digits"=19)

rs <- read.table(file = "awsrs.txt", sep = ",", stringsAsFactors = FALSE)

driver <- JDBC(driverClass = "com.amazon.redshift.jdbc42.Driver", 
               classPath = "C:/R/RedshiftJDBC42-1.2.1.1001.jar", identifier.quote="`")
oldurl <- rs$V1
conn <- dbConnect(driver, oldurl)
rm(rs)

#importing data


#query
query <- paste("select ... ")

base_table <- dbGetQuery(conn,query)

#seperate the products seperated by comma
products_seperated <- data.frame(str_split_fixed(base_table$u5,",",n=5))
products_seperated1 <- cbind(base_table$uid,products_seperated)

products_seperated1 <- rename(products_seperated1, uid = 'base_table$uid')
#melt the products to have uid and products

#making data long
user_product <- melt(products_seperated1,id.vars='uid')
user_product <- user_product[,-2]

#adding quantity for dcast
user_product <- user_product %>% mutate(qty = 1)

#making data wide in order to make a matrix
user_prod_matrix <- dcast(user_product,uid ~  value )

#removing uids and total quantity
user_prod_matrix_prods <- user_prod_matrix[,-c(1,2)]


# uid prod1  prod 2  .......
#item based - cosine similarity

#cosine similarity function
getCosine <- function(x,y) 
{
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}


#empty placeholder
product.similarity <- 
  matrix(NA, nrow=ncol(user_prod_matrix_prods),ncol=ncol(user_prod_matrix_prods),
         dimnames=list(colnames(user_prod_matrix_prods),colnames(user_prod_matrix_prods)))

# Fill in the empty spaces with cosine similarities
# Loop through the columns
for(i in 1:ncol(user_prod_matrix_prods)) {
  for(j in 1:ncol(user_prod_matrix_prods)) {
    product.similarity[i,j] <- getCosine(as.matrix(user_prod_matrix_prods[i]),as.matrix(user_prod_matrix_prods[j]))
  }
} 

# Back to dataframe
product.similarity.df <- as.data.frame(product.similarity)


              
# Get the top 10 neighbours for each
product.neighbours <- matrix(NA, nrow=ncol(product.similarity.df),
                                  ncol=11,dimnames=list(colnames(product.similarity.df)))

# Inserting values
for(i in 1:ncol(user_prod_matrix_prods)) {
  product.neighbours[i,] <- 
    (t(head(n=11,rownames(product.similarity.df[order(product.similarity.df[,i],
                                                      decreasing=TRUE),][i]))))
}


####Cosine value of similarity
# Get the top 10 neighbours for each
product.similarity.top <- matrix(NA, nrow=ncol(product.similarity.df),
                             ncol=11,dimnames=list(colnames(product.similarity.df)))

# Inserting values
for(i in 1:ncol(user_prod_matrix_prods)) {
  product.similarity.top[i,] <- 
    (t(head(n=11,product.similarity.df[order(product.similarity.df[,i],
                                                      decreasing=TRUE),][i])))
}



# final data frame to be used
product.similarity.top.df <- as.data.frame(product.similarity.top)

# product.neighbours.test <- matrix(NA, nrow=ncol(product.similarity.df),
#                                  ncol=11,dimnames=list(colnames(product.similarity.df)))

######test data frame similar to top neighbors###########
product.neighbours.test1 <- as.data.frame(product.neighbours)
# product.neighbours.test.df <- as.data.frame(product.neighbours.test)

######################NAing the products where similarity is zero############
for(i in 1:nrow(product.similarity.top.df)){
  for (j in 1:ncol(product.similarity.top.df)){
    if (product.similarity.top.df[i,j] == 0.00000000){
      product.neighbours.test1[i,j] = NA
    }
  }
}

# Removing duplicate prodcuts and their values
product.neighbours.test1 <- product.neighbours.test1[,-1]
product.similarity.top.df <- product.similarity.top.df[,-1]

#removing products having zero similarity to any of the products
product.neighbours.test2 <- subset(product.neighbours.test1, !is.na(V2))
product.similarity.top.df <- subset(product.similarity.top.df, !(V2 == 0))


#setting median as the threshold

for(i in 1:nrow(product.similarity.top.df)){
  for (j in 1:ncol(product.similarity.top.df)){
    if (product.similarity.top.df[i,j] <= boxplot.stats(round(product.similarity.top.df[,1],2))$stats[[3]]){
      product.neighbours.test2[i,j] = NA
    }
  }
}

product.neighbours.test2 <- subset(product.neighbours.test2, !is.na(V2))
product.similarity.top.threshold.df <- subset(product.similarity.top.df, 
                                              (V2 >= boxplot.stats(round(product.similarity.top.df[,1],2))$stats[[3]]))
