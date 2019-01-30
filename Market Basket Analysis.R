#Postgres
setwd("C:/R")
library("RJDBC")
library("readxl")
library("arules")
library("stringr")
library("reshape2")
library("tidyr")
library("dplyr")
library("mclust")
library(arulesViz)
library(igraph)
library("data.table")
library("reshape2")
options("scipen"=999, "digits"=19)

rs <- read.table(file = "awsrs.txt", sep = ",", stringsAsFactors = FALSE)
driver <- JDBC(driverClass = "..", 
               classPath = "C:/R/..", identifier.quote="`")
oldurl <- rs$V1
conn <- dbConnect(driver, oldurl)
rm(rs)

#advanced pixel data
from <- '2018-01-07'
to <- Sys.Date()

#query
query <- paste("select * from Archit_client_converts_retain where dt>='",from,"'and dt<='",
               to,"'")

client_base <- unique(dbGetQuery(conn,query))

# #mapping
# mapping1 <- client_base[,c(4,5)]
# sku1 <- str_split_fixed(string=mapping1$u2,pattern = ",",n=10)
# sku_prod <- str_split_fixed(string=mapping1$u5,pattern = ",",n=10)
# 
# sku_df <- data.frame(sku1)
# sku_df$id <- row_number(sku_df$X1)
# sku_melted <- melt(sku_df,id.vars = "id")
# sku_melted <- filter(sku_melted,!(value==""))
# 
# 
# sku_prod_df <- data.frame(sku_prod)
# sku_prod_df$id <- row_number(sku_prod_df$X1)
# sku_prod_melted <- melt(sku_prod_df,id.vars = "id")
# sku_prod_melted <- filter(sku_prod_melted,!(value==""))
# 
# mapping_new <- data.frame(cbind(sku_melted$value,sku_prod_melted$value))
#number of products

#Trimming
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


k <- max(str_count(client_base$u7,","))
#seperating products
client_products_sep <- str_split_fixed(string = client_base$u7, 
                                       pattern = ",", n = k) 
#converting into dataframe
client_products_df <- as.data.frame(trim(client_products_sep))


# Adding Transaction ID
client_products_df <- client_products_df %>% 
  mutate(transaction_id = row_number())

#converting wide to long format
client_products_final <- melt(client_products_df, 
                              variable.name = "Key", value.name = "Product", 
                              id.vars = "transaction_id")

client_products_final <- melt(kk, 
                             variable.name = "Key", value.name = "kws", 
                             id.vars = "conc")

# Filter for rows blank entries
client_products_final <- client_products_final %>% 
  filter(Product != "")

# De-duping for order
client_products_final <- client_products_final %>% 
  distinct(transaction_id, Product)

#converting to factors for transaction
client_products_final$transaction_id <- as.factor(client_products_final$transaction_id)
client_products_final$Product <- as.factor(client_products_final$Product)

#save workspace
# save.image("Market Basket Analysis.Rdata")
# load("Market Basket Analysis.Rdata")

#cleaning DFs from environment
rm(DFS_products_df)

#Sample splitting
client_products_final_basketsplit <- split(client_products_final$Product, 
                                          client_products_final$transaction_id)

client_txn <- as(client_products_final_basketsplit,"transactions")

basket_rules <- apriori(client_txn, parameter = list(sup = 0.002, conf = 0.1, target="rules",
                                                    minlen=2))

#checking freqplot
itemFrequencyPlot(client_txn, topN = 5)

#writing to csv
x<-data.frame(inspect(basket_rules))
write.csv(x,"client2.csv")

#visualization

#plot1
y <- plot(basket_rules,method="graph",interactive=TRUE,shading=NA)

#Plot2
plot(basket_rules, method = "graph", measure = "lift",control = NULL)

#Plot3

subrules2 <- head(sort(basket_rules, by="lift"), 10)
plot(subrules2, method="graph")