# Written By Tucker Johnson #

library(tidyverse)

setwd("~/Desktop/TREND/R/Data")

demographics.df <- read_csv("Demographic Data.csv")
cleaned.responses <- read.csv("Cleaned Responses.csv")
# Only include cleaned responses
demographics.df <- demographics.df[demographics.df$Response.ID %in% cleaned.responses$Response.ID,]

#### Race ####
percent_white <- (table(demographics.df$Race == 1)/nrow(demographics.df)*100)[[2]]
percent_hispanic <- (table(demographics.df$Race == 2)/nrow(demographics.df)*100)[[2]] 
percent_black <- (table(demographics.df$Race == 3)/nrow(demographics.df)*100)[[2]] 
percent_asain <- (table(demographics.df$Race == 4)/nrow(demographics.df)*100)[[2]] 
percent_native_american <- (table(demographics.df$Race == 5)/nrow(demographics.df)*100)[[2]] 
percent_middle_eastern <- (table(demographics.df$Race == 6)/nrow(demographics.df)*100)[[2]]
percent_hawaiian <- (table(demographics.df$Race == 7)/nrow(demographics.df)*100)[[2]]
percent_other <- (table(demographics.df$Race == 8)/nrow(demographics.df)*100)[[2]]

#### Income ####
under_ten <- (table(demographics.df$Income == 1)/nrow(demographics.df)*100)[[2]]
ten_fifteen <- (table(demographics.df$Income == 2)/nrow(demographics.df)*100)[[2]]
fifteen_twentyfive <- (table(demographics.df$Income == 3)/nrow(demographics.df)*100)[[2]]
twentyfive_thrityfive <- (table(demographics.df$Income == 4)/nrow(demographics.df)*100)[[2]]
thirtyfive_fifty <- (table(demographics.df$Income == 5)/nrow(demographics.df)*100)[[2]]
fifty_seventyfive <- (table(demographics.df$Income == 6)/nrow(demographics.df)*100)[[2]]
seventyfive_onehundred <- (table(demographics.df$Income == 7)/nrow(demographics.df)*100)[[2]]
onehundred_onefifty <- (table(demographics.df$Income == 8)/nrow(demographics.df)*100)[[2]]
onefifty_twohundred <- (table(demographics.df$Income == 9)/nrow(demographics.df)*100)[[2]]
over_twohundred <- (table(demographics.df$Income == 10)/nrow(demographics.df)*100)[[2]]

#### Kids ####
have_kids <- (table(demographics.df$Children == 1)/nrow(demographics.df)*100)[[2]]
dont_have_kids <- (table(demographics.df$Children != 1)/nrow(demographics.df)*100)[[2]]

#### Spending ####
spending.df <- demographics.df[1:43]
spending.df <- data.frame(spending.df)
spending.df.original.copy <- spending.df
spending.df <- apply(spending.df, 2, function(x) as.numeric(as.character(x)))
spending.df[is.na(spending.df)] <- 0


Grocery <- spending.df[,c(2, 9, 16, 23, 30, 37)]
Pharmacy <- spending.df[,c(3, 10, 17, 24, 31, 38)]
Food <- spending.df[,c(4, 11, 18, 25, 32, 39)]
Clothing<- spending.df[,c(5, 12, 19, 26, 33, 40)]
Household.items <- spending.df[,c(6, 13, 20, 27, 34, 41)]
Personal.Services <- spending.df[,c(7, 14, 21, 28, 35, 42)]
Banking <- spending.df[,c(8, 15, 22, 29, 36, 43)]

Response.ID <- spending.df[,1]
Grocery.sum <- apply(Grocery, 1, function(x) sum(x))
Pharmacy.sum <- apply(Pharmacy, 1, function(x) sum(x)) 
Food.sum <- apply(Food, 1, function(x) sum(x)) 
Clothing.sum <- apply(Clothing, 1, function(x) sum(x)) 
Household.items.sum <- apply(Household.items, 1, function(x) sum(x)) 
Personal.Services.sum <- apply(Personal.Services, 1, function(x) sum(x)) 
Banking.sum <- apply(Banking, 1, function(x) sum(x)) 
spending.sum.df <- data.frame(Response.ID, Grocery.sum, Pharmacy.sum, Food.sum, Clothing.sum, Household.items.sum, Personal.Services.sum, Banking.sum)

table(spending.sum.df$Grocery.sum == 0)
