# Written By Tucker Johnson #

library(rgeos)
library(sp)
library(rgdal)
library(tmap)
library(reshape2)
library(tidyverse)
library(ggmap)

# SET ADDRESS OF WORKING DIRECTORY #
setwd("~/Desktop/TREND/R")

# CLeaning for home addresses #
##### Cleaning to make sure all required questions were answered #####

Responses <- read.csv("Data/Formatted Data.csv")
Valid.Responses <- Responses[!is.na(Responses$NumStores),]
Valid.Responses <- Valid.Responses[Valid.Responses$Store1 != "defaultValue",]
Valid.Responses <- Valid.Responses[!is.na(Valid.Responses$HomeLat) | !is.na(Valid.Responses$HomeLon),]
Invalid.Responses <- Responses[is.na(Responses$NumStores) | (Responses$Store1 == "defaultValue") | is.na(Responses$HomeLat) |is.na(Responses$HomeLon),]

### Making sure Home Address and Store Locations are withen Chicago ###

Chicago.Map <- readOGR("Data/Boundaries - City/geo_export_fa26a062-e76f-426d-a52d-de5bf7e0dd8d.shp", layer = "geo_export_fa26a062-e76f-426d-a52d-de5bf7e0dd8d") 

## Home Addresses ##
Home.Addresses <- Valid.Responses[,c("Response.ID","HomeAddress", "HomeLon", "HomeLat")]
Home.Addresses.copy <- Home.Addresses
coordinates(Home.Addresses) <- ~HomeLon + HomeLat
proj4string(Home.Addresses) <- proj4string(Chicago.Map)
Home.Addresses.df <- SpatialPointsDataFrame(Home.Addresses, as.data.frame(Home.Addresses)[,1:2])
Valid.Addresses <-as.data.frame(over(Home.Addresses.df, Chicago.Map))
Valid.Addresses <- Home.Addresses.copy[!is.na(Valid.Addresses$shape_area),]

## Store Locations ##
Store.Locations <- filter(Valid.Responses, Valid.Responses$Response.ID %in% Valid.Addresses$Response.ID)
Store.Locations <- Store.Locations[, c("Response.ID", "Latitude1", "Longitude1", "Latitude2", "Longitude2"
                                       , "Latitude3", "Longitude3", "Latitude4", "Longitude4"
                                       , "Latitude5", "Longitude5", "Latitude6", "Longitude6")]
Store.Locations.Copy <- Store.Locations
store.lons <- Store.Locations[, c("Response.ID", "Longitude1", "Longitude2", "Longitude3", "Longitude4", "Longitude5", "Longitude6")]
store.lons <- melt(store.lons, id.vars = "Response.ID",variable.name = "Lonstore", value.name = "Longitude", na.rm = TRUE)
store.lons <- store.lons[with(store.lons, order(Response.ID, Lonstore)),]
store.lats <- Store.Locations[, c("Response.ID", "Latitude1", "Latitude2", "Latitude3", "Latitude4", "Latitude5", "Latitude6")]
store.lats <- melt(store.lats, id.vars = "Response.ID", variable.name = "Latstore", value.name = "Latitude", na.rm = TRUE)
store.lats <- store.lats[with(store.lats, order(Response.ID, Latstore)),]
Store.Locations.Long <- store.lats[,c("Response.ID","Latstore", "Latitude")]
Store.Locations.Long$Lonstore <- store.lons[,c("Lonstore")]
Store.Locations.Long$Longitude <- store.lons[,c("Longitude")]
Store.Locations.Long.Copy <- Store.Locations.Long

# Checks if stores are outside of square around Chicago #
## bottom left corner of square: lat 41.233900, lon -88.517024 ##
Store.Locations.Long.Invalids <- Store.Locations.Long[Store.Locations.Long$Longitude < -88.517024 | Store.Locations.Long$Longitude > -86.740040 | Store.Locations.Long$Latitude < 41.233900 | Store.Locations.Long$Latitude > 42.192003,] 
Store.Locations.Long <- Store.Locations.Long[!(Store.Locations.Long$Longitude < -88.517024 | Store.Locations.Long$Longitude > -86.740040 | Store.Locations.Long$Latitude < 41.233900 | Store.Locations.Long$Latitude > 42.192003),]

Store.Lats.Long <- Store.Locations.Long[,c("Response.ID","Latstore", "Latitude")]
Store.Lats.Wide <- dcast(Store.Lats.Long, Response.ID ~ Latstore, value.var = "Latitude")
Store.Lons.Long <- Store.Locations.Long[,c("Response.ID","Lonstore", "Longitude")]
Store.Lons.Wide <- dcast(Store.Lons.Long, Response.ID ~ Lonstore, value.var = "Longitude")
Valid.Stores <- cbind(Store.Lats.Wide, Store.Lons.Wide[,-1])

## Final Filtering of Valid.Responses for Invalid Home Addresses and Invalid Store Locations ##
Filtered.Valid.Responses <- Valid.Responses[(Valid.Responses$Response.ID %in% Valid.Stores$Response.ID),]
Filtered.Valid.Responses <- Filtered.Valid.Responses[order(Filtered.Valid.Responses$Response.ID),]
lats <- c("Latitude1", "Latitude2", "Latitude3", "Latitude4", "Latitude5", "Latitude6")
lons <- c("Longitude1", "Longitude2", "Longitude3", "Longitude4", "Longitude5", "Longitude6")
Filtered.Valid.Responses[,lats] <- Valid.Stores[,lats]
Filtered.Valid.Responses[,lons] <- Valid.Stores[,lons]
Filtered.Valid.Responses$SurveyID <- factor(Filtered.Valid.Responses$SurveyID, levels = c("Qsample","CRN"))

### Filteres Out Store Names that were invalid ###
for(i in 1:6){
  for(x in 1:nrow(Filtered.Valid.Responses)){
    if(is.na(Filtered.Valid.Responses[x, paste0("Latitude",i)])){
      Filtered.Valid.Responses[x,paste0("StoreName",i)] <- NA
    }
  }
}
for(i in 1:6){
  for(x in 1:nrow(Filtered.Valid.Responses)){
    if(is.na(Filtered.Valid.Responses[x, paste0("Latitude",i)])){
      Filtered.Valid.Responses[x,paste0("Address",i)] <- NA
    }
  }
}


write.csv(Filtered.Valid.Responses, file = "Output/Cleaned Responses.csv", na ="", row.names = FALSE)
write.csv(Filtered.Valid.Responses, file = "Data/Cleaned Responses.csv", na ="", row.names = FALSE)



###### Creating store long data frame #####
Store.df <- Filtered.Valid.Responses[, c("Response.ID", "StoreName1", "StoreName2", "StoreName3", "StoreName4", "StoreName5", "StoreName6")]
Store.df.Copy <- Store.df
Store.df <- melt(Store.df,id.vars = "Response.ID", na.rm = TRUE, value.name = "Store_Name")
Store.df <- Store.df[Store.df$Store != "",]

###### Cleaning for Unique Store Names ######
Store.df$Store_Name <- gsub(' & ', ' And ', Store.df$Store_Name)
Store.df$Store_Name <- gsub(' and ', ' And ', Store.df$Store_Name)
Store.df$Store_Name <- gsub("_\x84\x8e", "", Store.df$Store_Name)
Store.df$Store_Name <- tolower(Store.df$Store_Name)
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
Store.df$Store_Name <- sapply(Store.df$Store_Name, simpleCap)
Store.df$Store_Name <- sapply(Store.df$Store_Name, trim)
Store.df$Store_Name <- gsub('Abc', 'ABC', Store.df$Store_Name)
Store.df$Store_Name <- gsub('U.s.a.', 'USA', Store.df$Store_Name)
Store.df$Store_Name <- gsub('Bmo ', 'BMO ', Store.df$Store_Name)
Store.df$Store_Name <- gsub('Bmw ', 'BMW ', Store.df$Store_Name)
Store.df$Store_Name <- gsub(' Bmw ', ' BMW ', Store.df$Store_Name)
Store.df$Store_Name <- gsub(' Bmw', ' BMW', Store.df$Store_Name)

# Code block written by Peiyong Yu and Tucker Johnson #
Store.df[Store.df$Store_Name %in% c("Amazon Locker Kielbasa (at 7-eleven)", "7-eleven"),]$Store_Name <- "7-Eleven"
Store.df[Store.df$Store_Name %in% c("Gordon's Ace Hdw-orleans Llc", "Meyers Ace Hardware"),]$Store_Name <- "Ace Hardware"
Store.df[Store.df$Store_Name %in% "Addison Search",]$Store_Name <- 'Addison Group'
Aldi <- c('aldi', 'ALDI', "Aldi's", "Aldi Foods", "Aldi Grocery Store", "aldis", "Aldis", "Aldi 8500 S Holland Rd")
Store.df[Store.df$Store_Name %in% Aldi,]$Store_Name <- "Aldi"
AMC <- c("Amc Dine-in Theatres Block 37", "Amc Ford City 14", "Amc Loews 600 North Michigan 9", "Amc Loews Crestwood 18","Amc River East 21", "Amc Showplace Galewood 14")
Store.df[Store.df$Store_Name %in% AMC,]$Store_Name <- "AMC Movie Theater"
Apple <- c("Apple Computer Inc.", "Apple North Michigan Avenue", "Apple Old Orchard", "Apple Orland Square Mall")
Store.df[Store.df$Store_Name %in% Apple,]$Store_Name <- "Apple Reatail Store"
Store.df[Store.df$Store_Name %in% c("Auto zone", "Autozone"),]$Store_Name <- "Auto Zone"
Store.df[Store.df$Store_Name %in% c("Banana Republic Factory Outlet"),]$Store_Name <- "Banana Republic"
Store.df[Store.df$Store_Name %in% c("Bank Of America", "Bank Of America Building"),]$Store_Name <- "Bank of America"
Store.df[Store.df$Store_Name %in% c("Beggars Pizza"),]$Store_Name <- "Beggar's Pizza"
Store.df[Store.df$Store_Name %in% c("Big Apple Grocery"),]$Store_Name <- "Big Apple Finer Foods"
Store.df[Store.df$Store_Name %in% c("Big Lots #4345"),]$Store_Name <- "Big Lots"
Store.df[Store.df$Store_Name %in% c("Binnys", "Binney's", "Binny's Lincoln Park Tasting Room"),]$Store_Name <- "Binny's Beverage Depot"
Store.df[Store.df$Store_Name %in% c("Bloomingdale's Outlet", "Bloomingdale's Medinah Home"),]$Store_Name <- "Bloomingdale's"
Store.df[Store.df$Store_Name %in% c("BMO Harris Bank Headquarters"),]$Store_Name <- "BMO Harris Bank"
Store.df[Store.df$Store_Name %in% c("Bockwinkel's Streeterville", "Bockwinkels"),]$Store_Name <- "Bockwinkel's"
Store.df[Store.df$Store_Name %in% c("Boost Mobile Store"),]$Store_Name <- "Boost Mobile"
Store.df[Store.df$Store_Name %in% c("Bp"),]$Store_Name <- "BP Gas"
Store.df[Store.df$Store_Name %in% c("Butera Market"),]$Store_Name <- "Butera"
Store.df[Store.df$Store_Name %in% c("Caf\x92\xa9 Tolaa"),]$Store_Name <- "Cafe Tola"
Store.df[Store.df$Store_Name %in% c("CARSON PIRIE #532 WOODMAR", "Carson Pirie Scott Furniture Gallery", "Carson Pirie #532 Woodmar"),]$Store_Name <- "Carson Pirie Scott"
Store.df[Store.df$Store_Name %in% c("Cd One Price Dry Cleaner", "C D 1 Cleaners"),]$Store_Name <- "CD One Price Dry Cleaner"
Store.df[Store.df$Store_Name %in% c("Century 12 Evanston/cin\x92\xa9arts 6 & Xd"),]$Store_Name <- "Century Theaters"
Store.df[Store.df$Store_Name %in% c("Cermak Fresh Market", "cermak produce", "Cermak Produce Market", "Cermark Produce", "Cer", "Cermak & Western", "Cermak #8 Bus Stop", "Cermark"),]$Store_Name <- "Cermak Produce"
Store.df[Store.df$Store_Name %in% c("chase bank", "Chase Tower", "Dominicks Chase Branch"),]$Store_Name <- "Chase Bank"
Store.df[Store.df$Store_Name %in% c("Chick-fil-a Loyola Water Tower In-line"),]$Store_Name <- "Chick-Fil-A"
Store.df[Store.df$Store_Name %in% c("Chernins Shoe Zone"),]$Store_Name <- "Chernin's Shoe Zone"
CTA <- c("Cta - Austin", "Cta - Cermak-mccormick Place", "Cta - Grand (blue)", "Cta - Howard", "Cta - Merchandise Mart", "Cta - State/lake", "Cta Bus Stop - Clark & Catalpa", "Cta Bus Stop - Diversey & Rockwell", "Cta Bus Stop - John G. Shedd Aquarium", "Cta Bus Stop 1598", "Cta Bus Stop Lake Park & 55th")
Store.df[Store.df$Store_Name %in% CTA,]$Store_Name <- "Chicago Transit Authority"
Store.df[Store.df$Store_Name %in% c("Costco", "Cosco - Pharmacy", "Costco Hiring Center-south Loop", "Costco Wholesale"),]$Store_Name <- "Costco"
Store.df[Store.df$Store_Name %in% c("Costco Gas", "Costco Gas Station"),]$Store_Name <- "Costco Gasoline"
Store.df[Store.df$Store_Name %in% c("Criket"),]$Store_Name <- "Cricket Wireless"
Store.df[Store.df$Store_Name %in% c("cvs", "Cvs", "CVS", "CVS Pharmacy", "CVS/Pharmacy", "Cvs/pharmacy"),]$Store_Name <- "CVS"
Store.df[Store.df$Store_Name %in% c("Depaul - Arthur J. Schmitt Academic Center (sac)", "Depaul University Loop Campus Bookstore"),]$Store_Name <- "DePaul University"
Store.df[Store.df$Store_Name %in% "Discovery Clothing Co.",]$Store_Name <- "Discovery Clothing"
Store.df[Store.df$Store_Name %in% c("Diversey Place (516-524 W Diversey)", "Diversey, Clark & Broadway"),]$Store_Name <- "Diversey Place"
Store.df[Store.df$Store_Name %in% c("Dominican University Noble Campus", "Dominican University Graduate School Of Library & Information Sc"),]$Store_Name <- "Dominican University"
Dominick <- c("Domincks", "Domnick's", "Dominic's Pharmacy Dept", "Dominick", "Dominick's", "Dominick's Division Headquarters", "Dominick's Division Office", "Dominick's Finer Foods - Grocery- Bridgeview", "Dominick's Finer Foods - Stores- North Northwest- Pharmacy", "Dominick's Finer Foods Division Hq", "Dominick's Finer Foods, Glenview", "Dominicks")
Store.df[Store.df$Store_Name %in% Dominick,]$Store_Name <- "Dominick's"
Store.df[Store.df$Store_Name %in% c("Dsw Designer Shoe Warehouse"),]$Store_Name <- "DSW Designer Shoe Warehouse"
Store.df[Store.df$Store_Name %in% c("Dtlr"),]$Store_Name <- "DTLR"
Store.df[Store.df$Store_Name %in% c("Dunkin Donuts"),]$Store_Name <- "Dunkin' Donuts"
Store.df[Store.df$Store_Name %in% c("Fair Share Supermarket"),]$Store_Name <- "Fair Share Foods"
Store.df[Store.df$Store_Name %in% c("Fairplay", "Fairplay Finer Foods", "Fairplay Food & Liquor"),]$Store_Name <- "Dunkin' Donuts"
Store.df[Store.df$Store_Name %in% c("Fannie May Candies"),]$Store_Name <- "Fannie May"
Store.df[Store.df$Store_Name %in% c("Fashion Outlets Of Chicago Nordstrom Rack"),]$Store_Name <- "Fashion Outlets Of Chicago"
Store.df[Store.df$Store_Name %in% c("Food For Less"),]$Store_Name <- "Food 4 Less"
Store.df[Store.df$Store_Name %in% c("Food For Less Gas"),]$Store_Name <- "Food 4 Less Gas"
Store.df[Store.df$Store_Name %in% c("Footlocker", "Kids Footlocker"),]$Store_Name <- "Foot Locker"
Store.df[Store.df$Store_Name %in% c("Fresh Farms"),]$Store_Name <- "Fresh Farms International Market"
Store.df[Store.df$Store_Name %in% c("Fresh Marketplace", "Fresh Mart"),]$Store_Name <- "Fresh Market"
Store.df[Store.df$Store_Name %in% c("Gap", "Gap Factory", "Gap Outlet", "Gapkids"),]$Store_Name <- "Gap"
Store.df[Store.df$Store_Name %in% c("Garrett Popcorn Shops - Merchandise Mart", "Garrett Popcorn Shops - Navy Pier", "Garrett Popcorn Shops - Water Tower Place"),]$Store_Name <- "Garrett Popcorn Shops"
Store.df[Store.df$Store_Name %in% c("Gfs"),]$Store_Name <- "Gordon Food Service Store"
Store.df[Store.df$Store_Name %in% c("Halsted Street Deli-naperville"),]$Store_Name <- "Halsted Street Deli"
Store.df[Store.df$Store_Name %in% c("Harold's Fried Chicken", "Harolds Chicken"),]$Store_Name <- "Harold's Chicken Shack"
Store.df[Store.df$Store_Name %in% c("Hhgregg"),]$Store_Name <- "H. H. Gregg"
Store.df[Store.df$Store_Name %in% c("Hilton Chicago Athletic Club", "Hilton Chicago Marshall Field Suite", "Hilton Chicago/oak Lawn"),]$Store_Name <- "Hilton Chicago"
Store.df[Store.df$Store_Name %in% c("Hm Palace"),]$Store_Name <- "HM Palace"
Store.df[Store.df$Store_Name %in% c("Hmr Designs"),]$Store_Name <- "HMR Designs"
Store.df[Store.df$Store_Name %in% c("Holiday Inn Chicago Mart Plaza River North"),]$Store_Name <- "Holiday Inn"
Store.df[Store.df$Store_Name %in% c("Hollister"),]$Store_Name <- "Hollister Co."
Store.df[Store.df$Store_Name %in% c("Homegoods"),]$Store_Name <- "Home Goods"
Store.df[Store.df$Store_Name %in% c("Ikea", "Ikea Restaurant"),]$Store_Name <- "IKEA"
Store.df[Store.df$Store_Name %in% c("J.crew"),]$Store_Name <- "J. Crew"
Store.df[Store.df$Store_Name %in% c("Jcpenney"),]$Store_Name <- "J. C. Penny"
Store.df[Store.df$Store_Name %in% c("Jerry's"),]$Store_Name <- "Jerrys Fruit & Garden Center"
Jewel_Osco <- c("Jewal", "Jewel", "Jewel 9", "Jewel Food", "Jewel Food Store", "Jewel Foods", "Jewel Foods Store", "Jewel Osco", "Jewel-clybourn & Fullerton", "Jewel-osco", "Jewel-osco Pharmacy", "Jewel's", "Jewel's Foods", "Jewell", "Jewels", "Jewels Food Store", "Jewels-osco")
Store.df[Store.df$Store_Name %in% Jewel_Osco,]$Store_Name <- "Jewel-Osco"
Store.df[Store.df$Store_Name %in% c("K&g"),]$Store_Name <- "K & G"
Store.df[Store.df$Store_Name %in% c("Kfc", "Kfc - Kentucky Fried Chicken"),]$Store_Name <- "KFC"
Store.df[Store.df$Store_Name %in% c("Kohl's Bucktown", "Kohl's Burbank", "Kohl's Crestwood", "Kohl's Lincolnwood", "Kohls"),]$Store_Name <- "Kohl's"
Store.df[Store.df$Store_Name %in% c("L.l.bean"),]$Store_Name <- "L. L. Bean"
Store.df[Store.df$Store_Name %in% c("Lalo's Restaurant"),]$Store_Name <- "Lalo's Mexican Restaurant"
Store.df[Store.df$Store_Name %in% c("Larrys Barber College"),]$Store_Name <- "Larry's Barber College"
Store.df[Store.df$Store_Name %in% c("Leamington Food"),]$Store_Name <- "Leamington Foods"
Store.df[Store.df$Store_Name %in% c("Levi's Store"),]$Store_Name <- "Levi's"
Store.df[Store.df$Store_Name %in% c("Little Caesars"),]$Store_Name <- "Little Caesars Pizza"
Store.df[Store.df$Store_Name %in% c("Lush"),]$Store_Name <- "Lush Cosmetics"
Store.df[Store.df$Store_Name %in% c("Marshall Field & Co. Building", "Levenger At Macy's", "Macy's Associate Entrance", "Macy's Bridal Salon", "Macy's Marketplace Grill", "Macys"),]$Store_Name <- "Macy's"
Store.df[Store.df$Store_Name %in% c("Mariano Plaza"),]$Store_Name <- "Mariano Park"
Store.df[Store.df$Store_Name %in% c("Mariano", "Mariano's At South Loop", "Mariano's Fresh Market", "Mariano's Store", "Mariano's Western Springs", "Marianos", "Marianos Lakeview", "Marionos", "Marion's"),]$Store_Name <- "Mariano's"
Store.df[Store.df$Store_Name %in% c("Mccormick Place Lakeside Center"),]$Store_Name <- "Mccormick Place"
Store.df[Store.df$Store_Name %in% c("Mc Donalds", "Mcdonalds", "Mcdonald's"),]$Store_Name <- "McDonald's"
Store.df[Store.df$Store_Name %in% c("Menard's"),]$Store_Name <- "Menards"
Metra <- c("Metra - 18th Street", "Metra - 51st / 53rd (hyde Park)", "Metra - 83rd Street (avalon Park)", "Metra - Kensington / 115th Street", "Metra - Ravenswood", "Metra - Western Avenue", "Metra Bnsf Train 1254")
Store.df[Store.df$Store_Name %in% Metra,]$Store_Name <- "Metra"
Store.df[Store.df$Store_Name %in% c("Metro Pcs", "Metropcs"),]$Store_Name <- "MetroPCS"
Store.df[Store.df$Store_Name %in% c("Millercoors"),]$Store_Name <- "MillerCoors"
Store.df[Store.df$Store_Name %in% c("Navy Pier Food Court", "Navy Pier, Inc."),]$Store_Name <- "Navy Pier"
Store.df[Store.df$Store_Name %in% c("Nike Factory Store", "Niketown Chicago"),]$Store_Name <- "Nike"
Nordstrom <- c("Nordstrom Cafe - Mezzaz", "Nordstrom Michigan Avenue", "Nordstrom Oakbrook Center", "Nordstrom Old Orchard Center", "Nordstrom Rack", "Nordstrom Rack Lincoln Park", "Nordstrom Rack Orland Park Place", "Nordstrom Rack Shops At Orchard Place", "Nordstrom Rack South Loop", "Nordstrom Rack The Shops At State & Washington")
Store.df[Store.df$Store_Name %in% Nordstrom,]$Store_Name <- "Nordstrom"
Store.df[Store.df$Store_Name %in% c("Officemax"),]$Store_Name <- "OfficeMax"
Store.df[Store.df$Store_Name %in% c("Old Navy 4131 N Harlem Ave"),]$Store_Name <- "Old Navy"
Store.df[Store.df$Store_Name %in% c("Once Stop Shopping", "One Stop", "One Stop Grocery Store"),]$Store_Name <- "One Stop Foods"
Store.df[Store.df$Store_Name %in% c("Petes Fresh Market","Pete's", "Pete's Fresh Market #12", "Pete's Fresh Marketplace", "Pete's Grocery", "Pete's Market"),]$Store_Name <- "Pete's Fresh Market"
Store.df[Store.df$Store_Name %in% c("Pete's Poduce"),]$Store_Name <- "Pete's Produce"
Store.df[Store.df$Store_Name %in% c("Petsmart"),]$Store_Name <- "PetSmart"
Store.df[Store.df$Store_Name %in% c("Pizza Hut Express"),]$Store_Name <- "Pizza Hut"
Store.df[Store.df$Store_Name %in% c("Pnc Bank", "Pnc Bank (atm)"),]$Store_Name <- "PNC Bank"
Store.df[Store.df$Store_Name %in% c("Pnc Mortgage"),]$Store_Name <- "PNC Mortgage"
Popeyes <- c("Popeye's Chicken", "Popeye's Chicken & Biscuits", "Popeyes", "Popeyes Chicken", "Popeyes Chicken & Biscuits")
Store.df[Store.df$Store_Name %in% Popeyes,]$Store_Name <- "Popeye's"
Store.df[Store.df$Store_Name %in% c("Portillo"),]$Store_Name <- "Portillo's"
Store.df[Store.df$Store_Name %in% c("Pottery Barn Home"),]$Store_Name <- "Pottery Barn"
Store.df[Store.df$Store_Name %in% c("Rainbow Apparel Kids"),]$Store_Name <- "Rainbow Apparel"
Store.df[Store.df$Store_Name %in% c("Reggies"),]$Store_Name <- "Reggie's"
Store.df[Store.df$Store_Name %in% c("Rei"),]$Store_Name <- "REI"
Store.df[Store.df$Store_Name %in% c("Ross"),]$Store_Name <- "Ross Dress For Less"
Store.df[Store.df$Store_Name %in% c("Sally Beauty", "Sally Beauty Supply", "Sally's Beauty"),]$Store_Name <- "Sally's Beauty Supply"
Store.df[Store.df$Store_Name %in% c("Sam's Club - Rolling Meadows"),]$Store_Name <- "Sam's Club"
Store.df[Store.df$Store_Name %in% c("Sam's Club Fuel Center", "Sam's Club Gas Station"),]$Store_Name <- "Sam's Club Gas"
Store.df[Store.df$Store_Name %in% c("Sears Auto Center", "Sears Optical"),]$Store_Name <- "Sears"
Store.df[Store.df$Store_Name %in% c("Shark's Fish & Chicken", "Shark's Fish & Tony's Steak"),]$Store_Name <- "Sharks Fish & Chicken"
Store.df[Store.df$Store_Name %in% c("Sherwin-williams Commercial Paint Store", "Sherwin-williams Paint Store"),]$Store_Name <- "Sherwin-Williams Paint Store"
Store.df[Store.df$Store_Name %in% c("Shop-n-save Market"),]$Store_Name <- "Shop & Save Market"
Store.df[Store.df$Store_Name %in% c("Sonic Drive In"),]$Store_Name <- "Sonic Drive-in"
Store.df[Store.df$Store_Name %in% c("Starbucks @ Dominick's"),]$Store_Name <- "Starbucks"
Store.df[Store.df$Store_Name %in% c("Subway Sandwiches & Salads"),]$Store_Name <- "Subway"
Store.df[Store.df$Store_Name %in% c("Sprint"),]$Store_Name <- "Sprint Store"
Store.df[Store.df$Store_Name %in% c("Sunglass Hut At Macy's"),]$Store_Name <- "Sunglass Hut"
Store.df[Store.df$Store_Name %in% c("T.j. Maxx", "T.j.maxx", "Tj Maxx"),]$Store_Name <- "T.J. Maxx"
Store.df[Store.df$Store_Name %in% c("Citytarget", "Supertarget", "Taget", "Target Hyde Park"),]$Store_Name <- "Target"
Store.df[Store.df$Store_Name %in% c("Tcf", "Tcf Bank", "Tcf Bank - Uic Sce"),]$Store_Name <- "TCF Bank"
Store.df[Store.df$Store_Name %in% c("Tgi Fridays"),]$Store_Name <- "TGI Fridays"
Store.df[Store.df$Store_Name %in% c("The Gwen, A Luxury Collection Hotel, Chicago"),]$Store_Name <- "The Gwen Hotel"
Store.df[Store.df$Store_Name %in% c("Themart", "The Mart Food Court"),]$Store_Name <- "The Mart"
Store.df[Store.df$Store_Name %in% c("Tony's Finer Foods Market", "Tony's Finer Foods", "Tonys Fresh Market", "Super Tony's Finer Foods", "Tonys"),]$Store_Name <- "Tony's Fresh Market"
Store.df[Store.df$Store_Name %in% c("Treasure Island"),]$Store_Name <- "Treasure Island Foods"
Store.df[Store.df$Store_Name %in% c("U.s. Bank Branch", "Us Bank", "Us Bank Dominick's"),]$Store_Name <- "US Bank"
Store.df[Store.df$Store_Name %in% c("Uic Bookstore @ Student Center East", "Uic Skyspace By James Turrell", "Uic-halsted News Stand", "University Of Illinois At Chicago (uic)", "University Of Illinois Mile Square Health Center: Near West Side"),]$Store_Name <- "University Of Illinois At Chicago"
Store.df[Store.df$Store_Name %in% c("United States Post Office", "Us Post Office"),]$Store_Name <- "US Post Office"
Store.df[Store.df$Store_Name %in% c("Victoria Secret", "Victoria's Secret Pink"),]$Store_Name <- "Victoria's Secret"
Store.df[Store.df$Store_Name %in% c("Village Discount Outlet"),]$Store_Name <- "Village Discount"
Store.df[Store.df$Store_Name %in% c("Walgreen", "Walgreen Store", "Walgreen's", "Walgreens 400 N. Michigan Ave"),]$Store_Name <- "Walgreens"
Store.df[Store.df$Store_Name %in% c("Walmart 47th & Drexel", "Walmart Neighborhood Market", "Walmart Supercenter", "Walmart Supercenter 4700 135th St", "Walmarts"),]$Store_Name <- "Walmart"
Store.df[Store.df$Store_Name %in% c("Whole Foods", "Whole Foods Market - Midwest Support Office"),]$Store_Name <- "Whole Foods Market"
Store.df[Store.df$Store_Name %in% c("West Town Bakery"),]$Store_Name <- "West Town Bakery & Diner"
Store.df[Store.df$Store_Name %in% c("Wrigleyville Sports @ Macy's"),]$Store_Name <- "Wrigleyville Sports"
Store.df[Store.df$Store_Name %in% c("Xsport Fitness Express"),]$Store_Name <- "Xsport Fitness"
Store.df[Store.df$Store_Name %in% c("Zipcar 1920 S Halsted"),]$Store_Name <- "Zipcar Jewel At Kinzie/desplaines"
Store.df[Store.df$Store_Name %in% c("Hai Woon Dae\n\n\n\n6240 N. California Ave.\n\n\n\nchicago Il 60659"),]$Store_Name <- "Hai Woon Dae"


###### WARNING: MAC CLEANONG FOR SPECIAL CHARACTERS DOESN"T WORK FOR WINDOMS

### Cleaning Out Non-Stores (Parks, Trails, Railways, ect.) ###
delete <- c("#8 Halsted Bus/southbound To 79th", "Amtrak Train 303 To Bloomington", "Bloomingdale Trail - The 606", "Burlington Park", "Chicago", "Chicago Trolley Co. Stop #1", "Chicago Trolley Co. Stop #3", "Humboldt Park", "Shedd Park", "Virgin America Flight 201")
Store.df <- Store.df[!(Store.df$Store_Name %in% delete),]
Store.df.Long <- as.data.frame(Store.df)

# Create's Trip Dataset, unfiltered for Geographic Inconsistancies #
trip.df <- as.data.frame(dcast(Store.df.Long, Response.ID ~ variable))
write.csv(trip.df, file = "Output/Trips Dataset.csv", na ="", row.names = FALSE)
write.csv(trip.df, file = "Data/Trips Dataset.csv", na ="", row.names = FALSE)

Store.df$variable <- 1:nrow(Store.df)
Store.df.Wide <- as.data.frame(dcast(Store.df, variable ~ Store_Name, value.var = "Response.ID"))
Store.df.Wide <- apply(Store.df.Wide, 2, sort, na.last = TRUE)
Store.df.Wide <- as.data.frame(Store.df.Wide[,-1])


##### Cleaning out Inconsistant Geographic Locations #####

### Functions ###
getlonlat <- function(id, store.num){
  
  lon.location <- paste0("Longitude", substr(store.num, 10, 10))
  lat.location <- paste0("Latitude", substr(store.num, 10, 10))
  
  original.lon <- Filtered.Valid.Responses[Filtered.Valid.Responses$Response.ID == id, lon.location]
  original.lat <- Filtered.Valid.Responses[Filtered.Valid.Responses$Response.ID == id, lat.location]
  
    
  data.frame(original.lon, original.lat)
}

cbindPad <- function(...){
  args <- list(...)
  n <- sapply(args,nrow)
  mx <- max(n)
  pad <- function(x, mx){
    if (nrow(x) < mx){
      nms <- colnames(x)
      padTemp <- matrix(NA, mx - nrow(x), ncol(x))
      colnames(padTemp) <- nms
      if (ncol(x)==0) {
        return(padTemp)
      } else {
        return(rbind(x,padTemp))
      }
    }
    else{
      return(x)
    }
  }
  rs <- lapply(args,pad,mx)
  return(do.call(cbind,rs))
}

invalid.dist <- function(x){
  if(x[5] == "FAILED") return(TRUE)
  x <- as.numeric(x)
  xdist <- abs(x[2]) - abs(x[4])
  xdist <- xdist ** 2
  ydist <- abs(x[3]) - abs(x[5])
  ydist <- ydist ** 2
  
  total.dist <- sqrt(xdist + ydist)
  return(total.dist > 0.015)
}
### Geocoding ###
store.df.final <- data.frame()
flagged.stores <- data.frame() 
range <- 319 ###### SET RANGE HERE
for(i in range){
  store.name <- names(Store.df.Wide)[i]
  df <- Store.df.Long[Store.df.Long$Store_Name == store.name,]
  df <- df[sort.list(df$Response.ID),]
  temp <- (apply(df, 1, function(x){getlonlat(id = x[1], store.num = x[2])}))
  original.lonlat <- data.frame()
  for(x in 1:length(temp)){
    original.lonlat <- rbind(original.lonlat, temp[[x]])
  }
  
  Response.ID <- df$Response.ID
  
  updated.lonlat <- data.frame()
  for(y in 1:nrow(original.lonlat)){
    templonlat <- as.numeric(original.lonlat[y,])
    geopaste <- paste(store.name, "near", revgeocode(templonlat))
    updated.lonlat <- rbind(updated.lonlat, geocode(geopaste))
  }
  
  ### Flagging ###
  updated.lonlat[is.na(updated.lonlat$lon),c(1,2)] <- "FAILED"
  store.df.chunk <- cbind(Response.ID, original.lonlat, updated.lonlat)
  
  flagvector <- apply(store.df.chunk, 1,invalid.dist)
  IDs <- Response.ID[flagvector]
  Store <- rep(store.name, length(IDs))
  tempflag <- cbind(Store, IDs)
  flagged.stores <- rbind(flagged.stores,tempflag)
  
  colnames(store.df.chunk) <- c(store.name, paste0(store.name, "_Original_Lon"),paste0(store.name, "_Original_Lat"),paste0(store.name, "_Updated_Lon"), paste0(store.name, "_Updated_Lat"))
  store.df.final <- cbindPad(store.df.final, store.df.chunk)
}

##### WRITE CSV ###
store.paste <- paste("Store Dataset", range[1], "to", paste0(range[length(range)],".csv"))
flag.paste <- paste("Flagged Stores", range[1], "to", paste0(range[length(range)],".csv"))

setwd("Output")
write.csv(store.df.final, file = store.paste, na ="", row.names = FALSE)
write.csv(flagged.stores, file = flag.paste, na = "", row.names = FALSE)
setwd("..")

print(paste("Start Next Cycle of location checks at", i+1))

##### Run For each range of 2500. Need to Run in Ranges to get around geocodeQueryCheck #####


##### NOTE: for Dominick's and a Ultra Foods, store closed, so a ton of results come back as failed ##### 


# Ranges 
# 1 - 295
# 296 - 465
# 466 - 545
# 546 - 733
# 734 - 855
# 856 - 887
# 888 - 943
