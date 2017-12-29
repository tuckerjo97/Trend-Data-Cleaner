# Written By Tucker Johnson #

library(tidyverse)
library(rgdal)
library(sp)
library(rgeos)
library(reshape2)
  
setwd("~/Desktop/TREND/R/Data")
cleaned.responses <- read.csv("Cleaned Responses.csv")
complete.store.df <- read.csv("Store Dataset Complete List.csv", check.names = FALSE)
flagged.store.df <- read.csv("Flagged Stores Complete List.csv")
  
### Spatial Mapping ###
census.map <- readOGR("tl_2016_17_tract/tl_2016_17_tract.shp", layer = "tl_2016_17_tract")
neighborhoods.map <- readOGR("Boundaries - Neighborhoods/geo_export_bcccb956-56be-4e99-ad43-3c6839b94dc4.shp", layer = "geo_export_bcccb956-56be-4e99-ad43-3c6839b94dc4")
corridor.map <- readOGR("Corridor/Corridor.shp", layer = "Corridor")

### Create FIPS Data Base ###
ppl <- SpatialPoints(cleaned.responses[,c("HomeLon", "HomeLat")], proj4string = CRS(proj4string(census.map)))
census.df <- over(ppl, census.map)
fips.df <- data.frame(cleaned.responses$Response.ID, census.df$GEOID)
colnames(fips.df) <- c("Response.ID", "FIPS")
write.csv(fips.df, file = "Response ID to Fips Data Set.csv", row.names = FALSE)
  
### Create Neighborhood Data Base ###
ppl <- SpatialPoints(cleaned.responses[,c("HomeLon", "HomeLat")], proj4string = CRS(proj4string(neighborhoods.map)))
neighborhoods.temp<- over(ppl, neighborhoods.map)
neighborhoods.df <- data.frame(cleaned.responses$Response.ID, neighborhoods.temp$pri_neigh)
colnames(neighborhoods.df) <- c("Response.ID", "Neighboorhood")
write.csv(neighborhoods.df, file = "Response ID to Neighboorhood Data Set.csv", row.names = FALSE)

### Melting Store Dataframe ###

## Jonathan's version done without for loop ##
# complete.store.df$rownum <- 1:nrow(complete.store.df)
# 
# step1 <- melt(complete.store.df, id = "rownum")
# step1 <- step1 %>% filter(!is.na(value))
# step1$Store <- sapply(strsplit(as.character(step1$variable), split = "_"),
#                       function(x) x[1])
# step1$var <- sapply(strsplit(as.character(step1$variable), split = "_"),
#                     function(x) paste(x[-1], collapse = "_"))
# step1$var[step1$var == ""] <- "Response.ID"
# 
# step2 <- dcast(step1, formula = rownum + Store ~ var, value.var = "value")
# 
# lol <- step2[,c("Response.ID", "Store", "Original_Lon", "Original_Lat", "Updated_Lon", "Updated_Lat")]
  
  
colnames(complete.store.df)[seq(2,length(complete.store.df),5)] <- "Original_Lon"
colnames(complete.store.df)[seq(3,length(complete.store.df),5)] <- "Original_Lat"
colnames(complete.store.df)[seq(4,length(complete.store.df),5)] <- "Updated_Lon"
colnames(complete.store.df)[seq(5,length(complete.store.df),5)] <- "Updated_Lat"
complete.store.df.long <- data.frame()
for(i in seq(1, length(complete.store.df), 5)){
  temp <- complete.store.df[i:(i+4)]
  temp <- melt(temp,id = c(2:5),variable.name = "Store", value.name = "Response.ID", na.rm = TRUE)
  complete.store.df.long <- rbind(complete.store.df.long, temp)
}

complete.store.df.long <- complete.store.df.long[, c("Response.ID", "Store", "Original_Lon", "Original_Lat", "Updated_Lon",  "Updated_Lat" )]

## Change FAILED to original lon/lat ##
complete.store.df.long$Updated_Lon[complete.store.df.long$Updated_Lon == "FAILED"] <- complete.store.df.long$Original_Lon[complete.store.df.long$Updated_Lon == "FAILED"]
complete.store.df.long$Updated_Lat[complete.store.df.long$Updated_Lat == "FAILED"] <- complete.store.df.long$Original_Lat[complete.store.df.long$Updated_Lat == "FAILED"]
complete.store.df.long$Updated_Lon <- as.numeric(complete.store.df.long$Updated_Lon)
complete.store.df.long$Updated_Lat <- as.numeric(complete.store.df.long$Updated_Lat)


store.sp <- SpatialPointsDataFrame(coords = complete.store.df.long[, c("Updated_Lon", "Updated_Lat")], 
                                   data = complete.store.df.long[,c("Response.ID", "Store")],
                                   proj4string = CRS(proj4string(corridor.map)))

### Mapping stores to corridors ###
tempcrs <- proj4string(corridor.map) # Saves CRS so can later upload to carto. make sure to change porj4string when out.of.corridor is declared
corridor.map <- spTransform(corridor.map, CRS("+init=epsg:3347"))
corridor.map <- gBuffer(corridor.map, byid = TRUE, width = 100) #Add buffer to corridors of 100 meteres, or roughly one chicago block
store.sp <- spTransform(store.sp, CRS(proj4string(corridor.map)))

store.over <- over(store.sp, corridor.map)

in.corridor <- SpatialPointsDataFrame(coords = complete.store.df.long[!is.na(store.over$shape_leng), c("Updated_Lon", "Updated_Lat")], 
                                      data = complete.store.df.long[!is.na(store.over$shape_leng),c("Response.ID", "Store")],
                                      proj4string = CRS(proj4string(corridor.map)))
out.of.corridor <- SpatialPointsDataFrame(coords = complete.store.df.long[is.na(store.over$shape_leng), c("Updated_Lon", "Updated_Lat")], 
                                         data = complete.store.df.long[is.na(store.over$shape_leng),c("Response.ID", "Store")],
                                         proj4string = CRS(tempcrs))

store.corridor.df <- complete.store.df.long
store.corridor.df$corridor.id <- store.over$corridorid

write.csv(store.corridor.df, file = "Store to Corridor.csv", row.names = FALSE)
  
home.address.df.first <- SpatialPointsDataFrame(coords = cleaned.responses[1:(floor(nrow(cleaned.responses)/2)), c("HomeLon", "HomeLat")], 
                                          data = as.data.frame(cleaned.responses[1:(floor(nrow(cleaned.responses)/2)),c("Response.ID")]),
                                          proj4string = CRS(tempcrs))
home.address.df.second <- SpatialPointsDataFrame(coords = cleaned.responses[((floor(nrow(cleaned.responses)/2))+1):nrow(cleaned.responses), c("HomeLon", "HomeLat")], 
                                                data = as.data.frame(cleaned.responses[((floor(nrow(cleaned.responses)/2))+1):nrow(cleaned.responses),c("Response.ID")]),
                                                proj4string = CRS(tempcrs))

# Creates shapefile to be uploaded into Carto. 
writeOGR(out.of.corridor,dsn = "Stores not in a Corridor", layer = "Stores not in a Corridor", driver="ESRI Shapefile", overwrite_layer = TRUE) # Shapefile of stores not in a corridor
writeOGR(home.address.df.first, dsn = "Respondent Home Addresses first half", layer = "Respondent Home Addresses first half", driver="ESRI Shapefile", overwrite_layer = TRUE) # Shapefile of 1st half of respondent's home location
writeOGR(home.address.df.second, dsn = "Respondent Home Addresses second half", layer = "Respondent Home Addresses second half", driver="ESRI Shapefile", overwrite_layer = TRUE) # Shapefile 2nd half of respondent's home location

  
  
