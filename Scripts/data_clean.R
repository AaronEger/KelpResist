# Import data

bruv.site.data <- read.csv("C:/Users/z5189960/Documents/GitRepos/KelpResist/bruv_site_data.csv")

fish.trait.temp <- read.csv("C:/Users/z5189960/Documents/GitRepos/KelpResist/shannen_fish_trait.csv")

fish.trait.data <- cbind(as.character(fish.trait.temp$Species), 
                         as.character(fish.trait.temp$Ecoregion.2..trop.sub.vs.sub.temp.))

fish.trait.data <- as.data.frame(fish.trait.data)



# trim fish traits to only include those that are tropical


fish.trop <- fish.trait.data[(fish.trait.data$V2) == "trop", ]

# Create list of names of tropical fish

fish.trop.list <- unique(fish.trop$V1)



### Sum the total number of fish found in each survey

bruv.site.data[, ncol(bruv.site.data) + 1] <- rowSums(bruv.site.data[, 31:ncol(bruv.site.data)])

colnames(bruv.site.data)[ncol(bruv.site.data)] <- c("fishsum")

# Get the proportion of abundance for each fish during each survey

fish.prop <- as.data.frame(matrix(nrow = nrow(bruv.site.data), ncol = ncol(bruv.site.data) - 40))

for (i in 1:nrow(bruv.site.data)) {
  
  for (j in 31:(ncol(bruv.site.data) - 1)) {
    
    fish.prop[i, j] <- bruv.site.data[i, j]/ bruv.site.data[i, 159]

      }
}

# drop NA rows 

fish.prop <- fish.prop[, 31:ncol(fish.prop)]

# rename col and row names

colnames(fish.prop) <- colnames(bruv.site.data[, 31:(ncol(bruv.site.data) - 1)])

rownames(fish.prop) <- bruv.site.data$unique.ID_Hamish

### Select proportion data so that it only includes tropical species

fish.prop.trop <- fish.prop[, colnames(fish.prop[, 31:ncol(fish.prop)]) %in% fish.trop.list]


# Sum proportion of herbivorious fish at each site

fish.prop.trop[, (ncol(fish.prop.trop) + 1)] <- rowSums(fish.prop.trop)

colnames(fish.prop.trop)[ncol(fish.prop.trop)] <- "prop_trop"

### reattach prop trop to bruv master dataset

bruv.site.data$prop_trop <- fish.prop.trop$prop_trop

# Sum total number of herbivorous fish


fish.trop.sum <- bruv.site.data[, colnames(bruv.site.data[, 31:ncol(bruv.site.data)]) %in% fish.trop.list]

fish.trop.sum$fishsum <- rowSums(fish.trop.sum[, 10: (ncol(fish.trop.sum) - 1)])

## Reattach prop trop to bruv master dataset

bruv.site.data$sum_trop <- fish.trop.sum$fishsum


## run a brt to see if any of the exisitng factors predict the % herb

library(gbm)
library(dismo)

# Subset data to only include predictor variables of interest

bruv.sub <- data.frame(bruv.site.data$KelpEverPresent, bruv.site.data$TempAvSite_BRUVYear, 

            bruv.site.data$TempAvSite_CalYear, bruv.site.data$ChlAAvSite_BRUVYear, bruv.site.data$ChlAAvSite_CalYear, 
                
            bruv.site.data$NumOccurencesOrbVeloc2_CalYear, bruv.site.data$Protection.at.Aug.2002, 
            
            bruv.site.data$DistanceFromShore.m., bruv.site.data$Depth_m, bruv.site.data$StipeCount,
            
            bruv.site.data$SppRichness, bruv.site.data$Evenness, bruv.site.data$PercentGrazed,
            
            bruv.site.data$prop_trop, bruv.site.data$sum_trop)
                  
# Rename

colnames(bruv.sub) <- c("KelpEverPresent", "TempAvSite_BRUVYear", "TempAvSite_CalYear", "ChlAAvSite_BRUVYear",
                        "ChlAAvSite_CalYear", "NumOccurencesOrbVeloc2_CalYear", "Protection.at.Aug.2002",
                        "DistanceFromShore.m.", "Depth_m", "StipeCount", "SppRichness", "Evenness",
                        "PercentGrazed", "prop_trop", "sum_trop")

sub.variables  <- c("KelpEverPresent", "TempAvSite_BRUVYear", "TempAvSite_CalYear", "ChlAAvSite_BRUVYear",
                                         "ChlAAvSite_CalYear", "Protection.at.Aug.2002",
                                         "DistanceFromShore.m.", "Depth_m", "StipeCount", "SppRichness", "Evenness",
                                         "PercentGrazed", "SiteName", "SiteCode", "prop_trop", "sum_trop")

rownames(bruv.sub) <- rownames(bruv.site.data)

# Reclassify

#bruv.sub$PercentGrazed <- as.factor(bruv.sub$PercentGrazed)

#brt.prop.trop <- gbm.step(data = bruv.sub, gbm.x = 1:13, gbm.y = 14, family = "laplace")



## Quick look if the conditions in 2002 predict those in 2005

bruv.site.data.2002 <- bruv.site.data[bruv.site.data$Year == 2002, ]

bruv.site.data.2005 <-  bruv.site.data[bruv.site.data$Year == 2004, ]

bruv.site.data.2002.sub <- bruv.site.data.2002[, colnames(bruv.site.data.2002) %in% sub.variables]

bruv.site.data.2002.sub$prop_trop_2005 <- bruv.site.data.2005$prop_trop


#brt.prop.trop <- gbm.step(data = bruv.site.data.2002.sub, gbm.x = 1:13, gbm.y = 15, family = "laplace")

zz <- glm(prop_trop_2005 ~ KelpEverPresent + TempAvSite_BRUVYear + TempAvSite_CalYear + ChlAAvSite_BRUVYear +
          ChlAAvSite_CalYear + Protection.at.Aug.2002 +
          DistanceFromShore.m. + Depth_m + StipeCount + SppRichness + Evenness,
          data = bruv.site.data.2002.sub)

summary(zz)


#### See about some time series

date <- dmy(bruv.site.data$Time)

days.since <- data.frame()

for(i in 1:length(date)) {
  
  days.since[i, 1] <- date[i] - as.Date(min(date))

}


## Add total days back to bruv data set

bruv.site.data <- cbind(bruv.site.data, days.since$V1)

colnames(bruv.site.data)[ncol(bruv.site.data)] <- c("days_since")

# Sort by day of sample

bruv.site.data <- bruv.site.data[order(bruv.site.data$days_since), ]

# Create Site_Year combo

bruv.site.data$Site_year <- paste(bruv.site.data$SiteName, bruv.site.data$Year, sep = "")

# Aggregate day from the replicates


bruv.site.avg <- ddply(bruv.site.data, .(Site_year), summarise, prop_trop_avg = mean(prop_trop), sum_trop_avg = mean(sum_trop),
      
      temp_avg = mean(TempAvSite_BRUVYear), chla_avg = mean(ChlAAvSite_CalYear), 
      
      waves_avg = mean(NumOccurencesOrbVeloc2_CalYear), depth_avg = mean(Depth_m), 
      
      distance_shore_avg = mean(DistanceFromShore.m.), spp_rich_avg = mean(SppRichness), even_avg = mean(Evenness),
      
      days_since_max = max(days_since))

# Sort by day of sample

bruv.site.avg <- bruv.site.avg[order(bruv.site.avg$days_since_max), ]


########### break apart data into sites

site.names <- unique(bruv.site.data$SiteName)

site.names <- sort(site.names)



site.list <- list()

 for (i in 1:length(site.names)){
   
   site.list[[i]] <- bruv.site.data[bruv.site.data$SiteName %in% site.names[i], ]
 }



# Name list

names(site.list) <- site.names


#### Cross correlation and lagged regression

## For Sum Tropical Species

# Temperature

ccf(bruv.site.avg$temp_avg, bruv.site.avg$sum_trop_avg)

# Chl A

ccf(bruv.site.avg$chla_avg, bruv.site.avg$sum_trop_avg)

# Distance from shore


ccf(bruv.site.avg$distance_shore_avg, bruv.site.avg$sum_trop_avg)

# Waves

ccf(bruv.site.avg$waves_avg, bruv.site.avg$sum_trop_avg)

# species richness


ccf(bruv.site.avg$spp_rich_avg, bruv.site.avg$sum_trop_avg)

# species even


ccf(bruv.site.avg$even_avg, bruv.site.avg$sum_trop_avg)

# Depth

ccf(bruv.site.avg$depth_avg, bruv.site.avg$sum_trop_avg)


# For proportion trop species


# Temperature

ccf(bruv.site.avg$temp_avg, bruv.site.avg$prop_trop_avg)

# Chl A

ccf(bruv.site.avg$chla_avg, bruv.site.avg$prop_trop_avg)

# Distance from shore


ccf(bruv.site.avg$distance_shore_avg, bruv.site.avg$prop_trop_avg)

# Waves

ccf(bruv.site.avg$waves_avg, bruv.site.avg$prop_trop_avg)

# species richness


ccf(bruv.site.avg$spp_rich_avg, bruv.site.avg$prop_trop_avg)

# species even


ccf(bruv.site.avg$even_avg, bruv.site.avg$prop_trop_avg)

# Depth

ccf(bruv.site.avg$depth_avg, bruv.site.avg$prop_trop_avg)

