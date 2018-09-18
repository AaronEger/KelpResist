# Import data

bruv.site.data <- read.csv("C:/Users/z5189960/Documents/GitRepos/KelpResist/bruv_site_data.csv")

fish.trait.data <- read.csv("C:/Users/z5189960/Documents/GitRepos/KelpResist/fish_trait_data.csv")



# trim fish traits to only include those that are tropical

a<-dat[,(dat[1,]) == 1]

fish.trop <- fish.trait.data[, (fish.trait.data[1, ]) == "trop" ]

# Create list of names of tropical fish

fish.trop.list <- colnames(fish.trop)




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



## run a brt to see if any of the exisitng factors predict the % herb

library(gbm)
library(dismo)

# Subset data to only include predictor variables of interest

bruv.sub <- data.frame(bruv.site.data$KelpEverPresent, bruv.site.data$TempAvSite_BRUVYear, 

            bruv.site.data$TempAvSite_CalYear, bruv.site.data$ChlAAvSite_BRUVYear, bruv.site.data$ChlAAvSite_CalYear, 
                
            bruv.site.data$NumOccurencesOrbVeloc2_CalYear, bruv.site.data$Protection.at.Aug.2002, 
            
            bruv.site.data$DistanceFromShore.m., bruv.site.data$Depth_m, bruv.site.data$StipeCount,
            
            bruv.site.data$SppRichness, bruv.site.data$Evenness, bruv.site.data$PercentGrazed,
            
            bruv.site.data$prop_trop)
                  
# Rename

colnames(bruv.sub) <- c("KelpEverPresent", "TempAvSite_BRUVYear", "TempAvSite_CalYear", "ChlAAvSite_BRUVYear",
                        "ChlAAvSite_CalYear", "NumOccurencesOrbVeloc2_CalYear", "Protection.at.Aug.2002",
                        "DistanceFromShore.m.", "Depth_m", "StipeCount", "SppRichness", "Evenness",
                        "PercentGrazed", "prop_trop")

sub.variables  <- c("KelpEverPresent", "TempAvSite_BRUVYear", "TempAvSite_CalYear", "ChlAAvSite_BRUVYear",
                                         "ChlAAvSite_CalYear", "Protection.at.Aug.2002",
                                         "DistanceFromShore.m.", "Depth_m", "StipeCount", "SppRichness", "Evenness",
                                         "PercentGrazed", "SiteName", "SiteCode", "prop_trop")

rownames(bruv.sub) <- rownames(bruv.site.data)

# Reclassify

bruv.sub$PercentGrazed <- as.factor(bruv.sub$PercentGrazed)

brt.prop.trop <- gbm.step(data = bruv.sub, gbm.x = 1:13, gbm.y = 14, family = "laplace")



## Quick look if the conditions in 2002 predict those in 2005

bruv.site.data.2002 <- bruv.site.data[bruv.site.data$Year == 2008, ]

bruv.site.data.2005 <-  bruv.site.data[bruv.site.data$Year == 2011, ]

bruv.site.data.2002.sub <- bruv.site.data.2002[, colnames(bruv.site.data.2002) %in% sub.variables]

bruv.site.data.2002.sub$prop_trop_2005 <- bruv.site.data.2005$prop_trop


#brt.prop.trop <- gbm.step(data = bruv.site.data.2002.sub, gbm.x = 1:13, gbm.y = 15, family = "laplace")

zz <- glm(prop_trop_2005 ~ KelpEverPresent + TempAvSite_BRUVYear + TempAvSite_CalYear + ChlAAvSite_BRUVYear +
          ChlAAvSite_CalYear + Protection.at.Aug.2002 +
          DistanceFromShore.m. + Depth_m + StipeCount + SppRichness + Evenness,
          data = bruv.site.data.2002.sub, family = quasibinomial)

summary(zz)
