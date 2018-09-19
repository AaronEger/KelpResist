setwd("C:/Users/z5189960/Downloads/")

fish <- read.csv("Reef_Life_Survey_(RLS)#_Global_reef_fish_dataset.csv")

habitat <- read.csv("Reef_Life_Survey_(RLS)#_Habitat_Quadrats.csv")

fish <- fish[fish$SiteLat %in% habitat$SiteLat, ]

fish <- fish[fish$SiteLong %in% habitat$SiteLong, ]

fish <- fish[fish$SiteLat > -35, ]

habitat <- habitat[habitat$SurveyID %in% fish$SurveyID, ]


habitat.algae <- habitat[habitat$MajorCategory == "Algae", ]

habitat.algae.sum <- aggregate(habitat.algae$PercentCover ~
          habitat.algae$SurveyID, FUN = sum)

fish.limited <- fish[fish$SurveyID %in% 
                       habitat.algae.sum$`habitat.algae$SurveyID`,]

fish.limited.sum <- aggregate(fish.limited$Total ~
                                fish.limited$SurveyID, FUN = sum)

plot(log(fish.limited.sum$`fish.limited$Total`) ~
       habitat.algae.sum$`habitat.algae$PercentCover`)

abline(lm(log(fish.limited.sum$`fish.limited$Total`) ~
         habitat.algae.sum$`habitat.algae$PercentCover`))


## GAM

library(mgcv)
library(ggplot2)

gam_y <- gam(y ~ s(x), method = "REML")


gam(fish.limited.sum$`fish.limited$Total` ~
     s(habitat.algae.sum$`habitat.algae$PercentCover`), method = "REML")

zz <- 

ggplot() + geom_point(aes(habitat.algae.sum$`habitat.algae$PercentCover`,
                        
                           fish.limited.sum$`fish.limited$Total`)) +
  
theme_classic() + geom_smooth(method = "gam", formula = fish.limited.sum$`fish.limited$Total` ~
                                 s(habitat.algae.sum$`habitat.algae$PercentCover`))
zz
plot(zz)
