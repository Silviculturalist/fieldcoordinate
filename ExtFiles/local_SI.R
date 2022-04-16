tree_coords <- read.csv(file = "~/Desktop/2007_2019_Krycklan_SurveyData/Calibration_check/trees_coords.csv",sep=";")
tree_coords <- tree_coords %>% mutate(standtype=stringr::str_extract(string=tree_coords$StandId,pattern="^Circular\\d+"))
tree_coords <- tree_coords %>% mutate(Year=stringr::str_extract(string=tree_coords$StandId,pattern="20\\d+"))
tree_coords <- tree_coords %>% mutate(standnumber=case_when(stringr::str_count(tree_coords$StandId,pattern="_\\d+")>2 ~ stringr::str_extract(string = tree_coords$StandId,pattern="\\d+_\\d+$"),
                                                              TRUE ~ stringr::str_extract(string=tree_coords$StandId,pattern="\\d+$")))
tree_coords <- tree_coords %>% mutate(standnr2=str_extract(tree_coords$standnumber,"_\\d+$"))
tree_coords <- tree_coords %>% mutate(standnr2=str_extract(tree_coords$standnr2,"\\d+$"))
tree_coords <- tree_coords %>% mutate(standnumber=str_extract(tree_coords$standnumber,"^\\d+(?=[:punct:])|^\\d+$"))


pine <- tree_coords %>% filter(!is.na(Age),Species==10,!is.na(Height),Age<400)
#remove tree with age 738 and height 212.

spruce <- tree_coords %>% filter(!is.na(Age),Species==20,!is.na(Height))

library(tidyverse)
ggplot(rbind(pine,spruce),aes(x=Age,y=Height/10))+geom_point()+facet_wrap("Species")


#Chapman-Richards estimate,
#Sharma 2011 GADA


h1 = h0[(1-exp(-b1*age2))/(1-exp(-b1*age))]^(b2)
