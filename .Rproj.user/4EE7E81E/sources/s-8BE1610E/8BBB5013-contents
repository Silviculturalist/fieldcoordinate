tree_coords <- read.csv(file = "~/Desktop/2007_2019_Krycklan_SurveyData/Calibration_check/trees_coords.csv",sep=";")
tree_coords2 <- tree_coords %>% mutate(standtype=stringr::str_extract(string=tree_coords$StandId,pattern="^Circular\\d+"))
tree_coords2 <- tree_coords2 %>% mutate(Year=stringr::str_extract(string=tree_coords2$StandId,pattern="20\\d+"))
tree_coords2 <- tree_coords2 %>% mutate(standnumber=case_when(stringr::str_count(tree_coords2$StandId,pattern="_\\d+")>2 ~ stringr::str_extract(string = tree_coords2$StandId,pattern="\\d+_\\d+$"),
                                                              TRUE ~ stringr::str_extract(string=tree_coords2$StandId,pattern="\\d+$")))
tree_coords2 <- tree_coords2 %>% mutate(standnr2=str_extract(tree_coords2$standnumber,"_\\d+$"))
tree_coords2 <- tree_coords2 %>% mutate(standnr2=str_extract(tree_coords2$standnr2,"\\d+$"))
tree_coords2 <- tree_coords2 %>% mutate(standnumber=str_extract(tree_coords2$standnumber,"^\\d+(?=[:punct:])|^\\d+$"))

#Only interested in 10 m radius diameter plots.
tree_coords2 <- tree_coords2 %>% filter(standtype=="Circular10")
#Neither of following columns contribute information
tree_coords2 <- tree_coords2 %>% select(-c(AreaLevel2,AreaLevel3,StandId,Plotname))

#Tree species probably govd. by first number.
tree_coords2 <- tree_coords2 %>% mutate(speciesnr = str_extract(Species,pattern = "^\\d"))

#Enforce Species are factors and Diameter numeric
tree_coords2$Species <-as.factor(tree_coords2$Species)
tree_coords2$Diameter <- as.numeric(tree_coords2$Diameter)

#Force trackable ID for all indivduals - different between years.
tree_coords2$guid <- 1:nrow(tree_coords2)

rm(tree_coords)


tree_coords3 <- tree_coords2 %>% rename(id=guid,x=GPSEast,y=GPSNorth)

#Buggy but gets it done.
# t1 <- proc.time()
# tree_coords3 %>% as.coord.data.frame(id = id,x = x,y=y,Diameter,Species,Year,standnumber) %>%  save_transformation(source_year = 2015,target_year = 2019)
# t2 <- proc.time()
# source("~/Desktop/R help functions/self_notify.R")
# self_notify()
# t2[[3]]-t1[[3]]
#
# folders <- list.dirs(path = "matched_trees",recursive=FALSE)
# folderlength <- numeric()
# for(folder in folders){
#   folder_length <- length(dir(folder))
#   folderlength<- c(folderlength,folder_length)
#   }

#coord data frame
tree_coords4 <- tree_coords3 %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year,standnumber)

#Transform corrections.
# 3
corrected_csv <- tree_coords4 %>% filter(standnumber==3,Year==2015) %>% translate_coordinates(x=0.55,y=-0.5) %>% coordinate_rotation(rotation = degree_to_radian(-1)) #%>%  plot(size=Diameter,color=Species)
target_csv <- tree_coords4 %>% filter(standnumber==3,Year==2019) #%>% plot(size=Diameter,color=Species)
write.csv(x = bind_rows(corrected_csv,target_csv),file = "matched_trees/3/3_2015_to_2019_corrected.csv")


# 29
start_csv <- read.csv(file = "matched_trees/29/29_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)
corrected_csv <- start_csv %>% filter(Year==2015) %>%  scale_coordinates(.95,.95)# %>% plot(size=Diameter,color=factor(Species))
target_csv <- start_csv %>% filter(Year==2019) #%>% plot(size=Diameter,color=factor(Species))
write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/29/29_2015_to_2019_corrected.csv")

# 46
start_csv <- read.csv(file = "matched_trees/46/46_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)
corrected_csv <- start_csv %>% filter(Year==2015) %>%  translate_coordinates(x =2.5,y=-3 ) %>% coordinate_rotation(degree_to_radian(-2.5)) #%>% plot(size=Diameter,color=factor(Species))
target_csv <- start_csv %>% filter(Year==2019) #%>% filter_radius() %>% plot(size=Diameter,color=factor(Species))
write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/46/46_2015_to_2019_corrected.csv",row.names = FALSE)

# 53
start_csv <- tree_coords4 %>% filter(standnumber==53) #from scratch.
corrected_csv <- start_csv %>% filter(Year==2015) %>%  coordinate_rotation(degree_to_radian(-16)) %>% scale_coordinates(Sx=1,Sy=1.05) #%>% plot(size=Diameter,color=factor(Species))
target_csv <- start_csv %>% filter(Year==2019) #%>% filter_radius() %>% plot(size=Diameter,color=factor(Species))
write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/53/53_2015_to_2019_corrected.csv",row.names = FALSE)

# 55
start_csv <- read.csv(file = "matched_trees/55/55_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)
corrected_csv <-
  start_csv %>%
  filter(Year==2015) %>%
  scale_coordinates(Sx=1.1,Sy=1.1) %>%
  translate_coordinates(x=0.5,y=0.5) #%>%
  #filter_radius() %>%
  #plot(size=Diameter,color=factor(Species))+theme(legend.position = "none")

target_csv <-
  start_csv %>%
  filter(Year==2019) #%>%
  #filter_radius() %>%
  #plot(size=Diameter,color=factor(Species))+theme(legend.position="none")

write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/55/55_2015_to_2019_corrected.csv",row.names = FALSE)

# 67
start_csv <- read.csv(file = "matched_trees/67/67_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)
corrected_csv <-
  start_csv %>%
  filter(Year==2015) %>%
  scale_coordinates(Sx=1.03,Sy=1.03) %>%
  translate_coordinates(x=0,y=0) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position = "none")

target_csv <-
  start_csv %>%
  filter(Year==2019) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position="none")

write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/67/67_2015_to_2019_corrected.csv",row.names = FALSE)

# 74
start_csv <- read.csv(file = "matched_trees/74/74_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)
corrected_csv <-
  start_csv %>%
  filter(Year==2015) %>%
  scale_coordinates(Sx=1.0,Sy=1.1) %>%
  translate_coordinates(x=0,y=0) %>%
  coordinate_rotation(degree_to_radian(-16)) #%>%
  #filter_radius() %>%
  #plot(size=Diameter,color=factor(Species))+theme(legend.position = "none")

target_csv <-
  start_csv %>%
  filter(Year==2019) #%>%
  #filter_radius() %>%
  #plot(size=Diameter,color=factor(Species))+theme(legend.position="none")

write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/74/74_2015_to_2019_corrected.csv",row.names = FALSE)

# 94
start_csv <- read.csv(file = "matched_trees/94/94_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)
corrected_csv <-
  start_csv %>%
  filter(Year==2015) %>%
  scale_coordinates(Sx=1.0,Sy=1.0) %>%
  translate_coordinates(x=0.8,y=-0.8) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position = "none")

target_csv <-
  start_csv %>%
  filter(Year==2019) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position="none")

write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/94/94_2015_to_2019_corrected.csv",row.names = FALSE)

# 107
start_csv <- read.csv(file = "matched_trees/107/107_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)
corrected_csv <-
  start_csv %>%
  filter(Year==2015) %>%
  scale_coordinates(Sx=1.025,Sy=1.025) %>%
  translate_coordinates(x=0,y=0) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position = "none")

target_csv <-
  start_csv %>%
  filter(Year==2019) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position="none")

write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/107/107_2015_to_2019_corrected.csv",row.names = FALSE)

# 128
start_csv <- read.csv(file = "matched_trees/128/128_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)
corrected_csv <-
  start_csv %>%
  filter(Year==2015) %>%
  scale_coordinates(Sx=1.0,Sy=1.0) %>%
  translate_coordinates(x=-0.5,y=0) %>%
  coordinate_rotation(degree_to_radian(-13)) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position = "none")

target_csv <-
  start_csv %>%
  filter(Year==2019) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position="none")

write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/128/128_2015_to_2019_corrected.csv",row.names = FALSE)

# 158
start_csv <- read.csv(file = "matched_trees/158/158_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)
corrected_csv <-
  start_csv %>%
  filter(Year==2015) %>%
  scale_coordinates(Sx=1.0,Sy=1.15) %>%
  translate_coordinates(x=0.6,y=-0.4) %>%
  coordinate_rotation(degree_to_radian(19)) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position = "none")

target_csv <-
  start_csv %>%
  filter(Year==2019) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position="none")

write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/158/158_2015_to_2019_corrected.csv",row.names = FALSE)

# 177
start_csv <- read.csv(file = "matched_trees/177/177_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)

corrected_csv <-
  start_csv %>%
  filter(Year==2015) %>%
  scale_coordinates(Sx=1.05,Sy=1.4) %>%
  translate_coordinates(x=0,y=0) %>%
  coordinate_rotation(degree_to_radian(-30)) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position = "none")

target_csv <-
  start_csv %>%
  filter(Year==2019) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position="none")

write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/177/177_2015_to_2019_corrected.csv",row.names = FALSE)

# 226
start_csv <- read.csv(file = "matched_trees/226/226_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)

corrected_csv <-
  start_csv %>%
  filter(Year==2015) %>%
  scale_coordinates(Sx=1.0,Sy=1.0) %>%
  translate_coordinates(x=-1.9,y=1.8) %>%
  coordinate_rotation(degree_to_radian(0)) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position = "none")

target_csv <-
  start_csv %>%
  filter(Year==2019) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position="none")

write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/226/226_2015_to_2019_corrected.csv",row.names = FALSE)

# 337
start_csv <- read.csv(file = "matched_trees/337/337_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)

corrected_csv <-
  start_csv %>%
  filter(Year==2015) %>%
  scale_coordinates(Sx=1.0,Sy=1.0) %>%
  translate_coordinates(x=-1.9,y=1) %>%
  coordinate_rotation(degree_to_radian(20.5)) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position = "none")

target_csv <-
  start_csv %>%
  filter(Year==2019) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position="none")

write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/337/337_2015_to_2019_corrected.csv",row.names = FALSE)

# 347
start_csv <- read.csv(file = "matched_trees/347/347_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)

corrected_csv <-
  start_csv %>%
  filter(Year==2015) %>%
  scale_coordinates(Sx=1.0,Sy=1.0) %>%
  translate_coordinates(x=-0.5,y=0.25) %>%
  coordinate_rotation(degree_to_radian(-6)) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position = "none")

target_csv <-
  start_csv %>%
  filter(Year==2019) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position="none")

write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/347/347_2015_to_2019_corrected.csv",row.names = FALSE)

# 415
start_csv <- read.csv(file = "matched_trees/415/415_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)

corrected_csv <-
  start_csv %>%
  filter(Year==2015) %>%
  scale_coordinates(Sx=1.027,Sy=1.1) %>%
  translate_coordinates(x=0.25,y=-0.25) %>%
  coordinate_rotation(degree_to_radian(-15)) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position = "none")

target_csv <-
  start_csv %>%
  filter(Year==2019) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position="none")

write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/415/415_2015_to_2019_corrected.csv",row.names = FALSE)

# 422
start_csv <- read.csv(file = "matched_trees/422/422_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)

corrected_csv <-
  start_csv %>%
  filter(Year==2015) %>%
  scale_coordinates(Sx=1,Sy=1.1) %>%
  translate_coordinates(x=0,y=0) %>%
  coordinate_rotation(degree_to_radian(26)) %>%
  scale_coordinates(Sx=0.97,Sy=1.15) %>%
  translate_coordinates(x=-0.5,y=-0.5) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position = "none")

target_csv <-
  start_csv %>%
  filter(Year==2019) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position="none")

write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/422/422_2015_to_2019_corrected.csv",row.names = FALSE)

# 438
start_csv <- read.csv(file = "matched_trees/438/438_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)

corrected_csv <-
  start_csv %>%
  filter(Year==2015) %>%
  scale_coordinates(Sx=1.0,Sy=1.0) %>%
  translate_coordinates(x=1,y=-1) %>%
  coordinate_rotation(degree_to_radian(10)) %>%
  translate_coordinates(x=0,y=0.45) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position = "none")

target_csv <-
  start_csv %>%
  filter(Year==2019) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position="none")

write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/438/438_2015_to_2019_corrected.csv",row.names = FALSE)

# 441
start_csv <- read.csv(file = "matched_trees/441/441_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)

corrected_csv <-
  start_csv %>%
  filter(Year==2015) %>%
  scale_coordinates(Sx=0.98,Sy=1.15) %>%
  translate_coordinates(x=-0.5,y=0) %>%
  coordinate_rotation(degree_to_radian(-21)) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position = "none")

target_csv <-
  start_csv %>%
  filter(Year==2019) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position="none")

write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/441/441_2015_to_2019_corrected.csv",row.names = FALSE)

# 485
start_csv <- read.csv(file = "matched_trees/485/485_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)

corrected_csv <-
  start_csv %>%
  filter(Year==2015) %>%
  scale_coordinates(Sx=1.0,Sy=1.0) %>%
  translate_coordinates(x=-0.69,y=0.96) %>%
  coordinate_rotation(degree_to_radian(0)) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position = "none")

target_csv <-
  start_csv %>%
  filter(Year==2019) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position="none")

write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/485/485_2015_to_2019_corrected.csv",row.names = FALSE)

# 529
start_csv <- read.csv(file = "matched_trees/529/529_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)

corrected_csv <-
  start_csv %>%
  filter(Year==2015) %>%
  scale_coordinates(Sx=1.1,Sy=1.1) %>%
  translate_coordinates(x=0.5,y=-0.5) %>%
  coordinate_rotation(degree_to_radian(0)) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position = "none")

target_csv <-
  start_csv %>%
  filter(Year==2019) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position="none")

write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/529/529_2015_to_2019_corrected.csv",row.names = FALSE)


# 530
start_csv <- read.csv(file = "matched_trees/530/530_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)

corrected_csv <-
  start_csv %>%
  filter(Year==2015) %>%
  scale_coordinates(Sx=1.1,Sy=1.1) %>%
  translate_coordinates(x=0,y=1.3) %>%
  coordinate_rotation(degree_to_radian(0)) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position = "none")

target_csv <-
  start_csv %>%
  filter(Year==2019) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position="none")

write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/530/530_2015_to_2019_corrected.csv",row.names = FALSE)


#542

start_csv <- read.csv(file = "matched_trees/542/542_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)

corrected_csv <-
  start_csv %>%
  filter(Year==2015) %>%
  scale_coordinates(Sx=1.1,Sy=1.1) %>%
  translate_coordinates(x=0,y=0) %>%
  coordinate_rotation(degree_to_radian(0)) #%>%
  #filter_radius() %>%
  #plot(size=Diameter,color=factor(Species))+theme(legend.position = "none")

target_csv <-
  start_csv %>%
  filter(Year==2019) #%>%
  #filter_radius() %>%
  #plot(size=Diameter,color=factor(Species))+theme(legend.position="none")

write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/542/542_2015_to_2019_corrected.csv",row.names = FALSE)

#543

start_csv <- read.csv(file = "matched_trees/543/543_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)

corrected_csv <-
  start_csv %>%
  filter(Year==2015) %>%
  scale_coordinates(Sx=1.1,Sy=1.1) %>%
  translate_coordinates(x=-0.5,y=0) %>%
  coordinate_rotation(degree_to_radian(0)) #%>%
  #filter_radius() %>%
  #plot(size=Diameter,color=factor(Species))+theme(legend.position = "none")

target_csv <-
  start_csv %>%
  filter(Year==2019) #%>%
  #filter_radius() %>%
  #plot(size=Diameter,color=factor(Species))+theme(legend.position="none")

write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/543/543_2015_to_2019_corrected.csv",row.names = FALSE)


#1046

start_csv <- read.csv(file = "matched_trees/1046/1046_2015_to_2019.csv") %>% as.coord.data.frame(id=id,x=x,y=y,Diameter,Species,Year)

corrected_csv <-
  start_csv %>%
  filter(Year==2015) %>%
  scale_coordinates(Sx=1,Sy=1.1) %>%
  translate_coordinates(x=0,y=0) %>%
  coordinate_rotation(degree_to_radian(-15)) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position = "none")

target_csv <-
  start_csv %>%
  filter(Year==2019) #%>%
#filter_radius() %>%
#plot(size=Diameter,color=factor(Species))+theme(legend.position="none")

write.csv(x=bind_rows(corrected_csv,target_csv),file="matched_trees/1046/1046_2015_to_2019_corrected.csv",row.names = FALSE)
