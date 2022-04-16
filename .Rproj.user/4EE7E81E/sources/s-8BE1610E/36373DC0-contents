save_transformation <- function(data,source_year,target_year,filepath="matched_trees",radius=10,res=0.1){
  stopifnot(all(c("standnumber","id","x","y","Year","Diameter","Species")%in%colnames({data})))

  if(!dir.exists(filepath)) dir.create(filepath,showWarnings = FALSE) #Create results folder if not already exists.

  for(i in 1:(data %>% distinct(standnumber) %>% nrow())){
    message(paste0(i," out of ",(data %>% distinct(standnumber) %>% nrow())))

    currentid <- (data %>% distinct(standnumber))[i,][[1]]
    dir.create(paste0(filepath,"/",currentid),showWarnings = FALSE) #Create plot results folder.

    data2 <- data %>% filter(standnumber == currentid)
    #Skip if not source year and target year are available
    if(!all(c(source_year, target_year)%in%data2$"Year")){
      message("Source or Target Year not available for plot. Jumping to next plot.")
      next()
    }

    message("Transforming coordinates...")
    #Transform coordinates with NiftyRegR and save images of source and target.
    reg <- data2 %>% coordinate_transform2(target_year = {target_year},source_year = {source_year},radius={radius},res={res},filepath="matched_trees")

    #Save coordinates from transformed source and original target.
    write.csv(reg,row.names = FALSE,file = paste0(filepath,"/",currentid,"/",currentid,"_",source_year,"_to_",target_year,".csv"))

    message("Saving transformed image...")
    #Save image of transformed source.
    reg %>% filter(Year=={source_year}) %>% produce_gauss_array(point_strength = 'Diameter') %>% save_gauss(filename = paste0(filepath,"/",currentid,"/",currentid,"_",source_year,"_to_",target_year,".png"))


    #Print confirmation
    message(paste0("Plot ",currentid," Done."))

  }

  #message when finished.
  message("Finished all plots.")
}


