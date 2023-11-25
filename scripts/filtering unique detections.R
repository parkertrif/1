
images$timestamp <- as.POSIXct(images$timestamp, format = "%m/%d/%Y %H:%M") #read timestamp correctly
images$group <- paste(images$common_name, images$age, images$sex) # Add grouping columns


#split images by placename
placename_data <- split(images, images$placename)

#loop through each placename
for(i in seq_along(placename_data)) {
  
  placename_df <- placename_data[[i]]
  
  #arrange rows 
  placename_df <- arrange(placename_df, group, timestamp)
  
  #calculate time_since_last
  placename_df$time_since_last <- c(NA, diff(placename_df$timestamp))
  
  #fill NA  
  placename_df$time_since_last[is.na(placename_df$time_since_last)] <- -1000
  
  #re-assign into list
  placename_data[[i]] <- placename_df
  
}



# filtering out for unique detections
summarize_detections <- function(df) {
  
  df %>%
    filter(time_since_last > 600 | time_since_last < 0) %>%
    mutate(detection_id = cumsum(time_since_last > 600 | time_since_last < 0)) %>% 
    group_by(detection_id) %>%
    summarise(
      placename = first(placename),
      group = first(group),
      earliest_timestamp = min(timestamp),  
      number_of_objects = max(number_of_objects)
    )
  
}

# apply function to each placename dataframe
results <- lapply(placename_data, summarize_detections)

# bind results into one dataframe
all_detections <- do.call(rbind, results)
