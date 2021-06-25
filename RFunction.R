library('move')
library('moveVis')

rFunction <- function(data,reso,uni)
{
  Sys.setenv(tz="GMT")
  
  if (is.null(reso))
  {
    logger.info("You did not provide a suitable alignment time interval. By default we assume the mean resolution of the data set. Be aware that the calculations will likely take some time.")
    m <- align_move(data,res="mean",unit="hours")
  } else
  {
    logger.info(paste("You request an anitmation output file with alignment time intervals of",reso,uni,". Be aware that the calculations will likely take some time."))
    m <- align_move(data,res=as.numeric(reso),unit=uni) 
    #m <- align_move(data,res=reso,unit=uni) 
  }

  names_all <- namesIndiv(data)
  frames <- frames_spatial(m, path_colours = rep("red",n.indiv(m)),
                           map_service = "osm", map_type = "watercolor", alpha = 0.5,path_legend=FALSE) %>%
    add_labels(x = "Longitude", y = "Latitude") %>% 
    add_northarrow() %>%
    add_scalebar() %>%
    add_timestamps(m, type = "label") %>%
    add_progress()
  
  #frames[[100]]
  
  # animate frames
  animate_frames(frames, out_file = "animation_moveVis.gif")
  
  result <- data
  return(result)
}

  
  
  
  
  
  
  
  
  
  
