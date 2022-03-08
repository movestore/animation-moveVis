library('move')
library('moveVis')
library('fields')

rFunction <- function(data,reso=NULL,uni="hours",maptype="watercolor",frames_per_sec=25)
{
  Sys.setenv(tz="UTC")
  
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

  #m.list <- move::split(m) # split m into list by individual
  #m.list <- mapply(x = m.list, y = cols, function(x, y){
  #  x$colour <- y
  #  return(x)
  #})
  #m <- moveStack(m.list) 
  
  frames <- frames_spatial(m, path_colours=tim.colors(n.indiv(data)), map_service = "osm", map_type = maptype, alpha = 0.5, equidistant = FALSE) %>%
    add_labels(x = "Longitude", y = "Latitude") %>% 
    add_northarrow() %>%
    add_scalebar() %>%
    add_timestamps(m, type = "label") %>%
    add_progress(colour = "white")
  
  #frames[[100]]
  
  # animate frames
  animate_frames(frames, out_file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"animation_moveVis.gif"),overwrite=TRUE, fps=frames_per_sec)
  #animate_frames(frames, out_file = "animation_moveVis.gif",overwrite=TRUE)
  
  result <- data
  return(result)
}

  
  
  
  
  
  
  
  
  
  
