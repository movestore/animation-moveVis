library('move')
library('moveVis')
library('fields')

rFunction <- function(data,reso=NULL,uni="hours",maptype="watercolor",mapres=0.2,frames_per_sec=25,show_legend=TRUE,capt="",file_format="mp4",ext_adap=1)
{
  Sys.setenv(tz="UTC")
  
  if (is.null(reso))
  {
    logger.info("You did not provide a suitable alignment time interval. By default we assume the mean resolution of the data set. Beware that the calculations will likely take some time.")
    m <- align_move(data,res="mean",unit=uni)
  } else
  {
    logger.info(paste("You request an anitmation output file with alignment time intervals of",reso,uni,". Beware that the calculations will likely take some time."))
    m <- align_move(data,res=as.numeric(reso),unit=uni) 
    #m <- align_move(data,res=reso,unit=uni) 
  }
  
  if (mapres<0 | mapres>1) 
  {
    logger.info("Your map resolution value is outside of the required boundaries (between 0 and 1). Please go back and adapt. Here, the default value of 0.2 is used.")
    mapres <- 0.2
  }

  #m.list <- move::split(m) # split m into list by individual
  #m.list <- mapply(x = m.list, y = cols, function(x, y){
  #  x$colour <- y
  #  return(x)
  #})
  #m <- moveStack(m.list) 
  
  ex <- extent(data)*ext_adap
  
  #frames <- frames_spatial(m, path_colours=tim.colors(n.indiv(data)), ext=ex ,path_legend=show_legend, path_legend_title= "Track IDs", map_service = "osm", map_type = maptype, map_res=mapres, alpha = 0.5, equidistant = FALSE) %>%
  #  add_labels(x = "Longitude", y = "Latitude",caption=capt) %>% 
  #  add_northarrow() %>%
  #  add_scalebar() %>%
  #  add_timestamps(m, type = "label") %>%
  #  add_progress(colour = "white")
  
  frames <- frames_spatial(m, path_colours=tim.colors(n.indiv(data)), ext=ex ,path_legend=show_legend, path_legend_title= "Track IDs", map_service = "carto", map_type = maptype, map_res=mapres, alpha = 0.5, equidistant = FALSE) %>%
    add_labels(x = "Longitude", y = "Latitude",caption=capt) %>% 
    add_northarrow() %>%
    add_scalebar() %>%
    add_timestamps(m, type = "label") %>%
    add_progress(colour = "white")
  
  #frames[[100]]
  
  # animate frames
  animate_frames(frames, out_file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"animation_moveVis.",file_format),overwrite=TRUE, fps=frames_per_sec)
  #animate_frames(frames, out_file = "animation_moveVis.gif",overwrite=TRUE)
  
  result <- data
  return(result)
}

  
  
  
  
  
  
  
  
  
  
