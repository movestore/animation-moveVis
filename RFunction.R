library('move2')
library('move')
library('moveVis')
library('fields')

## The parameter "data" is reserved for the data object passed on from the previous app

# to display messages to the user in the log file of the App in MoveApps
# one can use the function from the logger.R file:
# logger.fatal(), logger.error(), logger.warn(), logger.info(), logger.debug(), logger.trace()

rFunction <- function(data,reso=NULL,uni="hours",maptype="voyager",mapres=0.2,frames_per_sec=25,col_opt="one",other="sex",show_legend=TRUE,capt="",file_format="mp4",ext_adap=1)
{
  data2 <- data
  data <- moveStack(to_move(data))
  
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
  
  data.split <- move::split(data)
  
  if (col_opt=="one")
  {
    logger.info("You have seleted to colour all your tracks in one colour (red).")
    cols <- rep("red",length(data.split))
    legend_titl <- "all Tracks"
  } else if (col_opt=="trackid")
  {
    logger.info("You have selected track ID for track colouring.")
    cols <- tim.colors(length(data.split))
    legend_titl <- "Track IDs"
  } else if (col_opt=="animalid")
  {
    logger.info("You have selected animal ID for track colouring. Note that this is only possible if 'individual.local.identifier' or 'local.identifier' are availbe in your data set. Else track ID will be used here.")
    iddata <- idData(data)
    #names(iddata) <- make.names(names(iddata),allow_=FALSE)
    
    if (any(names(iddata)=="individual_local_identifier")) {
      
      anims <- unlist(lapply(data.split,function(x) 
      {
        iddatax <- idData(x)
        names(iddatax) <- make.names(names(iddatax),allow_=FALSE)
        iddatax$individual.local.identifier
      }),use.names=FALSE)
      
    } else if (any(names(iddata)=="local_identifier")) {
      anims <- unlist(lapply(data.split,function(x)
      {
        iddatax <- idData(x)
        names(iddatax) <- make.names(names(iddatax),allow_=FALSE)
        iddatax$local.identifier
      }),use.names=FALSE)
    } else {
      anims <- names(data.split) #fallback to trackIds
    }
    uanims <- unique(anims)
    ucols <- tim.colors(length(uanims))
    cols <- ucols[apply(as.matrix(anims),1,function(x) which(uanims==x))]
    legend_titl <- "Animal IDs"
  } else if (col_opt=="other")
  {
    if (other %in% names(idData(data)))
    {
      logger.info(paste("You have selected the animal attribute",other,"for track colouring."))
      attr <- unlist(lapply(data.split,function(x) idData(x)[,other]),use.names=FALSE)
      uattr <- unique(attr)
      ucols <- tim.colors(length(uattr))
      cols <- ucols[apply(as.matrix(attr),1,function(x) which(uattr==x))]
      legend_titl <- other
    } else if (other %in% names(data@data))
    {
      logger.info(paste("Your have selected the track attribute",other,"for track colouring. Note that the value of this attribute at the first timestamp of each track is used in case they differ by location."))
      attr <- unlist(lapply(data.split,function(x) x@data[1,other]),use.names=FALSE) #use first element of attribute only!
      uattr <- unique(attr)
      ucols <- tim.colors(length(uattr))
      cols <- ucols[apply(as.matrix(attr),1,function(x) which(uattr==x))]
      legend_titl <- other
    } else
    {
      logger.info (paste("Your selected attribute for coloring of the tracks is not a Track or Animals attribute of your data set. Please go back and reconfigure the App. All tracks will be coloured in one colour (red)."))
      cols <- "red"
      legend_titl <- "all Tracks"
    }
  } else
  {
    logger.info ("No viable option for track colouring selected. Fallback to single colour (red).")
    cols <- "red"
    legend_titl <- "all Tracks"
  }
  
  #todo: here another option is "mapbox" (requires token), need to ask Jakob if possible to use OSM mirror
  frames <- frames_spatial(m, path_colours=cols, ext=ex ,path_legend=show_legend, path_legend_title= legend_titl, map_service = "carto", map_type = maptype, map_res=mapres, alpha = 0.5, equidistant = FALSE) %>%
    add_labels(x = "Longitude", y = "Latitude",caption=capt) %>% 
    add_northarrow() %>%
    add_scalebar() %>%
    add_timestamps(type = "label") %>%
    add_progress(colour = "white")
  
  #frames[[100]]
  
  # animate frames
  animate_frames(frames, out_file = appArtifactPath(paste0("animation_moveVis.",file_format)),overwrite=TRUE, fps=frames_per_sec)
  #animate_frames(frames, out_file = "animation_moveVis.gif",overwrite=TRUE)
  
  result <- data2
  return(result)
}



