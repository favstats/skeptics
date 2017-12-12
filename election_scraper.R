
#devtools::install_github("soodoku/tuber", build_vignettes = TRUE)
require(tuber)
require(ggplot2)
require(gtools)
require(ggthemes)
options(stringsAsFactors = FALSE) 
require(plyr)

## login credentials: console.google.com
yt_oauth("", "")


#######################################
# channel ID Hillary Clinton: UCLRYsOHrkk5qcIhtq033bLQ
# channel ID Donald Trump: UCAql2DyGU2un1Ei2nMYsqOA
######################################

dataframeFromJSON <- function(l) {
  l1 <- lapply(l, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  keys <- unique(unlist(lapply(l1, names)))
  l2 <- lapply(l1, '[', keys)
  l3 <- lapply(l2, setNames, keys)
  res <- data.frame(do.call(rbind, l3))
  return(res)
}

getAllChannelVideos <- function(channel_id=NULL){
  
  channelAct <- list_channel_activities(channel_id = channel_id ,part = "contentDetails")
  df <- dataframeFromJSON(channelAct$items)
  
  token <- channelAct$nextPageToken
  
  repeat{
    channelActSub <- list_channel_activities(channel_id = channel_id, part = "contentDetails",page_token = token)
    dff <- dataframeFromJSON(channelActSub$items)
    df <- smartbind(df, dff)
    
    print(channelActSub$nextPageToken)
    token <- channelActSub$nextPageToken
    if(is.null(token)){
      break
    }
    
  }
  return(df)
}


getVideoStatsDF <- function(video_id){
  stats <- get_stats(video_id)
  return(data.frame(t(unlist(stats))))
}

getVideoDetailsDF <- function(video_id){
  details <- get_video_details(video_id)
  return(data.frame(t(unlist(details))))
}
################# 

### get all channel Videos for a certain channel! --> Hillary
allActivities <- getAllChannelVideos("UCLRYsOHrkk5qcIhtq033bLQ")

list_of_video_ids <- allActivities$contentDetails.upload.videoId

allVideoStats <- ldply(list_of_video_ids, .fun = getVideoStatsDF)

allVideoDetails <- ldply(list_of_video_ids, .fun = getVideoDetailsDF)


##########################################
### same for Donald Trump.

allActivitiesDT <- getAllChannelVideos("UCAql2DyGU2un1Ei2nMYsqOA")

list_of_video_ids <- allActivitiesDT$contentDetails.upload.videoId

allVideoStats <- ldply(list_of_video_ids, .fun = getVideoStatsDF)

allVideoDetails <- ldply(list_of_video_ids, .fun = getVideoDetailsDF)