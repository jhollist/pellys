#' Function to determine 301 winning percent from an input data.frame
#' 
#' This takes an input dataframe of a specified format and caluclates the 
#' winning percentage
#' 
#' @param myDF input dataframe of 301 stats
#' 
#' @export
winPerc301<-function(myDF)
{
  wins<-length(myDF$PointsRemaining[myDF$PointsRemaining==0])
  total<-length(myDF$PointsRemaining)
  return(data.frame(wins=wins,total=total,percent=wins/total))
}