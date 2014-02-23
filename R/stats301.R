#' Function to determine 301 stats from an input data.frame
#' 
#' This takes an input dataframe of a specified format and calculates several
#' 301 stats.  Output file is a data.frame.
#' 
#' @param myDF input dataframe of 301 stats
#' 
#' @export
stats301<-function(myDF)
{
  startscore<-myDF$StartingScore
  pointsremain<-myDF$PointsRemaining
  totaldarts<-myDF$TotalDarts
  return(data.frame(avgStartScore=mean(startscore),avgPointsRemain=mean(pointsremain),
                    avgTotalDarts=mean(totaldarts),
                    avgPPD=(sum(startscore)-sum(pointsremain))/sum(totaldarts),
                    highPPD=max((startscore-pointsremain)/totaldarts),
                    highOut=max(myDF$Out)))
}