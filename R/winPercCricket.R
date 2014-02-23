#' Function to determine crickte winning percent from an input data.frame
#' 
#' This takes an input dataframe of a specified format and caluclates the 
#' winning percentage
#' 
#' @param myDF input dataframe of cricket stats
#' 
#' @export
winPercCricket<-function(myDF)
{
  wins<-length(myDF$Won[myDF$Won==1])
  total<-length(myDF$Won)
  return(data.frame(wins=wins,total=total,percent=wins/total))
}