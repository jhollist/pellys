#' Read Pelly's Players Dart Data from Google Docs
#' 
#' This function read's in Pelly's Players data.  It assumes data are stored 
#' on Google Docs.  Two specific file formats are expected, one for Cricket
#' games and one for 301 games
#' 
#' @param game  text string to indicate which file is to be read, cricket of 301
#' 
#' @export
readPellysData<-function(game=c("301","cricket")){
  if(game=="301"){
    url<-getURL("https://docs.google.com/spreadsheet/pub?key=0AiV4WHc7_JnQdHVweEF3dV9CcUlNWm43MHJ5SXUwSlE&output=csv",ssl.verifypeer=FALSE)
    xdf<-read.csv(textConnection(url))
    xdf<-na.omit(xdf[xdf$Name!="Forfeit",])
    xdf[,2]<-as.Date(xdf[,2], "%m/%d/%Y")
  } else {
    url<-getURL("https://docs.google.com/spreadsheet/pub?key=0AiV4WHc7_JnQdG0yVUdFYVNjdnM1cGhxLWNlZ19xU1E&single=true&gid=0&output=csv",ssl.verifypeer=FALSE)
    xdf<-read.csv(textConnection(url))
    #removes Forfeits and Formats date
    xdf<-na.omit(xdf[xdf$Name1!="Forfeit",])
    xdf<-na.omit(xdf[xdf$Name2!="Forfeit",])
    xdf[,2]<-as.Date(xdf[,2], "%m/%d/%Y")
  }
  return(xdf)
}  