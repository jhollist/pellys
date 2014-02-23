\name{readPellysData}
\alias{readPellysData}
\title{Read Pelly's Players Dart Data from Google Docs}
\usage{
readPellysData(game = c("301", "cricket"))
}
\arguments{
  \item{game}{text string to indicate which file is to be
  read, cricket of 301}
}
\description{
This function read's in Pelly's Players data.  It assumes
data are stored on Google Docs.  Two specific file formats
are expected, one for Cricket games and one for 301 games
}

