#' area.lost
#' 
#' A convenience function to classify the threat level:
#' 
#' @param x A numberic vector indicating the threat observations to be classified.
#' @param breaks A numeric vector indicating the cut points for the different threat levels
#' @param labels The threat labels for the different threat levels
#' @param include.lowest Include the lower bound for the break
#' @return The area lost within each sector 
#' @export
#' @author Tom Swinfield
#' @details Created 17-02-21

thrt_classify<-function(x, breaks, labels=threat_labels, include.lowest = TRUE)
  cut(x,
      breaks = breaks,
      labels = labels,
      include.lowest=TRUE) 

#' greatest_thrt
#' 
#' Calculate the overall threat level for a sector.
#' 
#' @param x factor vector with levels indicating the different threat levels
#' @param levels Labels describing the threat levels
#' @return factor of length 1.
#' @export
#' @author Tom Swinfield
#' @details Created 17-02-21

greatest_thrt<-function(x, levels){
  x<-factor(x, levels=levels)
  if(all(is.na(x)))
    y<-factor(NA, levels = levels)
  else
    y <- sort(x, decreasing =TRUE)[1]
  return(y)
}
