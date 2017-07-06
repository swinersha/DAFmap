#' col_pal
#' 
#' A convenient wrapper for displaying threat levels within sectors
#' according to a defined number of colours. It is also possible to
#' specify the range of interest.
#' 
#' @param x A numeric vector
#' @param no_survey Logical indicating whether or not any survey data was collected.
#' @param n_cols The number of unique colours to be displayed.
#' @param x_range The numeric range in x to be displayed
#' @return A vector of hexidecimal colours 
#' @export
#' @author Tom Swinfield
#' @details Created 17-02-21

col_pal<-function(x, no_survey=NULL, n_cols=10, x_range=NULL){
  col.pal<-col.pal<-brewer.pal(n_cols, "Blues")
  if(is.null(x_range))
    x_cuts<-cut(x, seq(min(x, na.rm=T), max(x, na.rm=T), length = n_cols)) # bins the x values
  else
    x_cuts<-cut(x, seq(x_range[1], x_range[2], length = n_cols)) # bins the x values
  x_cuts<-as.numeric(x_cuts) # as a factor
  x_cols<-col.pal[x_cuts]
  if(!is.null(no_survey))
    x_cols[no_survey]<-'grey'
  return(x_cols)
}

#' col_pal_thrt
#' 
#' A convenient wrapper for displaying threat levels within sectors
#' according to a defined number of colours. It is also possible to
#' specify the range of interest.
#' 
#' @param x A numeric vector
#' @param no_survey Logical indicating whether or not any survey data was collected.
#' @param breaks A numberic vector of length 5 indicating where x should be cut
#' to define the different threat levels.
#' @return A vector of hexidecimal colours 
#' @export
#' @author Tom Swinfield
#' @details Created 17-02-21

col_pal_thrt<-function(x, no_survey=NULL, breaks){
  n_cols<-length(breaks)-1
  col.pal<-brewer.pal(n_cols, "Blues")

  df_labels<-c('low', 'med', 'high', 'extreme')
  x_cuts<-cut(x, breaks = breaks, labels = df_labels, include.lowest=TRUE)
  
  x_cuts<-as.numeric(x_cuts) # as a factor
  x_cols<-col.pal[x_cuts]
  if(!is.null(no_survey))
    x_cols[no_survey]<-"grey"
  x_cols[is.na(x)]<-"grey"
  return(x_cols)
}

#' col_pal_total_thrt
#' 
#' A convenient wrapper for displaying threat levels that have already been
#' classified.
#' 
#' @param x A character vector
#' @return A vector of hexidecimal colours 
#' @export
#' @author Tom Swinfield
#' @details Created 17-02-21
#' 
col_pal_total_thrt<-function(x){
  
  n_cols<-nlevels(x)
  col.pal<-col.pal<-brewer.pal(n_cols, "Blues")
  
  x<-as.numeric(x)
  
  x_cols<-col.pal[x]
  x_cols[is.na(x_cols)]<-'grey'
  
  return(x_cols)
}

#' plot_reset
#' 
#' Resets the plotting area so that a legend for multiple images can be placed neatly.
#' 
#' @export
#' @author Tom Swinfield
#' @details Created 17-02-21

plot_reset <- function() {
  par(mfrow=c(1, 1), oma=rep(0, 4), mar=rep(0, 4), new=TRUE)
  plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)
}

