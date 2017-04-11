#-----------------------------------------
#
# This function produces a nice wrapper for displaying threats within
# sectors
#
# Tom Swinfield
# 17-02-21
#
#-----------------------------------------

col_pal<-function(x, no_survey=NULL, n_cols=10, x_range=NULL){
  col.pal<-colorRampPalette(colors=c('white', 'red'))
  if(is.null(x_range))
    x_cuts<-cut(x, seq(min(x, na.rm=T), max(x, na.rm=T), length = n_cols)) # bins the x values
  else
    x_cuts<-cut(x, seq(x_range[1], x_range[2], length = n_cols)) # bins the x values
  x_cuts<-as.numeric(x_cuts) # as a factor
  x_cols<-col.pal(n_cols)[x_cuts]
  if(!is.null(no_survey))
    x_cols[no_survey]<-'grey'
  return(x_cols)
}

col_pal_thrt<-function(x, no_survey=NULL, breaks){
  n_cols<-length(breaks)-1
  col.pal<-colorRampPalette(colors=c('white', 'blue'))

  df_labels<-c('low', 'med', 'high', 'extreme')
  x_cuts<-cut(x, breaks = breaks, labels = df_labels, include.lowest=TRUE)
  
  x_cuts<-as.numeric(x_cuts) # as a factor
  x_cols<-col.pal(n_cols)[x_cuts]
  if(!is.null(no_survey))
    x_cols[no_survey]<-"grey"
  x_cols[is.na(x)]<-"grey"
  return(x_cols)
}


col_pal_total_thrt<-function(x){
  
  n_cols<-nlevels(x)
  col.pal<-colorRampPalette(colors=c('white', 'blue'))
  
  x<-as.numeric(x)
  
  x_cols<-col.pal(n_cols)[x]
  x_cols[is.na(x_cols)]<-'grey'
  
  return(x_cols)
}

