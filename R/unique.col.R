#' unique.col
#' 
#' A function to create a unique colour palatette the same length as the vector provided.
#' The function depends upon palette town.
#' 
#' @param x An integer vector to be classified by seperate colours.
#' @param pal.name A character string describing the palette town palette to use.
#' @return A vector of hexidecimal colours
#' @export
#' @author Tom Swinfield
#' @details 
#' 
#' Created 17-04-11


#library(palettetown)


unique.col<-function(x, pal.name){
  x<-sort(x)
  u.x<-unique(x)
  col.pal<-palettetown::ichooseyou(pal.name, length(u.x))
  y<-vector(mode='character', length=length(x))
  for(i in 1:length(col.pal))
    y[x==u.x[i]]<-col.pal[i]
  return(y)
}    
