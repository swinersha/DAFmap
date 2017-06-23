
# A function to create a unique colour palatette the same length as teh vector.
#
# Tom Swinfield
# 17-04-11


library(palettetown)


unique.col<-function(x, pal.name){
  x<-sort(x)
  u.x<-unique(x)
  col.pal<-palettetown::ichooseyou(pal.name, length(u.x))
  y<-vector(mode='character', length=length(x))
  for(i in 1:length(col.pal))
    y[x==u.x[i]]<-col.pal[i]
  return(y)
}    
