#' sector_assign_kpi
#' 
#' Assigns calculated KPIs at the field office level to the individual sectors
#' 
#' @param x the sector dataframe
#' @param y the field office kpis per threat level as a data frame
#' @return  x updated to contain the kpis from y
#' @export
#' @author Tom Swinfield
#' 
#' Created 17-09-25
#' 

sector_assign_kpi<-function(x, y){
  y_update<-lapply(1:nrow(x), function(i){
    ind<-x$Kantor[i] == y$Kantor & x$total_thrt[i] == y$total_thrt
    return(y[ind,])
  })
  y_update<-do.call(rbind, y_update)
  x_update<-cbind(x, y_update[,c("hari", "periode")])
  return(x_update)
}
