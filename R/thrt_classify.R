

threat_labels<-c('rendah', 'sedang', 'tinggi', 'extrim')

# A function to classify the threat level:
thrt_classify<-function(x, breaks, labels=threat_labels, include.lowest = TRUE)
  cut(x,
      breaks = breaks,
      labels = labels,
      include.lowest=TRUE) 

# Function to calculate the overall threat level:
greatest_thrt<-function(x, levels){
  x<-factor(x, levels=levels)
  if(all(is.na(x)))
    y<-factor(NA, levels = levels)
  else
    y <- sort(x, decreasing =TRUE)[1]
  return(y)
}
