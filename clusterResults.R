library(data.table)

#cluster data with fixed operator and pci
clusterResults = function(tPci, scannerL, oprBand ,minPts, eps){
  
  temp <- scannerL[PCI == tPci & ARFCN == oprBand]
  tempCluster <- dbscan(temp[,.(Longitude, Latitude)], eps, minPts)
  temp$cluster <- tempCluster$cluster
  #maxCluster <- max(temp$cluster)
  #output <- cbind(oprBand, tPci, maxCluster) %>% as.data.table()
  return(temp)
  
}
