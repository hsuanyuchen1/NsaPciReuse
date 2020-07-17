library(data.table)

#calculate the location mass center for the top 5 RSRP points
top5avg = function(scannerCluster, oprBand, pci, clusterId){
  temp <- scannerCluster[ARFCN == oprBand & PCI == pci & cluster == clusterId]
  temp <- temp[order(RSRP, decreasing = T)]
  temp <- head(temp, 2)
  location <- temp[,.(lon = mean(Longitude), lat = mean(Latitude)),
                   by = .(ARFCN, PCI, cluster)]
  return(location)
}
