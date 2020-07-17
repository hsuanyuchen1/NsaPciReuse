library(tidyverse)
library(data.table)
library(dbscan)

source("c:/Work/Operators/TWM/scanner/rCode/clusterResults.R")
source("c:/Work/Operators/TWM/scanner/rCode/operatorClusterResult.R")
source("c:/Work/Operators/TWM/scanner/rCode/readWB2L.R")
source("c:/Work/Operators/TWM/scanner/rCode/top5avg.R")

##########################################################################

# fpath <- "c:/Work/Operators/TWM/scanner/dataFromQA/actixWorkBook/TaoYuan.xlsx"
# outputFolder <- "c:/Work/Operators/TWM/scanner/dataFromQA/Results"
# 
# 
# #parameters
# rsrpThr <- -115
# groupSize <- 2
# distance <- 0.02

calCellCount = function(fpath, outputFolder, rsrpThr, groupSize, distance){
  scanner <- readWB2L(fpath)
  
  #summary of unique PCI
  rawData <- scanner[,.(uPCI = length(unique(PCI))), by = .(ARFCN)]
  
  #summary of unique PCI where RSRP is greater than rsrpThr
  filterRSRP <- scanner[RSRP > rsrpThr ,.(uPCI_rsrp = length(unique(PCI))), 
                        by = .(ARFCN)]
  
  
  #cluster by ARFCN, band, PCI
  scannerCluster <- lapply(unique(scanner$ARFCN), 
                           function(x) operatorClusterResult(x, 
                                                             scanner[RSRP > rsrpThr], 
                                                             eps = distance, 
                                                             minPts = groupSize)) %>% 
    rbindlist()
  
  #export the raw data with clustering results
  #fwrite(scannerCluster, paste0(outputFolder, "RSRP", rsrpThr, "GS", groupSize, "Dist", distance*100, ".csv"))
  
  
  scannerCluster <- scannerCluster[cluster > 0]
  
  oprPci <- scannerCluster[,.(count = .N), by = .(ARFCN, PCI, cluster)]
  
  #filter with RSRP and gorup Size
  fRsrpSize <- unique.data.frame(oprPci[,.(ARFCN, PCI)])
  fRsrpSize <- fRsrpSize[,.(uPCI_RsrpSize = .N), by = .(ARFCN)]
  #calculate the estimated lat/lon
  clusterLocation <- lapply(1:nrow(oprPci), 
                            function(x) top5avg(scannerCluster, 
                                                oprBand = oprPci[x, ARFCN],
                                                pci = oprPci[x, PCI],
                                                clusterId = oprPci[x, cluster])) %>%
    rbindlist()
  
  
  cellEstimation <- clusterLocation[,.(cellCount = .N), by = .(ARFCN)]
  
  #summaryTable <- cbind(rawData, filterRSRP[,2], fRsrpSize[,2], cellEstimation[,2])
  summaryTable <- filterRSRP[rawData, on = .(ARFCN)]
  summaryTable <- fRsrpSize[summaryTable, on = .(ARFCN)]
  summaryTable <- cellEstimation[summaryTable, on = .(ARFCN)]
  summaryTable <- summaryTable[,.(ARFCN,uPCI, uPCI_rsrp, uPCI_RsrpSize, cellCount)]
  #summaryTable
  #write summary Results
  #summaryTable
  
  
  #write data to outFolder
  fwrite(summaryTable, paste0(outputFolder, basename(fpath) %>% tools::file_path_sans_ext(),
                              "CellCount_","RSRP", rsrpThr, "GS", groupSize, "Dist", distance*100, ".csv"))
  fwrite(clusterLocation, paste0(outputFolder, basename(fpath) %>% tools::file_path_sans_ext(),
                                 "Location_","RSRP", rsrpThr, "GS", groupSize, "Dist", distance*100, ".csv"))
  fwrite(scannerCluster, paste0(outputFolder, basename(fpath) %>% tools::file_path_sans_ext(),
                                 "RawCluster_","RSRP", rsrpThr, "GS", groupSize, "Dist", distance*100, ".csv"))
  
  
}