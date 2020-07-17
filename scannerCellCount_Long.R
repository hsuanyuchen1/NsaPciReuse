library(tidyverse)
library(data.table)
library(dbscan)

source("c:/Work/Operators/TWM/scanner/rCode/clusterResults.R")
source("c:/Work/Operators/TWM/scanner/rCode/operatorClusterResult.R")
source("c:/Work/Operators/TWM/scanner/rCode/readWB2L.R")
source("c:/Work/Operators/TWM/scanner/rCode/top5avg.R")

##########################################################################
#oprArr = c("TWM-n78", "FET-n78", "CHT-n78", "CHT-n1")
fpath <- "c:/Work/Operators/TWM/scanner/dataFromQA/actixWorkBook/TaoYuan.xlsx"
outputFolder <- "c:/Work/Operators/TWM/scanner/dataFromQA/Results"


#parameters
rsrpThr <- -115
groupSize <- 2
distance <- 0.02

scanner <- readWB2L(fpath)


#summary of unique PCI
rawData <- scanner[,.(uPCI = length(unique(PCI))), by = .(ARFCN)]

#summary of unique PCI where RSRP is greater than rsrpThr
filterRSRP <- scanner[RSRP > rsrpThr ,.(uPCI_rsrp = length(unique(PCI))), 
                           by = .(ARFCN)]


#cluster by operator, band, PCI
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

summaryTable <- cbind(rawData, filterRSRP[,2], fRsrpSize[,2], cellEstimation[,2])
#write summary Results
#summaryTable


#write data to outFolder
fwrite(summaryTable, paste0(outputFolder, basename(fpath) %>% tools::file_path_sans_ext(),
                            "CellCount_","RSRP", rsrpThr, "GS", groupSize, "Dist", distance*100, ".csv"))
fwrite(clusterLocation, paste0(outputFolder, basename(fpath) %>% tools::file_path_sans_ext(),
                            "Location_","RSRP", rsrpThr, "GS", groupSize, "Dist", distance*100, ".csv"))

################################overlay district border######################
# library(sf)
# clusterLocation.sf <- st_as_sf(clusterLocation, coords = c("lon", "lat"), crs=4326)
# district.sf <- st_read("c:/Work/ToolBox/twPolygon/Twn5000a2.TAB", options = "ENCODING=UTF-8") %>%
#   st_transform(.,crs=st_crs(clusterLocation.sf))
# 
# 
# colnames(district.sf)[5:7] <- c("PC", "Area" ,"engName")
# district.sf <- district.sf[,5:8]
# 
# #filter only TaoYuan districts
# taoYuan <- c("338", "337", "336", "335", "334", "333", "330", "328", "327", "326", "325", "324", "320")
# district.sf <- district.sf[district.sf$PC %in% taoYuan,]
# 
# districtResult <- st_join(clusterLocation.sf, district.sf) %>% as.data.table()
# distrcitResult.agg <- districtResult[,.(cellCount = .N), by = .(operatorBand, engName)]
# distrcitResult.agg.w <- dcast(distrcitResult.agg, engName ~ operatorBand, value.var = "cellCount")
# 
# #write cell count on district and operator-band level
# #fwrite(distrcitResult.agg.w, paste0(outputFolder, "District_CellCount_","RSRP", 
# #                                    rsrpThr, "GS", groupSize, "Dist", distance*100, ".csv"))
