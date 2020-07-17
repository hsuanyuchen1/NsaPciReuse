library(tidyverse)
library(data.table)
library(dbscan)

source("c:/Work/Operators/TWM/scanner/rCode/clusterResults.R")
source("c:/Work/Operators/TWM/scanner/rCode/operatorClusterResult.R")
source("c:/Work/Operators/TWM/scanner/rCode/top5avg.R")

##########################################################################
oprArr = c("TWM-n78", "FET-n78", "CHT-n78", "CHT-n1")
fpath <- "c:/Work/Operators/TWM/scanner/dataFromQA/Tainan/Tainan.xlsx"
outputFolder <- "c:/Work/Operators/TWM/scanner/clusterResults/"

#parameters
rsrpThr <- -115
groupSize <- 2
distance <- 0.02

#read data
readWB = function(fpath){
  #allSheets <- readxl::excel_sheets(fpath)
  rowNum <- readxl::read_xlsx(fpath, 
                              sheet = "Series Formatted Data") %>% nrow()
  scanner <- readxl::read_xlsx(fpath, 
                               sheet = "Series Formatted Data", 
                               guess_max = rowNum) %>% setDT()
  return(scanner)
}

aFiles <- list.files("c:/Work/Operators/TWM/scanner/dataFromQA/actixWorkBook/", recursive = T, full.names = T)
scanner <- lapply(aFiles, function(x) readWB(x))


#change column names
colnames(scanner) <- sub(pattern = "NR_Scan_PCI_SortedBy_RSRP_for_NRARFCN", x = names(scanner),
            replacement = "PCI") %>% 
  sub(pattern = "_0", x=., replacement = "") %>%
  sub(pattern = "NR_Scan_SS_RSRP_SortedBy_RSRP_for_NRARFCN", x=., replacement = "RSRP")


#wide to long
scanner.melt <- melt(scanner, id.vars = names(scanner)[1:5], 
                     measure.vars = list(c("PCI_636000", "PCI_625324", "PCI_631000", "PCI_431570"),
                                         c("RSRP_636000", "RSRP_625324", "RSRP_631000", "RSRP_431570")),
                     value.name = c("PCI", "RSRP"),
                     variable.name = "ARFCN")

scanner.melt$Operator <- case_when(scanner.melt$ARFCN == 1 ~ "TWM",
                                   scanner.melt$ARFCN == 2 ~ "FET",
                                   scanner.melt$ARFCN == 3 ~ "CHT",
                                   scanner.melt$ARFCN == 4 ~ "CHT")

scanner.melt$band <- case_when(scanner.melt$ARFCN == 1 ~ "n78",
                               scanner.melt$ARFCN == 2 ~ "n78",
                               scanner.melt$ARFCN == 3 ~ "n78",
                               scanner.melt$ARFCN == 4 ~ "n1")


scanner.melt$ARFCN <- case_when(scanner.melt$ARFCN == 1 ~ 636000,
                                scanner.melt$ARFCN == 2 ~ 625324,
                                scanner.melt$ARFCN == 3 ~ 631000,
                                scanner.melt$ARFCN == 4 ~ 431570)


scanner.melt$operatorBand <- paste(scanner.melt$Operator, scanner.melt$band, sep = "-")
scanner.melt <- na.omit(scanner.melt)

#fwrite(scanner.melt, "c:/Work/Operators/TWM/scanner/rawData.csv")

#summary of unique PCI
rawData <- scanner.melt[,.(uPCI = length(unique(PCI))), by = .(operatorBand)]

#summary of unique PCI where RSRP is greater than rsrpThr
filterRSRP <- scanner.melt[RSRP > rsrpThr ,.(uPCI_rsrp = length(unique(PCI))), 
                           by = .(operatorBand)]


#cluster by operator, band, PCI
scannerCluster <- lapply(oprArr, 
                         function(x) operatorClusterResult(x, 
                                                           scanner.melt[RSRP > rsrpThr], 
                                                           eps = distance, 
                                                           minPts = groupSize)) %>% 
  rbindlist()

#export the raw data with clustering results
#fwrite(scannerCluster, paste0(outputFolder, "RSRP", rsrpThr, "GS", groupSize, "Dist", distance*100, ".csv"))


scannerCluster <- scannerCluster[cluster > 0]

oprPci <- scannerCluster[,.(count = .N), by = .(Operator, band, operatorBand, PCI, cluster)]

#filter with RSRP and gorup Size
fRsrpSize <- unique.data.frame(oprPci[,.(operatorBand, PCI)])
fRsrpSize <- fRsrpSize[,.(uPCI_RsrpSize = .N), by = .(operatorBand)]
#calculate the estimated lat/lon
clusterLocation <- lapply(1:nrow(oprPci), 
                          function(x) top5avg(scannerCluster, 
                                              oprBand = oprPci[x, operatorBand],
                                              pci = oprPci[x, PCI],
                                              clusterId = oprPci[x, cluster])) %>%
  rbindlist()


cellEstimation <- clusterLocation[,.(cellCount = .N), by = .(operatorBand)]

summaryTable <- cbind(rawData, filterRSRP[,2], fRsrpSize[,2], cellEstimation[,2])
#write summary Results
summaryTable
#fwrite(summaryTable, paste0(outputFolder, "CellCount_","RSRP", rsrpThr, "GS", groupSize, "Dist", distance*100, ".csv"))
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
