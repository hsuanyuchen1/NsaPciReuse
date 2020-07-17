#fpath <- "c:/Work/Operators/TWM/scanner/dataFromQA/actixWorkBook/TaoYuan.xlsx"
source("c:/Work/Operators/TWM/scanner/rCode/calCellCount.R")


outputFolder <- "c:/Work/Operators/TWM/scanner/"


#parameters
rsrpThr <- -115
groupSize <- 2
distance <- 0.02

aFiles <- list.files("c:/Work/Operators/TWM/scanner/", full.names = T, pattern = "Xin")

t1 = Sys.time()
sapply(aFiles, function(x) calCellCount(x, outputFolder, rsrpThr=-115, groupSize=2, distance))

Sys.time() - t1

#calCellCount("c:/Work/Operators/TWM/scanner/dataFromQA/actixWorkBook/TaoYuan.xlsx", outputFolder, 
#             rsrpThr = -105, groupSize, distance)

