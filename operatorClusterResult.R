library(data.table)
library(magrittr)

#cluster data w.r.t operator
operatorClusterResult = function(oprBand, scannerL, eps, minPts){
  uPci <- scannerL[ARFCN == oprBand, PCI] %>% unique()
  tscanner <- scannerL[ARFCN == oprBand]
  result <- lapply(uPci,
                   function(x) clusterResults(x, tscanner, oprBand,
                                              eps = eps, minPts = minPts)) %>%
    rbindlist()
  return(result)
}