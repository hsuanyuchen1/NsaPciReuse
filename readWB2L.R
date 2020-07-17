##########################################################################
#read actix workbook and change from wide to long and remove the NA rows.
##########################################################################

library(data.table)
library(magrittr)



readWB2L = function(fpath){
  #allSheets <- readxl::excel_sheets(fpath)
  rowNum <- readxl::read_xlsx(fpath, 
                              sheet = "Series Formatted Data") %>% nrow()
  scanner <- readxl::read_xlsx(fpath, 
                               sheet = "Series Formatted Data", 
                               guess_max = rowNum) %>% setDT()
  
  colnames(scanner) <- sub(pattern = "NR_Scan_PCI_SortedBy_RSRP_for_NRARFCN", x = names(scanner),
                           replacement = "PCI") %>% 
    sub(pattern = "_0", x=., replacement = "") %>%
    sub(pattern = "NR_Scan_SS_RSRP_SortedBy_RSRP_for_NRARFCN", x=., replacement = "RSRP")
  
  
  arfcn <- gsub(pattern = "PCI_", "", grep("PCI", names(scanner), value = T))
  
  test <- melt(scanner, measure = patterns("PCI", "RSRP"), 
               value.name = c("PCI", "RSRP"),
               variable.name = "ARFCN")[ 
               ,ARFCN := factor(ARFCN, labels = arfcn)]
  test <- na.omit(test)
  
  return(test)
}
