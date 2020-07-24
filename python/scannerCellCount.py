import re
import pandas as pd
import numpy as np
from sklearn.cluster import DBSCAN
import os

#set parameters
rsrpThr = -115
minDist = 0.02
minPts = 3

inputFile = input("Please insert file path: ")

#directories
#inputFile = r"C:\Work\Operators\scanner\pythonCode\data\TaoYuan.xlsx"
outputFile = os.path.splitext(inputFile)[0] + "Cluster.csv"
#read excel
scanner = pd.read_excel(inputFile, sheet_name="Series Formatted Data")

#change column names: only keep  PCI, RSRP and the corresponding ARFCN
oldColName = scanner.columns
pattern = r"(NR_Scan_|_SortedBy_RSRP_for_NRARFCN_|_0|SS_)"
newColName = {}

#create oldColName to newColName mapping dictionary
for colName in oldColName:
    temp = re.sub(pattern, "", colName)
    newColName[colName] = temp

#rename coluimn names
scanner = scanner.rename(columns = newColName)

#wide to long
scannerL = pd.wide_to_long(scanner, ['PCI', 'RSRP'], j = "ARFCN", i=["Longitude", "Latitude"])
#scannerL.head()

#remove NaN
scannerL = scannerL.dropna(axis=0,how="any")
#scannerL.describe()

#set lon, lat, ARFCN to column
scannerL = scannerL.reset_index(['Longitude', 'Latitude', 'ARFCN'])
#scannerL.head()

scannerLfRsrp = scannerL[scannerL['RSRP'] > rsrpThr ]
#scannerLfRsrp.describe()


uPCI = scannerL.groupby('ARFCN').PCI.nunique()
uPCI = pd.DataFrame(uPCI).reset_index()
uPCI = uPCI.rename(columns = {'ARFCN': 'ARFCN', 'PCI': 'uPCI'})


uPCIfRsrp = scannerLfRsrp.groupby('ARFCN').PCI.nunique()



tList = list()

#do DBSCAN on ARFCN and PCI level
for oprBand in scannerLfRsrp['ARFCN'].unique():
    #get unique PCI on ARFCN level
    for tpci in scannerLfRsrp[scannerLfRsrp['ARFCN'] == oprBand]['PCI'].unique():
        temp = scannerLfRsrp[(scannerLfRsrp['ARFCN'] == oprBand) & (scannerLfRsrp['PCI'] == tpci)]
        #DBSCAN
        db = DBSCAN(eps = minDist, min_samples=minPts).fit(temp[['Latitude', 'Longitude']])
        tdf = pd.concat([temp.reset_index(drop=True), pd.DataFrame(db.labels_, columns= ["clusterID"])], axis=1)
        tList.append(tdf)

#Merge all the list into one dataframe
tMerge = pd.concat(tList)

#remove non clustered data points where clusterID = -1
tMergeCluster = tMerge[tMerge['clusterID'] > -1]

tMergeCluster.to_csv(outputFile, index=False)
print("parameters:")
print(">>> RSRP Threshold: ", rsrpThr)
print(">>> Minimal group size: ", minPts)
print(">>> Distance (km): ", minDist*100)
print("Result is created at",outputFile)
