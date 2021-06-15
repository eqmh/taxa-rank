### This code extracts OBIS records using dataset ID and plots number of records per taxonomic rank.
### Created by Enrique Montes
### June 14, 2021.

library(robis)

## download the data using the dataset ID using the OBIS URL (e.g. from this https://obis.org/dataset/3cdb3dca-4df4-4931-9155-b41da2ce4075)

## list dataset IDs 
## 1: Galapagos
## 2: Montemar
## 3: Pto Madryn
## 4: Biddeford
## 5: Concepcion
## 6: Gorgona
## 7: Fernando de Noronha
## 8: Arraial

id_tbl <- list(
"3cdb3dca-4df4-4931-9155-b41da2ce4075", 
"5e009291-b1ce-454f-bbcf-d98142eaeda9", 
"1d8fb994-64af-4a57-abe2-19573cc91f85",
"86ed1f5d-e911-4423-b965-8349248dfee2",
"e4a2fe06-f082-4f36-9165-73263a121508",
"871e92ab-2a00-4851-b948-cb6dda862f9c",
"dae283ff-1835-465c-b058-fa6417d0ff2d",
"7ece3f71-6bae-4cc4-8619-d560a564f187"
)

list_sz = length(id_tbl)
output <- matrix(ncol=5, nrow=list_sz)

for (i in 1:list_sz) {
datasetID <- id_tbl[i]
records <- occurrence(datasetid = datasetID)

total <- nrow(records)
spp <- as.data.frame(records$species)
genus <- as.data.frame(records$genus)
fam <- as.data.frame(records$family)

num_spp <- colSums(!is.na(spp))
num_genus <- colSums(!is.na(genus)) - num_spp
num_fam <- colSums(!is.na(fam)) - num_spp - num_genus
higher <- total - num_spp - num_genus - num_fam

output[i,] <- c(num_spp, num_genus, num_fam, higher, total)
}

## Excel table with all records (see 'network_sampling_optimization/submission/resubmission/spp_lists)

data_tbl <- read.csv(spp_list_summary)


## plot the data per taxa rank


