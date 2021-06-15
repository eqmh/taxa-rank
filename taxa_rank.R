### This code extracts OBIS records using dataset ID and plots number of records per taxonomic rank.
### Created by Enrique Montes
### June 14, 2021.

library(robis)
library(ggplot2)
library(reshape2)

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

data_tbl <- read.csv('spp_list_summary.csv')
taxa_frac <- data_tbl[, 2:5]/data_tbl$Total
taxa_frac <- cbind(Locality = data_tbl[,1], taxa_frac)

## Reorder matrix
taxa_frac_2 <- melt(taxa_frac, id.vars='Locality')
taxa_frac_2$Locality <- factor(taxa_frac_2$Locality, levels = c("Antarctica", "Punta Arenas","Puerto Madryn", "Concepcion", "Montemar", "Arraial do Cabo", "Costa Das Algas",
                                                                "Santa Cruz", "Fernando de Noronha", "Isla Gorgona", "Massachusetts", "Biddeford", "Giant Stairs", "Chamberlain",
                                                                "Grindstone", "N. Maine"),
                               
                               labels = c("Antartica (Chile)", "Punta Arenas (Chile)", "Puerto Madryn (Argentina)", 
                                          "Concepcion (Chile)", "Montemar (Chile)", "Arraial do Cabo (Brazil)",
                                          "Costa das Algas (Brazil)", "Santa Cruz (Galapagos I.)", "Fernando de Noronha (Brazil)",
                                          "Isla Gorgona (Colombia)", "Massachusetts (USA)", "Biddeford (USA)", "Giantstairs (USA)",
                                          "Chamberlain (USA)", "Grindstone (USA)", "N. Maine (USA)"))

## plot the data per taxa rank (palettes: http://rstudio-pubs-static.s3.amazonaws.com/5312_98fc1aba2d5740dd849a5ab797cc2c8d.html)

# Stacked + percent
ggplot(taxa_frac_2, aes(fill=variable, y=value, x=Locality)) + 
  geom_bar(colour = "black", position ="stack", stat="identity") + 
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)), text = element_text(size = 16)) +
  theme(axis.title.x = element_blank()) +
  ylab("Frequency") + xlab("Locality")

## Calculate mean and sd values per taxa rank

mean_spp <- mean(taxa_frac$Species)
mean_genus <-  mean(taxa_frac$Genus)
mean_fam <-  mean(taxa_frac$Family)
mean_higher <-  mean(taxa_frac$Higher)

sd_spp <- sd(taxa_frac$Species)
sd_genus <-  sd(taxa_frac$Genus)
sd_fam <-  sd(taxa_frac$Family)
sd_higher <-  sd(taxa_frac$Higher)

