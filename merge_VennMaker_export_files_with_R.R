#-------------------------------------------------------------------------------------------
#Author: Michael Kronenwett (2016, http://www.kronenwett-adolphs.com)
#
# Merging separated VennMaker (1.4.1 or higher) interview files to one interview file
#
# Each ego, alter and compute file will be merged to one file
# Additionally, the number of cluster for each network will be calculated. The resuls will
# be added to the compute file.
#
# Licence: CC-BY-SA
#
# Results:
# EGO*.csv --> R_Ego.csv
# ALTER*.csv --> R_Alteri.csv
# COMPUTE*.csv --> R_Compute.csv
#-------------------------------------------------------------------------------------------
require("network")
require("sna")
require("igraph")

#IMPORTANT: Please, change the path:
path <- "/home/nexus/temp/r/"

#---------------------------------------------------------
#Alteri data
#---------------------------------------------------------

filesAlter <- list.files(path, pattern="ALTER_");

allAlteriData <- NA
counter <- 1;

for(d in filesAlter){
  
  daten <- read.csv(file=paste(path,d, sep=""),head=TRUE,sep=";");
  id<-paste("_",counter, sep="");
  
  #Alteri data
  daten$id_Ego<-paste(daten$id_Ego, id, sep="");
  daten$id_Alter<-paste(daten$id_Alter, id, sep="");
  
  counter <- counter+1
  
  if (is.na(allAlteriData)==TRUE){
    allAlteriData <- daten;
  }else{
    allAlteriData<-rbind(allAlteriData, daten);
  }
}

allAlteriData;


#Save alteri data
write.csv2(allAlteriData, file=paste(path,"R_Alteri.csv", sep=""))


#---------------------------------------------------------
#Ego data
#---------------------------------------------------------

filesEgo <- list.files(path, pattern="EGO_");

allEgoData <- vector()
counter <- 1;

for(d in filesEgo){
  
  daten <- read.csv2(file=paste(path,d, sep=""),head=TRUE, na.strings=NA, quote="\"", dec=",", fill=TRUE);
  
  #add a unique network number to the ego id
  id<-paste("_",counter, sep="");
  daten$id_Ego<-paste(daten$id_Ego, id, sep="");
  counter = counter+1
  
  allEgoData<-rbind(allEgoData, daten);
  
}
#Save ego data
write.csv2(allEgoData, file=paste(path,"R_Ego.csv", sep=""))

#---------------------------------------------------------
#Compute data
#---------------------------------------------------------

filesCompute <- list.files(path, pattern="COMPUTE_");

allComputeData <- vector()
counter <- 1;

for(d in filesCompute){
  
  daten <- read.csv(file=paste(path,d, sep=""),head=TRUE, sep=";");
  
  #add a unique network number to the ego id
  id<-paste("_",counter, sep="");
  daten$Network.map<-paste(daten$Network.map, id, sep="");
  counter = counter+1
  
  allComputeData<-rbind(allComputeData, daten);
}


#---------------------------------------------------------
#Merge: Ego data and Compute data
#---------------------------------------------------------

allEgoComputeData <- cbind(allEgoData, allComputeData)



#---------------------------------------------------------------------------
# Read all pajek files from the directory and calculate the clique number
#---------------------------------------------------------------------------

cliqueNr <- vector()
filesNet <- list.files(path, pattern=".net");

for(d in filesNet){
  
  #read the net file
  netInhalt<-network::read.paj(file=paste(path, d, sep=""))
  
  #a little complicated...
  testMatrix <- network::as.matrix.network.adjacency(netInhalt)
  #generate a graph
  netzwerkGraph <- graph.adjacency(testMatrix, mode="undirected", diag=FALSE);
  
  #draw the graph
  plot(netzwerkGraph)
  
  #calculate the clique
  cliqueAnzahl <- cliques(netzwerkGraph, min=3)
  cliqueNumber <- length(cliqueAnzahl)
  
  #show the result
  print(d)
  print("Number of cliques:")
  print(cliqueNumber)
  
  print(cliqueAnzahl)
  
  cliqueNr <- rbind(cliqueNr, cliqueNumber)
  
}

#add the result to the compute data
allEgoComputeData <- cbind(allEgoData, cliqueNr);


#---------------------------------------------------------------------------
# Save the results
#---------------------------------------------------------------------------

write.csv2(allEgoComputeData, file=paste(path,"R_Compute.csv", sep=""))
