if (!require(rgdal)) install.packages("rgdal"); library(rgdal)                #used for spatial processing
if (!require(gdalUtils)) install.packages("gdalUtils"); library(gdalUtils)    #used for spatial processing
if (!require(sp)) install.packages("sp"); library(sp)                         #used for spatial processing
if (!require(GISTools)) install.packages('GISTools'); library(GISTools)
if (!require(dplyr)) install.packages('dplyr'); library(dplyr)
if (!require(sf)) install.packages('sf'); library(sf)
if (!require(raster)) install.packages('raster'); library(raster)
if (!require(cleangeo)) install.packages('cleangeo'); library(cleangeo)

#if (!require(readxl)) install.packages('readxl'); library(readxl)
if (!require(openxlsx)) install.packages('openxlsx'); library(openxlsx)
#if (!require(xlsx)) install.packages('xlsx'); library(xlsx)

if (!require(leaflet)) install.packages('leaflet'); library(leaflet) #Package with mapping functions
if (!require(viridis)) install.packages('viridis'); library(viridis)   #Provides color palettes that are good for mapping rain and account for colour blindness
if (!require(shiny)) install.packages('shiny'); library(shiny)
if (!require(mapview)) install.packages('mapview'); library(mapview)
if (!require(htmltools)) install.packages('htmltools'); library(htmltools)

if (!require(plyr)) install.packages('plyr'); library(plyr)   #needed for the ddply function
if (!require(tictoc)) install.packages("tictoc"); library(tictoc)                         #used for timing processing


#' A function to find the REC network downstream of a reach
#'
#'This function generates a vector of the reach ID numbers (nzsegment attribute) downstream of a reach. It uses the REC V2.4 network. See \href{https://niwa.co.nz/freshwater-and-estuaries/management-tools/river-environment-classification-0}{NIWA REC V2} for details about REC V2.
#'@param RECNetwork An REC V2 network (either dataframe of spatial dataframe), with at least nzsegment (or nzsgmnt), TO_NODE and FROM_NODE (or FROM_NO) attributes.
#'@param SourceReach The reach number (i.e. nzsgmnt attribute) of the reach for which the downstream network is required
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A vector of reach numbers (nzsgmnt attributes)
#'@keywords REC River Environment Classification
#'@export
DownstreamReachFinder <- function(RECNetwork=RECReachNetwork,SourceReach=7239110){ #The reach ID is for the Manawatu at Teachers College water quality reporting site

  #Initialise the vector of reach ID's downstream of the source
  DownstreamReaches <- SourceReach
  
  #If the nzsegment column is called nzsgmnt then rename it
  names(RECNetwork)[which(names(RECNetwork) == "nzsgmnt")] <- "nzsegment"
  
  #If the nzsegment column is called FROM_NO then rename it
  names(RECNetwork)[which(names(RECNetwork) == "FROM_NO")] <- "FROM_NODE"
  
  #Identify the row index of the source reach
  SourceSiteReachIndex<-which(RECNetwork$nzsegment==SourceReach)
  
  #Identify the reach immediately downstream of the source reach
  downstream_index<-which(RECNetwork$FROM_NODE==RECNetwork$TO_NODE[SourceSiteReachIndex])
  
  #downstream_reach<-RECNetwork$nzsgmnt[downstream_index]
  
  #Sequentially work downstream
  while (length(downstream_index)>0) {
    DownstreamReaches <- c(DownstreamReaches,RECNetwork$nzsegment[downstream_index])
    downstream_index<-which(RECNetwork$FROM_NODE==RECNetwork$TO_NODE[downstream_index])
    #downstream_index<-next_downstream_index
  } #end of while
  return(DownstreamReaches)
}

#' A function to label an REC network based on tributary size
#'
#'This function crawl up an REC network and numbers the tributaries. It starts from the bottom and work up to the first intersection. It decides which branch is the primary and which is a secondary, based on which is the longest to the headwaters, then gives a new number to the smaller tributary and keeps going. It uses the REC V2.4 network. See \href{https://niwa.co.nz/freshwater-and-estuaries/management-tools/river-environment-classification-0}{NIWA REC V2} for details about REC V2.
#'@param RECNetwork An REC V2 network (either dataframe of spatial dataframe), with at least nzsgmnt, TO_NODE, FROM_NO and hdw_dst attributes
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A list (one for each each independent network/diconnected catchment)) of vectors of tributary labels, and a side effect of a file called "OutletReaches.csv" listing the nzsgmnt attributes of the reaches that are considered to be at the bottom of the network.
#'@keywords REC River Environment Classification
#'@export
NetworkLabeler <- function(RECNetwork=MyREC){
  
  #Find the row indices of all the outlet reach's by finding which "to" nodes  don't have a corresponding "from" node
  OutletReachIndices <- which(!RECNetwork$TO_NODE %in% RECNetwork$FROM_NO)
  names(OutletReachIndices) <- 1:length(OutletReachIndices)
  
  #Save the outlet reaches to an external file so that we can figure out their names manually
  write.table(RECNetwork@data$nzsgmnt[OutletReachIndices],file.path(DataDirectory,"OutletReaches.csv"),sep=",",row.names = FALSE)
  
  #Crawl each network in turn
  ReachLabels <- lapply(OutletReachIndices, function(OutletReachIndex) {   
    #Initialise the label list
    RowNumber          <- 1
    CurrentLabel       <- 1
    Labels             <- data.frame(nzsgmnt=NA,Label=NA)
    Labels[RowNumber,] <- c(RECNetwork$nzsgmnt[OutletReachIndex],1)
    
    CurrentReachIndex  <- OutletReachIndex
    upstream_indices   <- which(RECNetwork$TO_NODE==RECNetwork$FROM_NO[CurrentReachIndex])
    LeftToDoIndices    <- upstream_indices
    
    while (length(LeftToDoIndices) > 0) {
      #Check if there are some upstream indices
      BiggestBranchIndex <- which.max(RECNetwork$hdw_dst[upstream_indices])
      
      CurrentReachIndex  <- upstream_indices[BiggestBranchIndex]
      LeftToDoIndices    <- LeftToDoIndices[LeftToDoIndices != CurrentReachIndex]
      RowNumber          <- RowNumber + 1
      
      Labels[RowNumber,] <- c(RECNetwork$nzsgmnt[CurrentReachIndex],CurrentLabel)
      upstream_indices   <- which(RECNetwork$TO_NODE==RECNetwork$FROM_NO[CurrentReachIndex])
      if (length(upstream_indices) == 0) {
        CurrentLabel = CurrentLabel + 1
        upstream_indices <- LeftToDoIndices[which.max(RECNetwork$hdw_dst[LeftToDoIndices])]
      } else {
        LeftToDoIndices <- c(upstream_indices,LeftToDoIndices)
      }
    }
    return(Labels)
  })
  
  
  return(ReachLabels)
}

#' A function to determine where each tributary connects to its parent river. This is needed to provide the river network connectivity table used by CASM.
#' CASM understands a network in terms of Tributary Names, Confluence Names and Confluence Locations.
#' Confluence Name is the name of the river that a tributary flows into.
#' Confluence Location is the distance along the confluence from its start, that the tributary joins it.
#'
#'@param RECNetwork An REC V2 network (either dataframe of spatial dataframe)
#'@param TributaryLabelList A list of tributary labels. One llist for each independent/disconnected catchment. The output of the NetworkLabeler() function
#'@param OutletReachNameLookUpTable A data frame of outlet reach nzsgmnt numbers and real-world names.
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A dataframe. One row for each tributary. Columns of nzsgmnt, tributary name, confluence name, confluence location in kilometres.
#'@keywords REC River Environment Classification CASM
#'@export
TributaryConnectionCreator <- function(RECNetwork = CompleteSpatialNetwork, TributaryLabelList = TribLabelList, OutletReachNameLookUpTable = OutletReachNames){
  
  #For each independent catchment, find the distance along the reach to the confluence
  CatchmentTribLinkages <- lapply(seq_along(TributaryLabelList), function(CatchmentIndex) { 
    
    #Get the tributary labels for all REC reaches within the current independent catchment 
    CatchmentTribLabels <- TributaryLabelList[[CatchmentIndex]]
    
    #Lookup the catchment name based on the OutletReachNames look up table. Assume that only one reach in the OutletReachName look up table will match the reach numbers in the tributary.
    CatchmentName <- OutletReachNames$Name[OutletReachNames$nzsegment %in% CatchmentTribLabels$nzsgmnt]
    #print(CatchmentName)
    
    #Find all the unique tributary labels
    UniqueTribs <- unique(CatchmentTribLabels$Label)
    
    #for each tributary find the minimum LENGTHD attribute from the REC data, the REC reach immediately downstream of the tributary, and the tributary label of the REC reach that is immediately downstream. This builds a matrix of 4 numbers for each tributary, giving the minimum LENGTHD, the lowest nzsgmnt, the highest nzsgmnt below the tributary, and the label of the tributary below (i.e. the confluence name)
    ConfluenceTotalDistances <- sapply(UniqueTribs, function(TribLabel) {
      
      #Get the REC data for the current tributary
      ReachData <- RECNetwork[RECNetwork$nzsgmnt %in% CatchmentTribLabels$nzsgmnt[CatchmentTribLabels$Label == TribLabel],]
      
      #Find the smallest LENGTHD attribute for the tributary. Note the special case of reach 7260002 (Kaikokopu Stream) which doesn't have a LENGTHD attribute. Ideally we could simply ignore it, but it is a one-reach tributary (it is the outlet of a sub zone without point source or measurment sites within it) so it has to be used
      TribConnectionTotalDistance <- min(ReachData$LENGTHD)
      if (is.na(TribConnectionTotalDistance)) TribConnectionTotalDistance <- 0
      
      #Find which reach is the lowest in the tributary, based on the hdw_dst attribute
      LowestReach <- ReachData$nzsgmnt[which.max(ReachData$hdw_dst)] 
      
      #Find the reach immediately below the lowest reach
      ReachBelow <- RECNetwork$nzsgmnt[which(RECNetwork$FROM_NO == RECNetwork$TO_NODE[RECNetwork$nzsgmnt == LowestReach])]
      
      #Special case if it is the lowest tributary, as it is effectively the mainstem
      if(length(ReachBelow)==0) {
        ReachBelow <- NA
        TribBelow <- 1 }
      else {TribBelow <- CatchmentTribLabels$Label[CatchmentTribLabels$nzsgmnt == ReachBelow]}
      
      return(c(TribConnectionTotalDistance,LowestReach,ReachBelow,TribBelow))
    })
    
    #CASM needs the distance of a confluence above the previous confluence. So the LENGTHD of the bottom of the parent tributary needs to be subtracted from the LENGTHD of the current tributary. This is achieved by using the tributary values just created for each catchment.
    ConfluenceCorrectedDistances <- apply(ConfluenceTotalDistances, 2, function(x) {
      CorrectedDistance <- x[1] - ConfluenceTotalDistances[1,x[4]] 
    })
    
    #These confluence distances are added as a row to the rest of the tributary information
    AllDistances <- rbind(ConfluenceTotalDistances,ConfluenceCorrectedDistances)
    
    #Lastly, just the useful information is retained, and the labels are formatted to distinguish one catchment from another
    #I now want the tributary label, and the distance along the parent tributary, and the parent tributary label
    TributaryDetails <- data.frame("nzsgmnt"=AllDistances[2,],"Tributary Name" = paste0(CatchmentName,"-",seq(1:ncol(AllDistances))), "Confluence Stream" = paste0(CatchmentName,"-",AllDistances[4,]), "Confluence Location (km)" = round(AllDistances[5,]/1000,0), check.names = FALSE, stringsAsFactors = FALSE)
  })
  
  #Combine all the catchment information into a single data frame.
  AllCatchments <- do.call(rbind,CatchmentTribLinkages)
  rownames(AllCatchments) <- NULL
  return(AllCatchments)
}

#' A function to create a CASM node location table given an REC network with additional attributes of CASM tributary labels and CASM tributary distances
#'
#'This function generates a data frame of CASM Node names, CASM tributary, CASM tributary location 
#'
#'@param CASMRECNetwork An REC V2 network (either dataframe of spatial dataframe), with at least nzsgmnt, CASMTrib and CASMTrib_Loc attributes
#'@param CASMNodes A dataframe of node names and REC V2 reach number (i.e. nzsgmnt attribute) of the nodes for which the CASM table is to be prepared
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A dataframe of CASM Node names, CASM tributary, CASM tributary location 
#'@keywords REC River Environment Classification CASM
#'@export
CASMNodeTablePreparer <- function(CASMRECNetwork=RECReachNetwork, NetworkLabelList = NetworkLabelList, TributaryConnectionTable = TributaryConnectionTable, CASMNodes=data.frame(NodeName = c("test","Manawatu at Teachers College"),nzsgmnt= c(7140020,7239110))){
  
  #Work through each catchment
  AllTribLocations <- lapply(NetworkLabelList, function(SingleCatchmentNeworkLabels) {
    
    #Get the catchment name
    CatchmentName <- OutletReachNames$Name[OutletReachNames$nzsegment %in% SingleCatchmentNeworkLabels$nzsgmnt]
    print(CatchmentName)
    
    #Find which CASMNodes are within the current catchment
    CatchmentNodes <- CASMNodes[(CASMNodes$nzsgmnt %in% SingleCatchmentNeworkLabels$nzsgmnt),]
    
    #Work through all the Nodes that are in this catchment
    if (nrow(CatchmentNodes) > 0) {
      #Now work through the nodes that are within the catchment, find the outlet reach, and then get the distance.
      TribLocations <- sapply(seq_along(CatchmentNodes$NodeName), function(NodeIndex){
        
        CASMNode <- CatchmentNodes[NodeIndex,]
        NodeNumber  <- SingleCatchmentNeworkLabels$Label[SingleCatchmentNeworkLabels$nzsgmnt == CASMNode$nzsgmnt]
        NodeTribName <- paste0(CatchmentName,"-",NodeNumber)
        
        #Look up the trib ID (prepended with the catchment name) of the reach of the current node and find the reach number (nzsgmnt attribute) of its confluence
        TribOutletReach <- TributaryConnectionTable$nzsgmnt[which(TributaryConnectionTable$`Tributary Name` == NodeTribName)]
        
        #Subtract the LENGTHD of the current reach from the LENGTHD of its tributary outlet reach
        LocationOnTrib <- CASMRECNetwork$LENGTHD[CASMRECNetwork$nzsgmnt == CASMNode$nzsgmnt] - CASMRECNetwork$LENGTHD[CASMRECNetwork$nzsgmnt == TribOutletReach]
        Result <- c(nzsgmnt=CASMNode$nzsgmnt,CASMNodeName=as.character(CASMNode$NodeName),TribName = NodeTribName,TribLocn = round(LocationOnTrib/1000,0))
        return(Result)   
      })
    } else {NULL}
    
    
  })
  
  #Turn the catchment-based list into a data frame
  CASMNodeTable <- data.frame(t(do.call(cbind,AllTribLocations)))
  #Convert the numbers into numbers
  CASMNodeTable$nzsgmnt <- as.numeric(levels(CASMNodeTable$nzsgmnt))[CASMNodeTable$nzsgmnt]
  CASMNodeTable$TribLocn <- as.numeric(levels(CASMNodeTable$TribLocn))[CASMNodeTable$TribLocn]
  return(CASMNodeTable)
}


#' A function to add labels to the REC network, based on the result of the network labeler and the OutletReachname look up table
#'
#'This function generates a data frame of CASM Node names, CASM tributary, CASM tributary location 
#'
#'@param NetworkLabels Ideally the output from the NetworkLabeler function. A list of dataframes (one for each independent river network) with the REC V2 "nzsgmnt" of confluence reaches and "Label" which is the assigned tributary number that the confluence is the start of.
#'@param OutletReachNamesLookUpTable A dataframe of names and REC V2 reach number (i.e. nzsgmnt attribute) for the lowest reach in each independent catchment.
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A dataframe of reach numbers ("nzsgmnt") and tributary labels ("Label")
#'@keywords REC River Environment Classification CASM
#'@export
ReachLabeler <- function(NetworkLabels=NetworkLabelList,OutletReachNamesLookUpTable = OutletReachNames){
  PrefixedLabels <- lapply(NetworkLabels, function(SingleCatchmentNeworkLabels) {
    #Get the catchment name
    CatchmentName <- OutletReachNamesLookUpTable$Name[OutletReachNamesLookUpTable$nzsegment %in% SingleCatchmentNeworkLabels$nzsgmnt]
    
    #Prefix the catchment name on to the network labels
    PrefixedNetworkLabels <- paste0(CatchmentName,"-",SingleCatchmentNeworkLabels$Label)
  })
  
  PrefixedLabelVector <- unlist(PrefixedLabels)
  REC_nzsegment <- do.call(rbind,NetworkLabels)$nzsgmnt
  LabeledNetwork <- data.frame(nzsgmnt = REC_nzsegment, Prefixedlabel = PrefixedLabelVector, stringsAsFactors = FALSE)
  return(LabeledNetwork)
}

#' A function to combine spatial data sources of climate, land use, irrigation and soil properties, and uses them to lookup leach rates based on a Ministry of Primary Industries designation. The processing is carried out at a 250 m x 250 m grid scale.
#'
#'This function generates a raster object of leach rates 
#'
#'@param ClimateData A spatial data file (in ESRI polygon shapefile format) of the climate regions, as used by the MPI lookup table. Unknown origins.
#'@param LanduseData A spatial data file (in ESRI polygon shapefile format) of the land use, as used by Horizons. Unknown origins.
#'@param IrrigatedLandData A spatial data file (in ESRI polygon shapefile format) of the irrigated land, as used by the MPI lookup table. A "1" represents irrigated. Originally sourced from the  \href{https://data.mfe.govt.nz/layer/90838-irrigated-land-area-2017/} \href{https://niwa.co.nz/freshwater-and-estuaries/management-tools/river-environment-classification-0}{MFE data portal}
#'@param IrrigableLandData A spatial data file (in ESRI polygon shapefile format) of land that is irrigable, as used by the MPI lookup table. A "1" represents irrigable. This is based on slope and elevation, but the exact source and details of preparation are unknown.
#'@param PAWData A spatial data file (in ESRI polygon shapefile format) of the plant available water (also known as profile available water), as used by the MPI lookup table. Originally sourced from the Fundamental Soils Layer data available from the \href{https://lris.scinfo.org.nz/layer/48100-fsl-profile-available-water/}{Landcare - Manaaki Whenua data portal}
#'@param MPILeachRateData A csv file with the first 5 columns (names are not important) providing, in order: "Climate";"Landuse";"Plant Available Water depth";"Irrigable Land";"Irrigated Land". A  sixth column labelled "N_loss" must also be present.
#'@param LanduseToLanduseLUT A csv file with two columns, one designated as "MPIClassNo" with the Ministry for Primary Industries land use classes, as used in the MPILeachRateDatalookup table. The other column designated "Horizons_Landuse_Name" with the land use names as provided in the LanduseData spatial data file.

#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A raster object of leach rates
#'@keywords REC River Environment Classification CASM
#'@export
LeachRateRasterCreator <- function(ClimateData=file.path(GISDataDirectory,ClimateShapeFileName),
                                   LanduseData=file.path(GISDataDirectory,SubZoneLanduseLUCShapeFileName),
                                   IrrigatedLandData=file.path(GISDataDirectory,IrrigatedLandShapeFileName),
                                   IrrigableLandData=file.path(GISDataDirectory,IrrigableLandShapeFileName),
                                   PAWData=file.path(GISDataDirectory,PAWShapeFileName),
                                   MPILeachRateData = file.path(DataDirectory,MPILeachRateLUTFile),
                                   LanduseToLanduseLUT = file.path(DataDirectory,LanduseToLanduseLUTFile)){
  #Load the lookup table that converts Horizon's Landuse names to the Landuse codes used by the MPI leaching-rate lookup table. 
  LanduseToLanduseLookUpTable <- read.csv(LanduseToLanduseLUT)
  
  #Load MPI lookup table of total nitrogen leaching rates based on climate, land use, PAW, irrigation, irrigable land
  MPILeachRateLookUpTable<- read.csv(MPILeachRateData)
  #Check for duplicates!! This has ocurred in the past and caused an error later on which took hours to figure out!
  MPILeachRateLookUpTable <- MPILeachRateLookUpTable[!duplicated(MPILeachRateLookUpTable[,1:5]),]
  #browser()
  #concatenate all the predictor class values into a long string to provide a single column to look up.
  MPILeachRateLookUpTable$CombinedCriteria <- do.call(paste,MPILeachRateLookUpTable[1:5])
  
  #Load climate spatial data
  ClimateSpatial <- readOGR(ClimateData, stringsAsFactors = FALSE)
  #Make sure the class id is numeric
  ClimateSpatial@data$id <- as.numeric(ClimateSpatial@data$id)
  #Convert to raster, note the creation of a base raster, which all subsequent raster's align to
  RasterBase <- raster(resolution = 250, ext = extent(ClimateSpatial), crs = proj4string(ClimateSpatial) )
  ClimateRaster <- rasterize(ClimateSpatial,RasterBase,"id")
  
  #Load the spatial data with the landuse, land use capability and submanagement zones alltogether
  SubZoneLanduseLUCSpatial <- readOGR(LanduseData,stringsAsFactors = FALSE)
  SubZoneLanduseLUCSpatial <- spTransform(SubZoneLanduseLUCSpatial,CRS("+init=epsg:2193") )
  
  #Determine the landuse code number needed to calculate MPI leaching rates
  SubZoneLanduseLUCSpatial@data$MPILanduseCode <-
    LanduseToLanduseLookUpTable$MPIClassNo[match(SubZoneLanduseLUCSpatial@data$RegiScLand,
                                                 LanduseToLanduseLookUpTable$Horizons_Landuse_Name)]
  LanduseRaster <- rasterize(SubZoneLanduseLUCSpatial,RasterBase,"MPILanduseCode")
  
  #Load Irrigable land spatial data
  IrrigableLandSpatial <- readOGR(IrrigableLandData,stringsAsFactors = FALSE)
  IrrigableLandSpatial@data$Irrigable <- as.numeric(IrrigableLandSpatial@data$Irrigable)
  IrrigableRaster <- rasterize(IrrigableLandSpatial, RasterBase, rep(1,length(IrrigableLandSpatial)),background = 0)
  
  #Load irrigated land spatial data
  IrrigatedLandSpatial <- readOGR(IrrigatedLandData,stringsAsFactors = FALSE)
  IrrigatedLandSpatial@data$Irrigated <- as.numeric(IrrigatedLandSpatial@data$Irrigated)
  IrrigatedRaster <- rasterize(IrrigatedLandSpatial, RasterBase, rep(1,length(IrrigatedLandSpatial)),background = 0)
  
  #Load Plant Available Water (PAW) spatial data
  PAWSpatial <- readOGR(PAWData, stringsAsFactors = FALSE)
  #Convert PAW values to numbers (I don't know why they are characters)
  PAWSpatial$PAW_Cat2 <- as.numeric(PAWSpatial$PAW_Cat2)
  PAWRaster <- rasterize(PAWSpatial,RasterBase,"PAW_Cat2")
  
  #Create a raster brick with all the parameters needed to determine the MPI leach rate
  TotalRaster <- brick(ClimateRaster,LanduseRaster,PAWRaster,IrrigableRaster,IrrigatedRaster)
  names(TotalRaster) <- c("Climate","Landuse","PAW","Irrigable","Irrigated")
  #Mask the raster brick to just the Horizons area. This helps clean up the irrigated and irrigable areas, which had never been clipped.
  TotalRaster <- rasterize(x=SubZoneLanduseLUCSpatial,y=TotalRaster,mask=TRUE)
  
  #Save a copy for later
  writeRaster(TotalRaster,file.path(GISDataDirectory,PredictorRasterFileName),overwrite=TRUE)

  
  # #Ton asked me to find which Horizon's predictor combinations didn't exist in the MPI look up table.
  # #Here is how to do that:
  # raspt <- rasterToPoints(TotalRaster)
  # PredictorCombinations <- as.data.frame(raspt[,c(3:7)])
  # UniquePredictorCombinations <- unique(PredictorCombinations)
  # names(UniquePredictorCombinations) <- names(MPILeachRateLookUpTable)[1:5]
  # UniquePredictorCombinations <- UniquePredictorCombinations[complete.cases(UniquePredictorCombinations),]
  # Ton also asked for a file of all the unique combinations
  # write.table(UniquePredictorCombinations, file.path(DataDirectory,"UniquePredictorCombinations250x250.csv"),row.names = FALSE, sep=",")
  # library(dplyr)
  # bob <- anti_join(UniquePredictorCombinations, MPILeachRateLookUpTable[,1:5])
  # charlie <- bob[complete.cases(bob),]
  # write.table(charlie,file.path(DataDirectory,"MissingLeachRatePredictorCombinations.csv"),sep=",",quote=FALSE, row.names = FALSE)
  #Now calculate leach rates using the MPI lookup table. This is done by using the raster brick of the preditors. Each x,y cell of the brick is sampled (which gives a vector of 5 values) and this vector is converted into a long string. This is compared to the combined-class column in the MPI leaching rate look up table to get the Nitrogen loss.
  
  #Create a criteria raster
  
  #i <-0
  #Use "calc" to work through each cell x,y cell of the raster brick
  LeachRateRaster <- calc(TotalRaster, function(x) {
    #i <<- i+1
    #print(i)
    #if(i %in% 42153) browser()
    #Concatenate the predictor values of the current x,y, cell
    CriteriaToLookup <- paste(x,collapse=" ")
    #if(complete.cases(x)) browser()
    #lookup the current cell's predictor string in the look up table
    N_loss <- MPILeachRateLookUpTable$N_loss[MPILeachRateLookUpTable$CombinedCriteria %in% CriteriaToLookup]
    
    #Catch any missing values and make them NA
    if(length(N_loss) == 0) N_loss <- NA
    
    return(N_loss)
  })
  
}

#' A function to  rasterize the zone-landuse-LUC data.
#'
#'This function generates a raster object of the zone-landuse-LUC data, writes a copy to file as a tiff, and writes a csv file of a look up table of the raster values to the combined class names 
#'
#'@param ZoneLanduseLUCPolygons A polygon spatial data file of the water management zone - land use - LUC intersection.
#'@param LeachRates A raster of leach rates. The output from the LeachRateRasterCreator function. Used to allign the output raster
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A raster of the zone-landuse-LUC combination
#'@keywords REC River Environment Classification CASM
#'@export


ZoneLanduseLUCRasterCreator <- function(ZoneLanduseLUCPolygons=SubZoneLanduseLUCSpatial,LeachRates=LeachRateRaster){
  
  #Add the combined-class name to the zone-landuse-LUC data
  ZoneLanduseLUCPolygons$CombinedClassName <- with(ZoneLanduseLUCPolygons@data,
                                                   paste0(Zone_Code,"-",RegiScLand,"-",LUC_CLASS))
  #Convert the combined name into levels
  ZoneLanduseLUCPolygons$CombinedClassNameFactor <- as.factor(ZoneLanduseLUCPolygons$CombinedClassName)
  #Create the raster, and allign to the Leach rate raster
  ZoneLanduseLUCRaster <- rasterize(ZoneLanduseLUCPolygons,LeachRates,"CombinedClassNameFactor")
  levels(ZoneLanduseLUCRaster) <- data.frame(ID=seq(1:3887),CombinedClassName=levels(ZoneLanduseLUCPolygons$CombinedClassNameFactor))
  #Save a copy for later
  writeRaster(ZoneLanduseLUCRaster,file.path(GISDataDirectory,ZoneLanduseLUCRasterFileName),overwrite=TRUE)
  #browser()
  #And save a levels-to-class name look up table
  write.table(levels(ZoneLanduseLUCRaster),file.path(DataDirectory,ZoneLanduseLUCCode_To_ClassLUTFileName),sep=",",quote=FALSE, row.names=FALSE)
  
  
  return(ZoneLanduseLUCRaster)
  
}

#' A function to  associate the leach rate with each zone-landuse-LUC combination and find the average leach rate and area for each combination.
#'
#'@param ZoneLanduseLUCRaster A raster of the water management zone - land use - LUC intersection.
#'@param LeachRates A raster of leach rates. The output from the LeachRateRasterCreator function.
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A data frame of average leach rates for each water management zone-landuse-LUC combination
#'@keywords REC River Environment Classification CASM
#'@export


#DiffuseLoadTableCreator <- function(ZoneLanduseLUCPolygons=SubZoneLanduseLUCSpatial,
#                                   LeachRates=LeachRateRaster){
DiffuseLoadTableCreator <- function(ZoneLanduseLUCRaster=ZoneLanduseLUCRaster,
                                    LeachRates=LeachRateRaster){
  
  # #Add the combined-class name to the zone-landuse-LUC data
  # ZoneLanduseLUCPolygons$CombinedClassName <- with(ZoneLanduseLUCPolygons@data,
  #                                                    paste0(Zone_Code,"-",RegiScLand,"-",LUC_CLASS))
  # #Convert the combined name into levels
  # ZoneLanduseLUCPolygons$CombinedClassNameFactor <- as.factor(ZoneLanduseLUCPolygons$CombinedClassName)
  # #Create the raster, and allign to the Leach rate raster
  # ZoneLanduseLUCRaster <- rasterize(ZoneLanduseLUCPolygons,LeachRates,"CombinedClassNameFactor")
  # levels(ZoneLanduseLUCRaster) <- data.frame(ID=seq(1:3887),CombinedClassName=levels(ZoneLanduseLUCPolygons$CombinedClassNameFactor))
  # #Save a copy for later
  # writeRaster(ZoneLanduseLUCRaster,file.path(GISDataDirectory,ZoneLanduseLUCRasterFileName),overwrite=TRUE)
  # browser()
  # #And save a levels-to-class name look up table
  # write.table(levels(ZoneLanduseLUCRaster),file.path(DataDirectory,ZoneLanduseLUCCode_To_ClassLUTFileName),sep=",",quote=FALSE, row.names=FALSE)
  
  #Now I can get all the leach rate values for all grid cells, and all the combined class names for all the cells
  RasterData <- data.frame(LeachRates = values(LeachRates), CombinedClassNameLevel = values(ZoneLanduseLUCRaster))
  RasterData <- RasterData[complete.cases(RasterData),]
  
  #I can then count the number of cells in each combined-class, and figure out the total area
  CellAreaHectares <- prod(res(LeachRates)) / 10000
  
  #Get the mean leach rate for each combined-class level
  #I don't need to get area weighted mean because all cells are the same size
  ClassSummaries <- ddply(RasterData,.(CombinedClassNameLevel), function(x) c(LeachRate =round(mean(x$LeachRates),1), count =  length(x$LeachRates), Hectares = length(x$LeachRates) * CellAreaHectares))
  
  ClassSummaries$CombinedClassName <- levels(ZoneLanduseLUCRaster)[[1]][ClassSummaries$CombinedClassNameLevel,2]
  return(ClassSummaries)
  
}

