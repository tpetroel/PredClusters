#' Isle Royale Wolf GPS Cluster Identification
#'
#' This function formats GPS data from Telonics and Vectoronic Collars used in Isle Royale National Park and then identifies potential predation sites ("clusters") from the formated GPS locations and user defined thresholds.
#' @param DownloadDate The date and folder title in which the raw GPS data are housed, should be formated as DDMONYYYY (i.e., 14MAY2019)
#' @param CollarID The individual identifier for each collar preceeded by "Collar" (i.e., Collar32712)
#' @param AnimalID The individual identifier for each wolf (i.e., W001F) indicating the wolf number and sex
#' @param NextCluster The next number to be assinged for cluster labeling for independent identification, reference Download_Dates.xlsx
#' @param ClusterDate The oldest date to be used for the period to identify clusters within (format = "YYYY-MM-DD", i.e., "2019-05-14")
#' @param time_thres Time, measured in days, between GPS locations to be considered as part of a cluster (i.e., 1 = 24 hours)
#' @param dist_thres Distance, measured in kilometers, between GPS points to be considered in a cluster (i.e., 0.05 = 50 meters)
#' @keywords Wolf Predation Clusters GPS Isle Royale
#' @examples
#' # Identify clusters formed by GPS locations that occur within 1 day of eachother
#' # and within 50 meters of eachother.
#'
#' IRwolves <- function(DownloadDate = "14MAY2019", CollarID = "Collar32712", AnimalID = "W001F", NextCluster = 001, ClusterDate = "2019-05-07", time_thres = 1, dist_thres = 0.05)

IRwolves = function(DownloadDate = DownloadDate, CollarID = CollarID, AnimalID = AnimalID,
                     NextCluster = NextCluster, ClusterDate = ClusterDate, time_thres = time_thres, dist_thres = dist_thres){

  setwd(paste("C:/IR_Clusters/Downloads/",DownloadDate,sep=''))

  # IF/ELSE STATEMENT FOR VECTRONIC VS. TELONICS ----------------------------
  if (as.numeric(str_sub(CollarID,7))>678910){

    ###IF TELONICS
    raw.data = data.frame(read.csv(paste("GPS_",CollarID,"_",AnimalID,".csv",sep=""),skip=23,header=T)) # Load the file name with the raw GPS data
    str(raw.data)

    raw.data$index <- as.numeric(row.names(raw.data))
    raw.data <- raw.data[order(raw.data$index), ]

    raw.data = subset(raw.data,GPS.Fix.Attempt=='Succeeded')
    raw.data$line <- 1:nrow(raw.data)

    clean.data = (data.frame(line=raw.data$line,Date=raw.data$GPS.Fix.Time,date=raw.data$GPS.Fix.Time,
                             Time=raw.data$GPS.Fix.Time,lat=raw.data$GPS.Latitude,long=raw.data$GPS.Longitude,activity=1,
                             GPS.DT=raw.data$GPS.Fix.Time,ACT.DT=raw.data$GPS.Fix.Time))

    clean.data$Date<-as.Date(clean.data$Date,format='%Y.%m.%d %H:%M:%S')
    clean.data$date<-as.Date(clean.data$Date,format='%Y.%m.%d %H:%M:%S')
    clean.data$GPS.DT<-strptime(clean.data$Time,format='%Y.%m.%d %H:%M:%S')
    clean.data$ACT.DT<-strptime(clean.data$Time,format='%Y.%m.%d %H:%M:%S')

    clean.data$Time<-strptime(clean.data$Time,format='%Y.%m.%d %H:%M:%S')
    clean.data$Time<-strftime(clean.data$Time,format='%H:%M:%S')
    clean.data$Time<-times(as.character(clean.data$Time))

    clean.data$GPS.DT<-as.POSIXct(clean.data$GPS.DT)
    clean.data$ACT.DT<-as.POSIXct(clean.data$ACT.DT)

    clean.data<-clean.data[rev(order(as.Date(clean.data$Date))),]
    # This formats date to work with the "find_cluster" function
    data = subset(clean.data,as.Date(date) >= ClusterDate & as.Date(date) < Sys.Date())
    data$line <- as.numeric(row.names(data))
    data <- data[order(data$line), ]

  } else {
    #####IF VECTRONIC
    raw.data = data.frame(read.csv(paste('GPS_',CollarID,'_',AnimalID,'.csv',sep=""))) # Load the file name with the raw GPS data
    #raw.act = data.frame(read.csv(paste("ACT_",CollarID,"_",DownloadDate,".csv",sep=""))) # Load the file name with the raw Activity data
    #raw.act$sum.act = raw.act$ActivityX + raw.act$ActivityY # Sum X and Y Activity into one column
    raw.data$GPS.dt = paste(raw.data$LMT_Date, raw.data$LMT_Time)
    #raw.act$act.dt = paste(raw.act$LMT_Date, raw.act$LMT_Time)
    raw.data$GPS.dt<-as.POSIXct(raw.data$GPS.dt,format="%m/%d/%Y %H:%M:%S")
    #raw.act$act.dt<-as.POSIXct(raw.act$act.dt,format="%m/%d/%Y %H:%M:%S")
    #raw.data$GPS.dt = as.POSIXct(paste(raw.data$LMT_Date, raw.data$LMT_Time),format="%Y-%m-%d %H:%M:%S")
    #raw.act$act.dt = as.POSIXct(paste(raw.act$LMT_Date, raw.act$LMT_Time),format="%Y-%m-%d %H:%M:%S")
    #raw.merge = cbind(raw.data, raw.act[ sapply(raw.data$GPS.dt, function(x) which.min(abs(difftime(x, raw.act$act.dt)))), ])
    clean.data = na.omit(data.frame(line=raw.data$No,Date=raw.data$LMT_Date,date=raw.data$LMT_Date,
                                    Time=raw.data$LMT_Time,lat=raw.data$Latitude,long=raw.data$Longitude,activity=1,
                                    GPS.DT=raw.data$GPS.dt,ACT.DT=raw.data$GPS.dt))
    # Cleans up missing data only keeps specified columns
    clean.data$Date = as.character(clean.data$Date)
    clean.data$Date = as.Date(clean.data$Date,format="%m/%d/%Y")
    clean.data$date = as.character(clean.data$date)
    clean.data$date = as.Date(clean.data$date,format="%m/%d/%Y")
    clean.data$Time <- format(strptime(clean.data$Time, "%I:%M:%S %p"), "%H:%M:%S")
    #clean.data$Time <- times(clean.data$Time)
    # This formats date to work with the "find_cluster" function
    data = subset(clean.data,as.Date(date) >= ClusterDate & as.Date(date) < Sys.Date())
  }
  # EXPORT PRIMARY GPS FILE ------------------------------

  setwd(paste("C:/IR_Clusters/Wolf_GPS_Data/",AnimalID,sep=''))
  data$AnimalID = AnimalID
  write.csv(data,paste(AnimalID,CollarID,DownloadDate,'R.csv',sep="_"))
  data$PID = 1
  data$POS = data$line

  ## function 'as.PolySet' builds PolySet-class from data.frame for 'convUL' to convert latlong to UTM
  data.XY = data.frame(POS=data$POS,PID=data$PID,X=data$long,Y=data$lat) # "X"=longitude "Y"=latitude in PBSmapping
  data.LL = as.PolySet(data.XY, projection = "LL", zone = 16) # Change LongLat to Poly Set
  data.UTM = convUL(data.LL,km = FALSE) # Convert LongLat to UTM
  data.locs = data.UTM[, c("X", "Y")] # Extract "Easting"& "Northing"
  data = cbind(data,data.locs)
  data = subset(data, select = -c(GPS.DT, ACT.DT))

  setwd(paste("C:/IR_Clusters/Downloads/",DownloadDate,sep=""))
  write.table(data,paste("ALL_WOLF_GPS_DATA",DownloadDate,".txt",sep = ""),sep="\t", dec=".",quote=FALSE,row.name=FALSE,append=T) # Output of cluster locations/date/time

  b<-read.table(paste("ALL_WOLF_GPS_DATA",DownloadDate,".txt",sep = ""),fill=T,na.strings="NA",header=T)

  b<-b[!b$AnimalID =="AnimalID",]

  b$X <-as.character(b$X)
  b$Y <-as.character(b$Y)
  b$X <-as.numeric(b$X)
  b$Y <-as.numeric(b$Y)

  b$id = 1:nrow(b)

  write.table(b,paste("ALL_WOLF_GPS_DATA",DownloadDate,".txt",sep = ""),sep="\t", dec=".",quote=FALSE,row.name=FALSE,append=F) # Output of cluster locations/date/time

  ST = data.frame(Id = b$id, Easting = b$X, Northing = b$Y)
  AT = data.frame(AnimalID = b$AnimalID, Line = b$line, Date = b$Date,
                  Time = b$Time, Id = b$id)

  class(ST$Id)
  class(AT$Id)

  shpfl = convert.to.shapefile(shpTable = ST, attTable = AT, field = "Id",type = 1)

  class(shpfl)
  write.shapefile(shpfl, paste("ALL_WOLF_GPS_DATA",DownloadDate,sep = ""),arcgis = TRUE) # Output of shape file for ArcGIS mapping of cluster locations

  ## RUN FUNCTION WITH DEFINED PARAMETERS ----------------------------------------------------------------------------------------------------------
  setwd(paste("C:/IR_Clusters/Wolf_GPS_Data/",AnimalID,'/Tables_Maps',sep=''))

  ## Function command line, "time_thres" is days between first/last point (i.e., 1=24hr),
  # "dist_thres" is km boundry to include points (0.05 = 50m),
  # "size" refers to text size on cluster map
  res1 = find_cluster(ind = AnimalID, data = data, time_thres = time_thres, dist_thres = dist_thres, size = 2)
  res = read.table("res.txt" , header=T, sep="\t", dec=".")
  centr = read.table("centr.txt", header=T, sep="\t", dec=".")
  act.clus = subset(res, cluster >0)
  act.clus = aggregate(activity ~ cluster, data=act.clus,FUN=sum)
  res$PID = 1
  centr$PID = 1
  res$POS = res$line
  centr$POS = centr$cluster

  ## function 'as.PolySet' builds PolySet-class from data.frame for 'convUL' to convert latlong to UTM
  res.XY = data.frame(POS=res$POS,PID=res$PID,X=res$long,Y=res$lat) # "X"=longitude "Y"=latitude in PBSmapping
  centr.XY = data.frame(POS=centr$POS,PID=centr$PID,X=centr$long,Y=centr$lat)

  res.LL = as.PolySet(res.XY, projection = "LL", zone = 16) # Change LongLat to Poly Set
  res.UTM = convUL(res.LL,km = FALSE) # Convert LongLat to UTM
  res.locs = res.UTM[, c("X", "Y")] # Extract "Easting"& "Northing"
  res = cbind(res,res.locs)

  centr.LL = as.PolySet(centr.XY, projection = "LL", zone = 16) # Change LongLat to Poly Set
  centr.UTM = convUL(centr.LL,km = FALSE) # Convert LongLat to UTM
  centr.locs = centr.UTM[, c("X", "Y")] # Extract "Easting"& "Northing"
  clus.centr = cbind(centr,centr.locs,act.clus)
  clus.centr$avg.act = round(clus.centr$activity/clus.centr$n, digits = 1)
  clus.centrs = subset(clus.centr, n >= 2) # Takes a subset of all defined clusters with 5 or more locations

  centrs = data.frame(cluster_ID=clus.centrs$cluster, n=clus.centrs$n, activity=clus.centrs$activity,
                      avg_act=clus.centrs$avg.act, Date_i=clus.centrs$Date_i, Time_i=clus.centrs$Time_i,
                      Date_f=clus.centrs$Date_f, Time_f=clus.centrs$Time_f, Easting=clus.centrs$X,Northing=clus.centrs$Y)
  centrs[c("Initials","Date_Completed")] = " "
  centrs[c("Animal_ID")] = AnimalID
  centrs[c("Cluster_Num")] = seq(from = as.numeric(NextCluster), to = nrow(centrs)+ as.numeric(NextCluster) -1, by = 1)
  centrs[c("Correct_Num")] = sprintf("%03d", centrs$Cluster_Num)
  centrs[c("Cluster_ID")] = paste(centrs$Animal_ID, centrs$Correct_Num, sep = "_")

  ## This adds columns used for cluster tables. Initials and Date Completed are filled out when visiting a cluster.
  ## Animal_ID is changed each time for the corresponding animal for this collar data
  ## Cluster_Num is generating a sequence of numbers starting with the specified cluster number from the Download Dates file. This should be changed each time it is run.
  ## The from number will be the first cluster in the sequence
  ## The to number is the last number in this cluster sequence, calculate this by looking in the Global Environment at "centrs". The number of obs. is the number of clusters generated with this download.
  ## For example from= 125, to = 247, by 1, means there are 123 obs starting at cluster 125, going to 247, by increments of 1
  ## Correct_Num is correcting the number of digits, i.e. adding zeros, in the cluster number
  ## Cluster_ID is combining the column with Animal_ID and Corrected_Num to get the correct Cluster_ID for each cluster generated

  centrs_clean = data.frame(Animal_ID=centrs$Animal_ID, Cluster_ID=centrs$Cluster_ID, n=clus.centrs$n, activity=clus.centrs$activity, avg_act=clus.centrs$avg.act, Date_i=centrs$Date_i, Time_i=centrs$Time_i,
                            Date_f=centrs$Date_f, Time_f=centrs$Time_f, Easting=clus.centrs$X,Northing=clus.centrs$Y, Initials=centrs$Initials, Date_Completed=centrs$Date_Completed)
  ## This creates a new data frame, "centrs_clean", with only the columns needed for cluster tables that is used below to write into a text file

  centrs_clean$Easting <- round(centrs$Easting,0)
  centrs_clean$Northing <- round(centrs$Northing,0)

  ## CLUSTER KILL SITE PREDICTION-----------------------------------------------------------------------------------------------------------

  # Load fitted models for each carnivore species (BB.fm, BC.fm, CO.fm, WO.fm)

  ## Scale cluster covariates to match data from fitted models
  #newdat <- as.data.frame((centrs_clean$n - 22.2748815) / 22.6240699)
  #colnames(newdat) <- c("LOCS")
  #newdat$MEANACT <- (centrs_clean$avg_act - 23.8128815) / 28.5886211
  #newdat$SUMACT <- (centrs_clean$activity - 381.2426540) / 237.5869941

  ## Predict which clusters are likely kill sites from models built with Phase 2 data
  ## Does not apply to IR wolves, disregard errors here

  #pred <- data.frame(predict(model, newdata=newdat, ## CHANGE MODEL HERE ACCORDING TO SPECIES USED (BB.fm, BC.fm, CO.fm, WO.fm)
  #                           type = "response"))



  #colnames(pred) <- c("logodds") # Prediction for kill sites in logodds
  #pred$prob <- exp(pred[,1])/(1+exp(pred[,1])) # Covert the cut-of value out of logodds
  ## Cut-off Values --
  #BBX <- 0.5051975
  #BCX <- 0.5462662
  #COX <- 0.5172079
  #WOX <- 0.5269982

  #for(i in 1:nrow(pred))
  # if(pred$prob[i] > paste(substring(AnimalID,1,2),"X",sep="")) { ## CHANGE TO THE CORRECT SPECIES CUTOFF
  #    pred$Predicted[i] = "YES" # yes, the cluster is predicted as a kill
  # } else {
  #   pred$Predicted[i] = "NO" # no, the cluster is likely not a kill
  # }



  ## OUTPUT OF CLUSTERS TABLES/MAPS/ETC TO FILES---------------------------------------------------------------------------------------------------------
  drops<-c("activity","avg_act")

  centrs_clean <- centrs_clean[,!names(centrs_clean) %in% drops]
  write.table(res,paste(AnimalID,"_",DownloadDate,"_res",".txt",sep = ""), sep="\t", dec=".",quote=FALSE,row.name=FALSE) # Output of all calculated clusters from function "find_Cluster"
  write.table(centrs_clean,paste(AnimalID,"_",DownloadDate,"_center",".txt",sep = ""), sep="\t", dec=".",quote=FALSE,row.name=FALSE) # Output of cluster locations/date/time

  setwd(paste("C:/IR_Clusters/Downloads/",DownloadDate,sep=""))
  write.table(centrs_clean,paste("ALL_CLUSTERS",DownloadDate,"_center",".txt",sep = ""),sep="\t", dec=".",quote=FALSE,row.name=FALSE,append=T) # Output of cluster locations/date/time

  a<-read.table(paste("ALL_CLUSTERS",DownloadDate,"_center",".txt",sep = ""),fill=T,na.strings="NA",header=T)
  a<-a[!a$Animal_ID=="Animal_ID",]

  a$Easting<-as.character(a$Easting)
  a$Northing<-as.character(a$Northing)
  a$Easting<-as.numeric(a$Easting)
  a$Northing<-as.numeric(a$Northing)

  write.table(a,paste("ALL_CLUSTERS",DownloadDate,"_center",".txt",sep = ""),sep="\t", dec=".",quote=FALSE,row.name=FALSE,append=F) # Output of cluster locations/date/time


  shpTable = data.frame(Id = a$Cluster_ID, Easting = a$Easting, Northing = a$Northing)
  attTable = data.frame(Animal = a$Animal_ID, Id = a$Cluster_ID, N = a$n, Date_Started = a$Date_i,
                        Time_Started = a$Time_i, Date_Finished = a$Date_f,
                        Time_Finished = a$Time_f, Cluster_ID = a$Cluster_ID)

  class(shpTable$Id)
  class(attTable$Id)

  shp.file = convert.to.shapefile(shpTable = shpTable, attTable = attTable, field = "Id",type = 1)

  class(shp.file)
  write.shapefile(shp.file, paste("ALL_CLUSTERS",DownloadDate,"_center",sep = ""),arcgis = TRUE) # Output of shape file for ArcGIS mapping of cluster locations


  ## -------------------------------------------

  setwd(paste("C:/IR_Clusters/Wolf_GPS_Data/",AnimalID,'/Shapefiles',sep=''))

  shpTable = data.frame(Id = centrs$cluster_ID, Easting = centrs$Easting, Northing = centrs$Northing)
  attTable = data.frame(Id = centrs$cluster_ID, N = centrs$n, Date_Started = centrs$Date_i,
                        Time_Started = centrs$Time_i, Date_Finished = centrs$Date_f,
                        Time_Finished = centrs$Time_f, Cluster_ID = centrs_clean$Cluster_ID)

  shp.file = convert.to.shapefile(shpTable = shpTable, attTable = attTable, field = "Id",type = 1)

  setwd(paste("C:/IR_Clusters/Wolf_GPS_Data/",AnimalID,"/Shapefiles",sep=''))
  write.shapefile(shp.file, paste(AnimalID,"_",DownloadDate,"_center",sep = ""), arcgis = TRUE) # Output of shape file for ArcGIS mapping of cluster locations

}

