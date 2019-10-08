#' GPS Cluster Identification
#'
#' This function identifies potential predation sites ("clusters") by using GPS locations from GPS collared carnivores and user defined thresholds.
#' @param ind Text for title of figure of cluster locations produced by 'find_cluster'. Defaults to "Cluster Locations"
#' @param data A data.frame that contains 'date', 'Time', 'lat', and 'long' columns
#' @param time_thres Time, measured in days, between GPS locations to be considered as part of a cluster (i.e., 1 = 24 hours)
#' @param dist_thres Distance, measured in kilometers, between GPS points to be considered in a cluster (i.e., 0.05 = 50 meters)
#' @keywords Carnivore Predation Clusters GPS
#' @examples
#' # Identify clusters of GPS locations that occur within 1 day of eachother
#' # and within 100 meters of eachother.
#'
#' find_cluster(ind = "Cluster Locations", data = data, time_thres = 1, dist_thres = 0.1, size = 1)

find_cluster = function(ind = "Cluster Locations", data = data, time_thres = 1, dist_thres = 0.05, size = 1) {
  clust = function( ind = "Cluster Locations", data = data, time_thres = 1, dist_thres = 0.05 ) {
    d = data
    n = length(d[,1])
    data = cbind(data, time = rep(NA, n), cluster = rep(0, n))
    cluster_nb = 1
    #---
    code  = as.character(d$Time) ;
    for(i in 1 : n) {
      if(substr(code[i], 2, 2) == ":") code[i] = paste("0", code[i], sep = "")
    }
    hour = substr(code, 1, 2) ; hour  = as.numeric(hour)
    min = substr(code, 4, 5); min = as.numeric(min)
    sec = substr(code, 7, 8); sec = as.numeric(sec)
    for(i in 1 : n) d$time[i] = ( hour[i]*60 + min[i] + sec[i] / 60 ) / ( 24*60 )
    data$time = d$time
    #---
    code  = as.character(d$date)
    year = substr(code, 1, 4); year = as.numeric(year)
    month = substr(code, 6, 7); month = as.numeric(month)
    day = substr(code, 9, 10); day = as.numeric(day)
    days = unclass(as.Date(d$date)) - unclass(as.Date(d$date[1]))
    hours = d$time ; t = days + hours
    d = cbind(d, t, belong_clust = rep(0, n))
    d = subset(d, select = c(line, lat, long, t, activity, belong_clust))
    #---
    calc_seed = function(data = data, d = d) {
      # select one candidate (if any), fill in the cluster variable for the focal + candidate,
      # recalculate d (d = d, not data)
      foc = d[1,]
      cand = d[-1,]
      dt = abs(cand$t - foc$t)
      xfoc = foc$long ; yfoc = foc$lat ; pos_foc = cbind(xfoc, yfoc)
      xcand = cand$long; ycand = cand$lat; pos_cand  = cbind(xcand, ycand)
      dx = as.numeric(rdist.earth(pos_foc, pos_cand, miles = F))
      #-
      whole_selec = cand[dt < time_thres & dx < dist_thres,]
      if(length(whole_selec[,1]) >= 1) cand_selected = whole_selec[1,] else{
        cand_selected = cand ; for(k in 1 : length(d)) cand_selected [k] = NA }
      #
      ifelse(is.na(cand_selected$line), t <- 0, t <- 1)
      if(t == 0 & length(d[,1]) == 2) { end_foc = TRUE ; end_d = TRUE }
      if(t == 0 & length(d[,1]) > 2) {d = d[-1,] ; cluster_nb  = cluster_nb + 1 ; end_foc = TRUE }
      if(t == 1) {
        end_foc = FALSE
        if(length(d[,1]) == 2) { end_foc = TRUE ; end_d = TRUE }
        # calculate (the average) seed
        nbvar = length(d); seed = rep(NA, nbvar)
        for(k in 1 : nbvar) seed[k] = mean(c(as.numeric(foc[k]), as.numeric(cand_selected[k])))
        seed[5] = max(c(as.numeric(foc[5]), as.numeric(cand_selected[5])))
        seed[7] = 1
        #
        j = which(data$line == cand_selected$line)
        if(foc$belong_clust == 0) {
          i = which(data$line == foc$line)
          data$cluster[i] = data$cluster[j] = cluster_nb
        } else {
          data$cluster[j] = cluster_nb
        } # if(foc$belong_clust == 0)
        jd = which(d$line == cand_selected$line)
        d = d[-c(1,jd),] ; d = rbind(seed, d)
      } # if(is.na(cand_selected$line))
      #-
      return(list(data = data, d = d, cluster_nb = cluster_nb, end_foc = end_foc, end_d = end_d))
    }
    #------
    #---
    end_d = FALSE
    for(i in 1 : n) {
      if(end_d == FALSE) {
        end_foc = FALSE
        while(end_foc == FALSE) {
          step = calc_seed(data = data, d = d)
          #
          data = step$data
          d = step$d
          cluster_nb = step$cluster_nb
          end_foc = step$end_foc
          end_d = step$end_d
        }
      } else break
    }# for(i in 1 : n)
    #---
    data = cbind(data, t)
    return(data)
  }
  #----------
  sort_cluster = function(res = res1) {
    sort.res = res[order(res$cluster), ]
    s=sort.res$cluster; sb=rep(NA, length(s))
    first_diff_0 = which(s!=0)[1]
    sb[first_diff_0] = 1
    for(i in (first_diff_0 + 1) : length(s)){
      if(s[i]==s[i-1])  sb[i]<-sb[i-1] else sb[i]<-sb[i-1] +1
    }
    sort.res$cluster = sb
    #---
    return(sort.res)
  }
  #----------
  centroid_cluster = function(sort.res = sort.res1) {
    clust_only = sort.res[!is.na(sort.res$cluster),]
    nr = length(unique(clust_only$cluster))
    mat = as.data.frame(matrix(NA, length(data[,1]), 8))
    rownames(mat) = 1:length(data[,1]); colnames(mat) = c("cluster", "lat", "long", "n", "Date_i", "Time_i", "Date_f", "Time_f")
    mat$Date_i = mat$Date_f = data$Date
    mat$Time_i = mat$Time_f = data$Time
    mat = mat[1:nr,]
    for(i in 1 : nr) {
      cluster_i = clust_only[clust_only$cluster == i,]
      mat$cluster[i] = i
      mat$lat[i] = mean(cluster_i$lat)
      mat$long[i] = mean(cluster_i$long)
      mat$n[i] = n = length(cluster_i$long)
      mat$Date_i[i] = cluster_i$Date[1]
      mat$Date_f[i] = cluster_i$Date[n]
      mat$Time_i[i] = cluster_i$Time[1]
      mat$Time_f[i] = cluster_i$Time[n]
    }
    return(mat)
  }
  #----------
  res = clust( ind = ind, data = data, time_thres = time_thres, dist_thres = dist_thres)
  write.table(res, file = "res.txt", quote = FALSE, sep="\t", dec=".",  row.names=FALSE)
  sort.res = sort_cluster(res = res)
  centr.res = centroid_cluster(sort.res = sort.res)
  write.table(centr.res, file = "centr.txt", quote = FALSE, sep="\t", dec=".",  row.names=FALSE)
  #---
  n = length(data[,1])
  pch = rep(19, n)
  col = res$cluster
  for(i in 1 : length(col)) { if(col[i] == 0) { col[i] = "black" ; pch[i] = 21 }}
  cex = rep(1, n) ; cex[n] = 2.5
  plot(res$long, res$lat, type = "n", xlab = "long", ylab = "lat", main = ind)
  points(res$long, res$lat, pch = pch, col = col, cex = cex)
  lines(res$long, res$lat)
  # text(res$long, res$lat, labels = res$t, cex = 0.7)
  text(centr.res$long, centr.res$lat, labels = centr.res$cluster, cex = size)
  #---
  return(list(
    res = res,
    centr = centr.res
  )
  )
}
