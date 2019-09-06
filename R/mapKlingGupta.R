plotKGE <- function (data_all_loc, filelandmask, spec_dis = T, fyear = 0, 
                                                lyear = 0, typeout = "screen", continuous = F, thresnryrs = 10,
				        selmth = 0, sizecm = 9.0, filenamebase = "test_", ...) {

   library (hydroGOF)
   library (ncdf4)
   library (fields)
   library (RColorBrewer)
   library (mapdata)
   
   if (selmth > 0 & continuous) print (paste ("If the evaluation is done for a specific month, ",
      "you should consider not to require the time series to be continuous", sep = ""))
	  
   if (selmth == 0) {
      thresnrsam <- thresnryrs * 12
   } else {
      thresnrsam <- thresnryrs 
   }
      
   fac_sp_dis <- 24 * 3600 / 1000
   dpi <- 100
   type_stat <- c ("kge", "cc", "ratmean", "ratcv")
   mthname <- c ("Jan", "Feb", "Mar", "Apr", "May", "June", 
                                 "July", "Aug", "Sep", "Oct", "Nov", "Dec")
   
   nloc <- data_all_loc$nloc

   rivername <- data_all_loc$river_name																							  
   stationname <- data_all_loc$station_name																							  
   areaobs <- as.numeric (data_all_loc$area_observed)																								  
   areamod <- as.numeric (data_all_loc$area_model)																								  
   GRDCnr <- as.numeric (data_all_loc$GRDC_number)																								  
   latstat <- as.numeric (data_all_loc$latitude_station)																								  
   lonstat <- as.numeric (data_all_loc$longitude_station)																								  
   altitude <- as.numeric (data_all_loc$altitude)

   # select elements within the chosen time period
   timeall <- data_all_loc$time 
   ntimeall <- data_all_loc$ntime
   fyearall <- strtoi (substr (timeall[1], 1, 4))
   lyearall <- strtoi (substr (timeall[ntimeall], 1, 4))
  
   if (fyear == 0) {
      fyear_per <- fyearall
   } else {
      fyear_per <- fyear
   }	  

   if (fyear == 0) {
      lyear_per <- lyearall
   } else {
      lyear_per <- lyear
   }

   if (fyear_per > lyear_per) stop (paste ("First year (", fyear_per, 
      ") is later than last year (", lyear_per, ")", sep = "" ))

   fday <- paste (fyear_per, "-01-01", sep = "")
   lday <- paste (lyear_per, "-12-31", sep = "")
   fday_jul <- as.numeric (as.Date (fday)) 
   lday_jul <- as.numeric (as.Date (lday))
   alldays <- lday_jul - fday_jul + 1
 
   time_jul <- as.numeric (as.Date (timeall))
   ind_per <- which (time_jul >= fday_jul & time_jul <= lday_jul)
   
   obs_per <- data_all_loc$obs [ ,ind_per]
   sim_per <- data_all_loc$sim [ , 1, ind_per]
   time_per  <- timeall[ind_per]

   if (selmth < 10) {
      strselmth <- paste ("0", toString(selmth), sep = "")
   } else {
      strselmth <- toString(selmth)
   }

   # Select specific months if desired
   if (selmth != 0) {
      time_per_mth <- substr (time_per, 6, 7)
      indmth <- which (time_per_mth == strselmth)
      obs_ana <- obs_per[ ,indmth]
      sim_ana <- sim_per[ , indmth]
      time_ana <- time_per[indmth]
   } else {
      obs_ana <- obs_per
      sim_ana <- sim_per
      time_ana <- time_per
   }
   
   kge <- vector (mode = "double", length = nloc)
   cc <- vector (mode = "double", length = nloc)
   ratmean <- vector (mode = "double", length = nloc)
   ratcv <- vector (mode = "double", length = nloc)
   statsel <- vector (mode = "logical", length = nloc)
   kge[] <- NA
   cc[] <- NA
   ratmean[] <- NA
   ratcv[] <- NA
   statsel[] <- F
   
   for (iloc in (1:nloc)) {

      obs_stat_ana  <- obs_ana[iloc, ] 
      sim_stat_ana  <- sim_ana[iloc, ]
 
      # Select only data pairs for which the observation is not missing 
      indvalall <- which (!is.na (obs_stat_ana))
      # And select the longest period with a continuous time series
      if (continuous) {
            nvalall <- length (indvalall)
	  indvallast <- indvalall[nvalall]
	  indnanall <- which (is.na (obs_stat_ana))
	  indend <- 0
	  maxnumval <- 0
	  while (indend != indvallast) {
	     ind <- which (indvalall > indend)
               indbeg <- indvalall[ind[1]]
               ind <- which (indnanall > indbeg)			   
               if (length (ind) > 0) {
	        indend <- indnanall[ind[1]] - 1
	     } else {
	        indend <- indvallast
	     }
	     numval <- indend - indbeg + 1
	     if (numval > maxnumval) {
		 maxnumval <- numval
		 begbestper <- indbeg
		 endbestper <- indend
	     }
	  }
	  indsel <- seq (begbestper, endbestper, length.out = maxnumval)
      } else {
	  indsel <- indvalall
      }
	  
      nsel <- length (indsel)
      if (nsel < thresnrsam) next
      statsel[iloc] <- T
	  	  
     #if (nval < min_pr) next
      obs_stat_val <- obs_stat_ana[indsel]
      sim_stat_val <- sim_stat_ana[indsel]
	  
      # Compute specific discharge
      if (spec_dis) {
         obs_stat_val <- obs_stat_val / areaobs[iloc] * fac_sp_dis	  
         sim_stat_val <- sim_stat_val / areamod[iloc] * fac_sp_dis
      }

      kge_obj <- KGE(sim_stat_val, obs_stat_val, s=c(1,1,1), 
	  method = "2012", out.type="full")
      kge[iloc] <- kge_obj$KGE.value
      cc[iloc] <- kge_obj$KGE.elements[1]
      ratmean[iloc] <- kge_obj$KGE.elements[2]
      ratcv[iloc] <- kge_obj$KGE.elements[3]
	  
   }
   
   nclm <- nc_open (filelandmask)
   lat <- ncvar_get(nclm, varid = "lat")
   lon <- ncvar_get(nclm, varid = "lon")
   landmask0 <- ncvar_get(nclm, varid = "LandMask")
   epslm <- 0.001
   nc_close(nclm)
   
   lonmin <- min (lon)
   lonmax <- max (lon)
   latmin <- min (lat)
   latmax <- max (lat)
   latmean <- mean (lat)
   disy <- latmax - latmin
   disx <- (lonmax - lonmin) * cos (latmean / 180.0 * pi)
   ratsize <- disy / disx
   sizeinch <- sizecm / 2.54
   widthimage <- sizeinch / sqrt(ratsize)   
   heightimage <- sizeinch * sqrt(ratsize)

   # Margins at the left, right, bottom and top side op the map in inch
   marbig <- c (0.35, 0.7, 0.35, 0.35)
   # Distance from the right hand side of the plot of the left and the
   #    right side of the color bar   
   distsmall <- c (0.54, 0.41)
   col_lm <- rgb(0, 0, 0, maxColorValue=255, alpha=40) 
   col_stat <- vector (mode = "character", length = nloc)
   col_stat[] <- NA
   
   #par (mfrow=c(2,2))

   for (type_stat_here in type_stat) {
   
      if (type_stat_here == "kge" | type_stat_here == "cc") {
         nintval <- 5
         colvec <- c("black", brewer.pal(nintval, "YlOrRd"), col_lm)
         lowb <- 0
         highb <- 1
         dintval <- (highb - lowb) / nintval
         breaks_stat <- c ((lowb - dintval), seq (lowb, highb, length.out = (nintval+1)), highb + epslm) 
         breaks_leg <- breaks_stat		 
         landmask <- landmask0 * (highb + 0.5 * epslm)
      } else if (type_stat_here == "ratmean" | type_stat_here == "ratcv") {
         nintval <- 7
         ncolpal <- ceiling ((nintval / 2))
         colvec <- c("black", brewer.pal(ncolpal, "YlOrRd"),
           		        rev(brewer.pal(ncolpal, "PuRd"))[2:ncolpal], "blue", col_lm)
         rat1 <- 2
         lowb <- log(1/rat1)
         highb <- log(rat1)
         dintval <- (highb - lowb) / nintval
         breaks_stat <- c (lowb - dintval, seq (lowb, highb, length.out = (nintval+1)),
            		                    highb + dintval, highb + dintval + epslm)   
         breaks_leg <- exp(breaks_stat)		 
         landmask <- landmask0 * (highb + dintval + 0.5 * epslm)
      }
   
      if (type_stat_here == "kge") {
         stathere <- kge
         titlestat <- "Kling-Gupta efficiency"
      } else if (type_stat_here == "cc") {
         stathere <- cc
         titlestat <- "correlation coefficient"
      } else if (type_stat_here == "ratmean") {
         stathere <- log(ratmean)
         titlestat <- "ratio of the means"
      } else if (type_stat_here == "ratcv") {
         stathere <- log(ratcv)
         titlestat <- "ratio of the coefficient of variation"
      }
	  
      if (selmth == 0) {
          titlemth <- " - all mths"
      } else {
          titlemth <- paste (" - ", mthname[selmth], sep = "")
      }
	  
      titleall <- paste (titlestat, titlemth, sep = "")
   
      ncol <- length (colvec) 
      ncolstat <- ncol - 1
      nbreaks <- length (breaks_stat)
      if (ncol != (nbreaks - 1)) stop ("the number of colors does not match with the number of breaks")	  

      widthwin <- widthimage + marbig[1] + marbig[2]
      heightwin <- heightimage + marbig[3] + marbig[4]
      relbig <- c (marbig[1]/widthwin, 1-marbig[2]/widthwin,  
	                        marbig[3]/heightwin, 1-marbig[4]/heightwin)
      relsmall <- c (1-distsmall[1]/widthwin, 1-distsmall[2]/widthwin, 0.15, 0.8)

      if (typeout == "screen") {
         win.graph (width = widthwin, height = heightwin)
      } else if (typeout == "pdf"){
         pdfname <- paste (filenamebase, type_stat_here, "_", strselmth, ".pdf", sep = "") 
         pdf (file = pdfname ,width = widthwin, height = heightwin)
      } else if (typeout == "png"){
         pngname <- paste (filenamebase, type_stat_here, "_", strselmth, ".png", sep = "")
         png (file = pngname ,width = widthwin, height = heightwin, units = "in", res = 720)
      } else {
         stop ("typeout must be screen, pdf or png" )
      } 
	  
      image.plot (lon, lat, landmask, col = colvec, breaks = breaks_stat,
                                  xlab = "", ylab = "", 
			    smallplot = relsmall,
			    lab.breaks = c (" ", format (breaks_leg[2:(nbreaks-2)], digits = 2), " ", " "),
			    bigplot = relbig, cex.axis=0.6, mgp = c(2, 0.15, 0), tck = -0.02,
			    axis.args = list(cex.axis = 0.6, tck = -0.3, mgp = c(0, 0.4, 0)))

      map ("world", add = T)
      map("rivers",  col = "blue", lwd = 2, add = T)
				
      title (titleall, line = 0.4, cex.main = 0.6)
				
      for (iloc in (1:nloc)) {
	if (!statsel[iloc] | is.na(stathere[iloc])) next
         icol <- ceiling((stathere[iloc] - lowb) / dintval)+ 1
         if (icol < 1) icol <- 1
         if (icol > ncolstat) icol <- ncolstat
         col_stat[iloc] <- colvec[icol]
      }
	  
      indstatsel <- which (statsel)
         
      points (lonstat[indstatsel], latstat[indstatsel], col = col_stat[indstatsel], 
	               lwd = 1.0, cex = 0.6)
	  
      if (typeout == "screen") {
         print ("Enter something to continue")
         entval <- scan (file = "", what = "", nmax = 1) 
     }
	 
      dev.off ()
	 
   }   # End of the loop over the type of statistics

}   # End of plotKGE   
   
 

   





