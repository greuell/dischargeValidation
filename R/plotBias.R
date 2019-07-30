#' Make scatter plot of temporal mean simulated versus observed discharge.
#'
#' @param data_all_loc Object with the data (observations, simulations and meta data) obtained by a call with getData.
#' @param perc_pr Minimum percentage of all time slots during the period from fyear to lyear with both a valid observation AND a valid simulation. i.e. with a valid data pair. Stations with less than perc_pr valid data pairs are not plotted. Mean values of observations and simulations will be computed for all time slots with a valid data pair. Default: 90.
#' @param log_plot Plot with logarithmic (T) or linear (F) axes. Default: linear axes (F).
#' @param power_symb This parameter affects the relative size of the symbols in the plot. The area of the circles representing individual stations are proportional to the area of the catchment upstream from the station (according to the meta data of the observations) to the power power_symb. Default: 0.25.
#' @param spec_dis Plot specific (T) or absolute amount (F) of discharge. Specific discharge (in mm/d) is equal to absolute amount of discharge (in m3/s) divided by the area of the catchment upstream from the station (for the observations) or area of the catchment according to the routing network (for simulations; this includes the area of the grid cell in which the station is located).
#' @param fyear First year of the period considered. Default: first year of data_all_loc.
#' @param lyear Last year of the period considered. Default: last year of data_all_loc.
#' @param title_plot Title for the plot. Default: fyear-lyear, e.g. 1981-2000.
#' @param ... Parameters accepted by the R function plot can be added.
#'
#' @return A data frame with each row representing a station. Columns contain respectively values for the temporal mean difference between simulation and observation, for the temporal mean of the observations and the temporal mean of the simulations, for the river and station name, for the latitude and longitude of the station, for the area of the catchment according to the meta data of the observations and the area of the catchment of the model routing network and for the GRDC number. Rows are ranked according to the relative difference between simulation and observation.
#' @export
#'
#' @examples
plotBias <- function (data_all_loc, perc_pr = 90, log_plot = F,
                                                  power_symb = 0.25, spec_dis = T, fyear = 0, lyear = 0,
			                    title_plot = " " , ...) {

   fac_sp_dis <- 24 * 3600 / 1000

   nloc <- data_all_loc$nloc

   obsmean <- vector (mode = "double", length = nloc)
   simmean <- vector (mode = "double", length = nloc)
   obsmean[] <- NA
   simmean [] <- NA

   areaobs <- data_all_loc$area_observed
   areamod <- data_all_loc$area_model
   GRDCnr <- data_all_loc$GRDC_number
   latstat <- data_all_loc$latitude_station
   lonstat <- data_all_loc$longitude_station
   altitude <- data_all_loc$altitude

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
   min_pr <- perc_pr / 100 * alldays

   time_jul <- as.numeric (as.Date (timeall))
   ind_per <- which (time_jul >= fday_jul & time_jul <= lday_jul)
   time_per  <- timeall[ind_per]

   obs_per <- data_all_loc$observations [ ,ind_per]
   sim_per <- data_all_loc$simulations [ ,1 ,ind_per]

   for (iloc in (1:nloc)) {

      obs_stat_per  <- obs_per[iloc, ]
      sim_stat_per  <- sim_per[iloc, ]

      # Select only data pairs for which the observation is not missing
      ind_val <- which (!is.na (obs_stat_per))
      ndays <- length (ind_val)
      if (ndays < min_pr) next
      obs_stat_val <- obs_stat_per[ind_val]
      sim_stat_val <- sim_stat_per[ind_val]

      # Compute specific discharge
      if (spec_dis) {
         obs_stat_val <- obs_stat_val / areaobs [iloc] * fac_sp_dis
         sim_stat_val <- sim_stat_val / areamod [iloc] * fac_sp_dis
      }

      obsmean[iloc] <- mean (obs_stat_val)
      simmean[iloc] <- mean (sim_stat_val)

   }

   rel_diff <- (simmean - obsmean) / obsmean * 100
   rel_diff_sort <- sort (rel_diff, index.return = T, na.last = T)
   ind_sort <- rel_diff_sort$ix
   observ <- obsmean [ind_sort]
   simulat <- simmean [ind_sort]
   rel_diff_perc <- rel_diff [ind_sort]

   area_obs <- areaobs [ind_sort]
   area_mod <- areamod [ind_sort]
   GRDC_nr <- GRDCnr [ind_sort]
   lat_station <- latstat [ind_sort]
   lon_station <- lonstat [ind_sort]
   altitude_m <- altitude [ind_sort]
   anal_diff_frame <- data.frame (rel_diff_perc, observ, simulat, lat_station,
                                                                            lon_station, altitude_m, area_obs,
							      area_mod, GRDC_nr)

   maxall <- max (c(obsmean, simmean), na.rm = T)
   minall <- min (c(obsmean, simmean), na.rm = T)
   ax_lim_lin <- c(0, 1.1 * maxall)
   range_data <- c(minall, maxall)

   maxarea <- max (areamod, na.rm = T)
   maxsymsize <- 3
   symsize <- sqrt(areamod / maxarea) * maxsymsize
   symsize <-(areamod / maxarea)^power_symb * maxsymsize

   par (mai = c (1, 1, 0.5, 0.5))
   if (title_plot == " " ) title_plot <- paste (fyear_per, "-", lyear_per, sep = "")

   if (log_plot) {
      log_pl <- "xy"
      axis_style <- "r"
      lim_type <- NULL
   } else {
      log_pl <- ""
      axis_style <- "i"
      lim_type <- ax_lim_lin
   }

   if (spec_dis) {
      x_text <- "Observed mean specific discharge (mm/d)"
      y_text <- "Modelled mean specific discharge (mm/d)"
   } else {
      x_text <- expression("Observed mean discharge (m"^3*"/s)")
      y_text <- expression("Modelled mean discharge (m"^3*"/s)")
   }

   plot (obsmean, simmean,
                xlab = x_text, ylab = y_text,
               xlim = lim_type, xaxs = axis_style, ylim = lim_type, yaxs = axis_style,
	      main = title_plot, type = "n", log = log_pl, ...)
   for (iloc in (1:nloc)) points (obsmean[iloc], simmean[iloc], cex = symsize[iloc], lwd = 1.5)

   lines (range_data, range_data, lty = 1.5)

   return (anal_diff_frame)

}







