#' Aggregate validation data for further analysis
#'
#' @param data Object returned from getData (class validationData)
#' @param aggregation Time aggregation. Values for "month" and "year" are automatically converted. Custom aggregation by specifying a date format (i.e. used in format.Date) which is subsequently converted to a date if possible (i.e. used in as.Date)
#' @param na.rm Handles how to deal with NA's
#'
#' @return An object containing aggregated values for nloc, nsim, ntime, time, observations, simulations and the requested attributes
#' @export
#'
#' @examples
aggregateData <- function(data, aggregation = "month", na.rm = TRUE)
{
  if(!isValidationData(data)){
    stop(paste0("Data provided is not of class ", validationDataClass()))
  }

  if(aggregation == "month"){
    aggregation = "%Y-%m-01"
  } else if (aggregation == "year"){
    aggregation = "%Y-01-01"
  }

  # Set time
  time.agg = format.Date(data$time, format = aggregation)
  time.agg = try(as.Date(time.agg), silent = TRUE)
  if(class(time.agg) == "try-error"){
    warning("Aggregation format is not recognized as a date, passing (character) results of format into time")
    time.agg = format.Date(data$time, format = aggregation)
  }
  print(paste0("Aggregation timeframe from ", time.agg[1], " to ", time.agg[length(time.agg)]))

  # Set attributes
  attVars = c()
  for(var in names(data)){
    if(! var %in% c("nloc", "nsim", "ntime", "time", "observations", "simulations")){
      attVars = c(attVars, var)
    }
  }

  nloc = data$nloc
  nsim = data$nsim
  ntime = length(unique(time.agg))

  observations.agg = array(data = NA, dim = c(nloc, ntime))
  simulations.agg = array(data = NA, dim = c(nloc, nsim, ntime))

  # Set values
  for (iLoc in 1:nloc) {
    # Aggregate observation data
    observations.agg[iLoc,] = aggregate(x = data$observations[iLoc,], by = list(time.agg), FUN = mean, na.rm = na.rm)[,2]
    observations.agg[iLoc, is.nan(observations.agg[iLoc,])] = NA
    print(paste0("Aggregated observations at location ", iLoc, " of ", nloc))
    for (iSim in 1:nsim) {
      # Aggregate simulation data
      simulations.agg[iLoc, iSim, ] = aggregate(x = data$simulations[iLoc, iSim, ], by = list(time.agg), FUN = mean, na.rm = na.rm)[,2]
      simulations.agg[iLoc, iSim, is.nan(simulations.agg[iLoc, iSim, ])] = NA
      print(paste0("Aggregated simulation at location ", iLoc, " of ", nloc, ", (simulation ", iSim, " of ", nsim, ")"))
    }
  }

  # Load attribute data
  attributes.agg = data[attVars]

  ## Create object
  datum = list(nloc = nloc,
               ntime = ntime,
               nsim = nsim,
               time = unique(time.agg),
               observations = observations.agg,
               simulations = simulations.agg)
  datum = c(datum, attributes.agg)
  class(datum) = validationDataClass()
  return(datum)
}
