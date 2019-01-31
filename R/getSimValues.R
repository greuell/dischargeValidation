#' getSimValues
#'
#' Gather simulation values from NetCDF file based on the observation time domain
#'
#' @param simFile
#' @param location
#' @param simOrigin
#' @param simVar
#' @param values
#'
#' @return data.frame with the observation time and values and the simulation values (added as a column)
#' @export
#'
#' @examples
getSimValues <- function(simFile,
                         location,
                         simOrigin,
                         simVar,
                         values)
{
  values = cbind(values, rep(NaN, nrow(values)))
  colnames(values)[ncol(values)] = basename(simFile)

  lon = location[1]
  lat = location[2]

  obsTime = values$time

  # Load simulation data
  nc = nc_open(simFile)
  x = which.min(abs(nc$dim$lon$vals - lon))
  y = which.min(abs(nc$dim$lat$vals - lat))

  simTime = as.Date(x = nc$dim$time$vals, origin = simOrigin)

  timeStart = min(which(simTime > min(obsTime)))
  timeEnd = max(which(simTime < max(obsTime)))
  timeCount = timeEnd - timeStart + 1

  if (is.infinite(timeStart) || is.infinite(timeEnd) || timeCount < 0) {
    warning(paste0("Simulation and observation do not overlap (", basename(simFile), ")"))
    return(values)
  }

  simTime = simTime[timeStart:timeEnd]
  simValues = ncvar_get(
    nc = nc,
    varid = simVar,
    start = c(x, y, timeStart),
    count = c(1, 1, timeCount)
  )

  nc_close(nc)

  print(paste0("> Loaded values from ",
               simTime[1], " to ", simTime[length(simTime)]))

  # Add to values
  transpose = rep(NA, length(simTime))
  for(iTransp in 1:length(transpose)){
    transpose[iTransp] = which(simTime[iTransp] == obsTime)
  }
  values[transpose,ncol(values)] = simValues

  return(values)
}
