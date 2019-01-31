#' getDatum
#'
#' Get observation and simulation data and attributes at specified location
#'
#' @param obsFile
#' @param simFiles
#' @param location
#' @param simOrigins
#' @param simVars
#' @param obsOrigin
#' @param obsVar
#' @param attributes
#'
#' @return validationData classes for the location
#' @export
#'
#' @examples
getDatum <- function(obsFile,
                     simFiles,
                     location,
                     simOrigins,
                     simVars,
                     obsOrigin,
                     obsVar,
                     attributes)
{
  datum = list(values = data.frame(), attributes = list())

  lon = location[1]
  lat = location[2]

  print(paste0("Observations: ", basename(obsFile)))

  # Load observation data
  nc = nc_open(obsFile)
  x = which.min(abs(nc$dim$lon$vals - lon))
  y = which.min(abs(nc$dim$lat$vals - lat))

  obsTime = as.Date(x = nc$dim$time$vals, origin = obsOrigin)
  obsValues = ncvar_get(
    nc = nc,
    varid = obsVar,
    start = c(x, y, 1),
    count = c(1, 1, -1)
  )

  nc_close(nc)

  # Trim
  timeStart = min(which(!is.na(obsValues)))
  timeEnd = max(which(!is.na(obsValues)))

  obsTime = obsTime[timeStart:timeEnd]
  obsValues = obsValues[timeStart:timeEnd]

  print(paste0("> Loaded values from ",
               obsTime[1], " to ", obsTime[length(obsTime)]))

  # Add to values
  values = data.frame(time = obsTime, observation = obsValues)

  # Load simulations data
  for (iSim in length(simFiles)) {
    simFile = simFiles[iSim]
    simOrigin = simOrigins[iSim]
    simVar = simVars[iSim]

    print(paste0("Simulation: ", basename(simFile),
                 " (", iSim, " of ", length(simFiles), ")"))

    values = getSimValues(simFile = simFile,
                          location = location,
                          simOrigin = simOrigin,
                          simVar = simVar,
                          values = values)
  }

  # Load attributes
  attributes = list()

  datum$attributes = attributes
  datum$values = values

  class(data) = validationDataClass()
  return(datum)
}
