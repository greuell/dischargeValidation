getTime <- function(obsFile,
                    simFiles,
                    simOrigins,
                    simSkips,
                    obsOrigin)
{
  # Initialize
  time = c()

  # Load observation time
  nc = nc_open(obsFile)
  obsTime = as.Date(x = nc$dim$time$vals, origin = obsOrigin)
  nc_close(nc)

  timeStart = min(obsTime)
  timeEnd = max(obsTime)

  for (iSim in length(simFiles)) {
    simFile = simFiles[iSim]
    simOrigin = simOrigins[iSim]
    simSkip = simSkips[iSim]

    # Load simulation time
    nc = nc_open(simFile)
    simTime = as.Date(x = nc$dim$time$vals, origin = simOrigin)
    nc_close(nc)

    startSimTime = min(simTime)
    endSimTime = max(simTime)

    # Add skip time
    startSimYear = as.numeric(format(startSimTime, "%Y"))
    startSimMonth = as.numeric(format(startSimTime, "%m"))
    startSimDay = as.numeric(format(startSimTime, "%d"))

    startSimMonth = startSimMonth + simSkip
    if(startSimMonth > 12){
      startSimYear = startSimYear + 1
      startSimMonth = startSimMonth - 12
    }

    startSimTime = as.Date(paste0(startSimYear, "-", startSimMonth, "-", startSimDay))

    # Calculate overlap
    timeStart = max(timeStart, startSimTime)
    timeEnd = min(timeEnd, endSimTime)
  }

  time = seq(from = timeStart, to = timeEnd, by = "day")

  print(paste0("Loaded timeframe from ", timeStart, " to ", timeEnd))
  return(time)
}
