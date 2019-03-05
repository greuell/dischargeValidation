#' Get the location of stations based on the mask of the observation file and the mask of the simulation file(s)
#'
#' @param obsFile Path to the observation file.
#' @param simFiles Path to the simulation file.
#' @param simVar Simulation variable to be used as mask. If the variable is NA, it is assumed the cell does not contain data. Variable dimensions > 2 only use the first slice of redundand dimensions. Defaults to VIC discharge variable.
#' @param obsVar Observation variable to be used as mask. If the variable is NA, it is assumed  the cell does not contain data. Variable dimensions > 2 only use the first slice of redundand dimensions. Defaults to GRDC_number.
#'
#' @return List of station locations containing lon/lat values in a vector.
#' @export
#'
#' @examples
#' @import ncdf4
getLocationsFromSimulations <- function(obsFile,
                                        simFile,
                                        simVar = "OUT_DISCHARGE",
                                        obsVar = "GRDC_number"){
  locations = list()

  # Observation mask
  nc = nc_open(filename = obsFile)
  for(iVar in 1:nc$nvars){
    if(nc$var[[iVar]]$name == obsVar){
      start = count = rep(1, nc$var[[iVar]]$ndims)
      count[1:2] = -1

      obsMask = ncvar_get(nc = nc, varid = obsVar, start = start, count = count)
    }
  }

  obsLons = nc$dim$lon$vals
  obsLats = nc$dim$lat$vals
  nc_close(nc)

  if(!exists("obsMask")){
    stop("Could not find the observation mask variable")
    return(locations)
  } else {
    print(paste0("Loaded observation mask (dimensions: ", dim(obsMask)[1], " by ", dim(obsMask)[2], ")"))
  }

  nc = nc_open(filename = simFile)
  for(iVar in 1:nc$nvars){
    if(nc$var[[iVar]]$name == simVar){
      start = count = rep(1, nc$var[[iVar]]$ndims)
      count[1:2] = -1

      simMask = ncvar_get(nc = nc, varid = simVar, start = start, count = count)
    }
  }
  simLons = nc$dim$lon$vals
  simLats = nc$dim$lat$vals
  nc_close(nc)

  if(!exists("simMask")){
    stop("Could not find the simulation mask variable")
    return(locations)
  } else {
    print(paste0("Loaded simulation mask (dimensions: ", dim(simMask)[1], " by ", dim(simMask)[2], ")"))
  }

  for(obsX in 1:dim(obsMask)[1]){
    for(obsY in 1:dim(obsMask)[2]){
      if(! (obsLons[obsX] %in% simLons && obsLats[obsY] %in% simLats)){
        next
      }

      if(is.na(obsMask[obsX,obsY])){
        next
      }

      simX = which(simLons == obsLons[obsX])
      simY = which(simLats == obsLats[obsY])

      if(is.na(simMask[simX, simY])){
        next
      }

      locations[[length(locations) + 1]] = c(obsLons[obsX], obsLats[obsY])
    }
  }

  print(paste0("Found ", length(locations), " locations"))

  return(locations)
}
