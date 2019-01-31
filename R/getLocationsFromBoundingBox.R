getLocationsFromBoundingBox <- function(obsFile,
                                        boundingBox,
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

  for(obsX in 1:dim(obsMask)[1]){
    for(obsY in 1:dim(obsMask)[2]){
      if(!(obsLons[obsX] >= boundingBox[1] && obsLons[obsX] <= boundingBox[2] &&
           obsLats[obsY] >= boundingBox[3] && obsLats[obsY] <= boundingBox[4])){
        next
      }
      if(is.na(obs.mask[obsX,obsY])){
        next
      }

      locations[[length(locations) + 1]] = c(obsLons[obsX], obsLats[obsY])
    }
  }

  print(paste0("Found ", length(locations), " locations"))

  return(locations)
}
