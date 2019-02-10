getValues <- function(file,
                      location,
                      variable,
                      origin,
                      time)
{
  # Initialize
  values = rep(x = NA, length.out = length(time))
  lon = location[1]
  lat = location[2]

  # Load data
  nc = nc_open(file)

  getTime = as.Date(x = nc$dim$time$vals, origin = origin)

  timeStart = which.min(abs(getTime - min(time)))
  timeEnd = which.min(abs(getTime - max(time)))
  timeCount = timeEnd - timeStart + 1

  if (is.infinite(timeStart) || is.infinite(timeEnd) || timeCount < 0) {
    nc_close(nc)
    return(values)
  }

  x = which.min(abs(nc$dim$lon$vals - lon))
  y = which.min(abs(nc$dim$lat$vals - lat))

  getTime = getTime[timeStart:timeEnd]
  getValues = ncvar_get(nc = nc, varid = variable, start = c(x, y, timeStart), count = c(1, 1, timeCount))
  nc_close(nc)

  # Add to values
  transpose = rep(NA, length(getTime))
  for(iTransp in 1:length(transpose)){
    transpose[iTransp] = which(getTime[iTransp] == time)
  }
  values[transpose] = getValues

  print(paste0("Loaded values from ", basename(file)))
  return(values)
}
