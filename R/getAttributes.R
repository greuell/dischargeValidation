getAttributes <- function(file,
                          location,
                          variables)
{
  # Initialize
  attributes = array(data = NA, dim = c(length(variables)))
  lon = location[1]
  lat = location[2]

  # Load data
  for(iVar in 1:length(variables)){
    variable = variables[iVar]

    nc = nc_open(file)
    x = which.min(abs(nc$dim$lon$vals - lon))
    y = which.min(abs(nc$dim$lat$vals - lat))

    for(iVar2 in 1:nc$nvars){
      if(nc$var[[iVar2]]$name == variable){
        variableStart = rep(1, nc$var[[iVar2]]$ndims)
        variableCount = rep(-1, nc$var[[iVar2]]$ndims)
        if(nc$var[[iVar2]]$prec == "char"){
          variableStart[2] = x
          variableStart[3] = y
          variableCount[2] = 1
          variableCount[3] = 1
        } else {
          variableStart[1] = x
          variableStart[2] = y
          variableCount[1] = 1
          variableCount[2] = 1
        }
        break
      }
    }

    value = ncvar_get(nc = nc, varid = variable, start = variableStart, count = variableCount)

    nc_close(nc)

    attributes[iVar] = c(value)
  }

  print(paste0("Loaded ", length(attributes), " attributes (", paste0(variables, collapse = ", "), ")"))

  return(attributes)
}
