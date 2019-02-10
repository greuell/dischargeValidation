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
    value = ncvar_get(nc = nc, varid = variable, start = c(x, y), count = c(1, 1))

    nc_close(nc)

    attributes[iVar] = c(value)
  }

  print(paste0("Loaded ", length(attributes), " attributes (", paste0(variables, collapse = ", "), ")"))

  return(attributes)
}
