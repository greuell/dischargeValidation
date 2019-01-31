
rm(list = ls())

obsFile = system.file(c("data", "fluxes_global(nat-dngw).1979-01.nc"), package = "dischargeValidation")
simFile = system.file(c("data", "global_res_halfdegr_LargestArea.nc"), package = "dischargeValidation")

locations = getLocationsFromSimulations(obsFile = obsFile,
                                        simFile = simFile)

data = getData(obsFile = obsFile,
               simFiles = c(simFile),
               locations = locations[1])

datum = data[[1]]
plot(datum$values$time, datum$values$observation, type = "l")
lines(datum$values$time, datum$values[,3], col = "red")
