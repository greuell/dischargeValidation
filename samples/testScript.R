rm(list = ls())

simFile = "/home/bram/dischargeValidation/data/fluxes_global(nat-dngw).1979-01.nc"
obsFile = "/home/bram/dischargeValidation/data/global_res_halfdegr_LargestArea.nc"


locations = getLocationsFromSimulations(obsFile = obsFile,
                                        simFile = simFile)

data = getData(obsFile = obsFile,
               simFiles = c(simFile),
               locations = locations)

datum = data[[10]]
plot(datum$values$time, datum$values$observation, type = "l")
