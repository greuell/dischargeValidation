#' getData
#'
#' Get observation and simulation data and attributes at specified locations
#'
#' @param obsFile
#' @param simFiles
#' @param locations
#' @param simOrigins
#' @param simVars
#' @param obsOrigin
#' @param obsVar
#' @param attributes
#'
#' @return list containing validationData classes for each location
#' @export
#'
#' @examples
getData <- function(obsFile,
                    simFiles,
                    locations,
                    simOrigins = rep("0001-01-01", length(simFiles)),
                    simVars = rep("OUT_DISCHARGE", length(simFiles)),
                    obsOrigin = "1900-01-01",
                    obsVar = "dis",
                    attributes = c("all"))
{
  data = list()

  for (iLoc in 1:length(locations)) {
    location = locations[[iLoc]]

    print(paste0("Location: ",
                 location[1], " N ", location[2], " E ",
                 "(", iLoc, " of ", length(locations), ")"))

    datum = getDatum(
      obsFile = obsFile,
      simFiles = simFiles,
      location = location,
      simOrigins = simOrigins,
      simVars = simVars,
      obsOrigin = obsOrigin,
      obsVar = obsVar,
      attributes = attributes
    )

    data[[iLoc]] = datum
  }

  return(data)
}
