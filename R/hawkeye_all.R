#' Hawkeye Stats for All Tests, ODI or IPL matches
#'
#' @param type Type of match data to return: must be one of "TEST", "ODI" or "IPL". Default is "TEST".
#' @param competition Men's or Women's competitions: must be one of "MEN" or "WOMEN". Default is "MEN".
#' @return A tibble containing the Hawkey stats for all available matches. The Hawkeye stats include the ball speed in metres/second,
#'     the x, y location when the ball impacts the pitch, the x, y location of the ball as it passes the stumps and the x, y location
#'     on the field after the batter has played their shot. x, y locations are presented in metres. Individual delivery Hawkeye stats
#'     may be missing if the technology failed. Ball tracking is not available for all matches and field x, y locations are not
#'     available for all matches. Some matches include field x, y locations for non scoring shots, while others only include
#'     field x, y locations for scoring shots.
#' @import curl
#' @importFrom utils read.csv
#' @export
#' @examples
#' df <- get_hawkeye_stats_all(type = "TEST", competition = "WOMEN")

get_hawkeye_stats_all <- function(type = "TEST", competition = "MEN") {

  if(type == "TEST" & competition == "MEN") {
    dat <- read.csv(curl("https://raw.githubusercontent.com/alittlefitness/HawkeyeStats/main/mensTestHawkeyeStats.csv"), stringsAsFactors = F)
  } else if(type == "ODI" & competition == "MEN") {
    dat <- read.csv(curl("https://raw.githubusercontent.com/alittlefitness/HawkeyeStats/main/mensODIHawkeyeStats.csv"), stringsAsFactors = F)
  } else if(type == "IPL" & competition == "MEN") {
    dat <- read.csv(curl("https://raw.githubusercontent.com/alittlefitness/HawkeyeStats/main/mensIPLHawkeyeStats.csv"), stringsAsFactors = F)
  }else if(type == "TEST" & competition == "WOMEN") {
    dat <- read.csv(curl("https://raw.githubusercontent.com/alittlefitness/HawkeyeStats/main/womensTestHawkeyeStats.csv"), stringsAsFactors = F)
  }else if(type == "ODI" & competition == "WOMEN") {
    dat <- read.csv(curl("https://raw.githubusercontent.com/alittlefitness/HawkeyeStats/main/womensODIHawkeyeStats.csv"), stringsAsFactors = F)
  } else if(type == "IPL" & competition == "WOMEN") {
    dat <- read.csv(curl("https://raw.githubusercontent.com/alittlefitness/HawkeyeStats/main/womensIPLHawkeyeStats.csv"), stringsAsFactors = F)
  }

  return(dat)
}
