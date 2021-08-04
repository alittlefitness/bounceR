#' Hawkeye Stats for Tests, ODI or IPL matches
#'
#' @param id Id for match(es) to collect Hawkeye stats from. Invalid ids or ids with no Hawkeye data will result in an empty tibble
#'     or an error. Ids can be found using the get_completed_matches function or via the icc-cricket.com results page for Test and
#'     ODI matches and via the iplt20.com page for IPL matches.
#' @return A tibble containing the Hawkey stats for the requested matches. The Hawkeye stats include the ball speed in metres/second,
#'     the x, y location when the ball impacts the pitch, the x, y location of the ball as it passes the stumps and the x, y location
#'     on the field after the batter has played their shot. x, y locations are presented in metres. Individual delivery Hawkeye stats
#'     may be missing if the technology failed. Ball tracking is not available for all matches and field x, y locations are not
#'     available for all matches. Some matches include field x, y locations for non scoring shots, while others only include
#'     field x, y locations for scoring shots.
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import jsonlite
#' @import curl
#' @importFrom stats na.omit
#' @export
#' @examples
#' df <- get_hawkeye_stats(id = 23469)

get_hawkeye_stats <- function(id) {
  ID <- ID_1 <- ID_10 <- ID_11 <- ID_13 <- ID_14 <- ID_15 <- ID_16 <- ID_17 <- ID_18 <- ID_2 <- ID_3 <-
    ID_4 <- ID_5 <- ID_7 <- ID_8 <- ID_9 <- bowlingStyle <- delivery <- fullName <- matchId <-
    nonStriker <- rightArmedBowl <- rightHandedBat <- value <- . <- NULL # set the global variables to NULL

  working <- tibble(delivery = "") # create a working tibble
  players <- tibble(id = NULL) # create an empty players tibble

  for(i in id) {
    url <- paste0("https://cricketapi-icc.pulselive.com/fixtures/", i, "/uds/stats") # url string to collect Hawkeye stats
    player_url <- paste0("https://cricketapi-icc.pulselive.com/fixtures/", i, "/uds") # url string to collect player information

    initial <- try(fromJSON(url, simplifyVector = F), silent = T) # if url doesn't exist register the error but move on

    suppressWarnings(
      if(str_detect(initial, "error") == F) {
        if(length(initial$data) != 0) {
          content <- tibble(raw = initial$data) %>%
            unnest_longer(col = raw, values_to = "value", indices_to = "delivery") %>%
            mutate(matchId = i)

          working <- bind_rows(working, content)

          players_list <- fromJSON(player_url)
          team1 <- tibble(players_list$teams$players[[1]])
          team2 <- tibble(players_list$teams$players[[2]])

          players <- bind_rows(players, team1, team2)
        }
      }
    )
  }

  try({
    # turn the data into a tidy tibble
    working_hawkeye <- working %>%
      mutate(value = str_split(value, ",")) %>%
      unnest(value) %>%
      group_by(matchId, delivery) %>%
      mutate(ID = paste0("ID_", row_number())) %>%
      pivot_wider(names_from = ID, values_from = value)

    # determine whether the match has hawkeye stats available
    has_hawkeye <- working_hawkeye %>%
      filter(ID_5 > 0 | ID_17 > 0) %>%
      ungroup() %>%
      distinct(matchId)

    # filter the working tibble to only include matches with hawkeye stats available
    hawkeye <- working_hawkeye %>%
      filter(matchId %in% has_hawkeye$matchId) %>%
      mutate_at(vars(ID_2, ID_3, ID_4), ~ as.integer(.))

    batter <- distinct(players) %>%
      select(ID_2 = id, batter = fullName, rightHandedBat)

    non_striker <- distinct(players) %>%
      select(ID_3 = id, nonStriker = fullName)

    bowler <- distinct(players) %>%
      select(ID_4 = id, bowler = fullName, rightArmedBowl, bowlingStyle)

    suppressMessages(
      # join the filtered tibble with batter, non-striker and bowler information and create a tidy tibble
      hawkeye_stats <- left_join(hawkeye, batter) %>%
        left_join(., non_striker) %>%
        left_join(., bowler) %>%
        na.omit() %>%
        select(matchId, delivery, ball = ID_1, batter, batterId = ID_3, rightHandedBat, nonStriker, nonStrikerId = ID_3,
               bowler, bowlerId = ID_4, rightArmedBowl, bowlingStyle, ballSpeed = ID_5, dismissalDetails = ID_7, runs = ID_8,
               batterRuns = ID_9, bowlerRuns = ID_10, extras = ID_11, pitchX = ID_14, pitchY = ID_13, stumpsX = ID_15,
               stumpsY = ID_16, fieldX = ID_17, fieldY = ID_18)
      )
  }, silent = T)
  return(hawkeye_stats)
}


