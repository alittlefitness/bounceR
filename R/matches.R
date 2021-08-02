#' Tidy metadata from completed Test, ODI or IPL T20 matches
#'
#' @param type Type of match data to return: must be one of "TEST", "ODI" or "IPL". Default is "TEST".
#' @param competition Men's or Women's competitions: must be one of "MEN" or "WOMEN". Default is "MEN".
#' @return A tibble containing metadata for Test, ODI or IPL T20 matches.
#' @import dplyr
#' @import jsonlite
#' @import curl
#' @importFrom stats na.omit
#' @importFrom stats setNames
#' @export
#' @examples
#' df <- get_completed_matches(type = "TEST", competition = "MEN")
#' df <- get_completed_matches(type = "ODI", competition = "WOMEN")
#' df <- get_completed_matches(type = "IPL", competition = "MEN")

get_completed_matches <- function(type = "TEST", competition = "MEN") {
  value <- NULL
  comp <- if_else(competition == "MEN", "b%2Cm", "w") # used to set the url for Tests and ODI

  if(type %in% c("TEST", "ODI") & competition %in% c("MEN", "WOMEN")) { # Test and ODI matches use the ICC page to access the json format data
    initial_url <- paste0("https://cricketapi-icc.pulselive.com/fixtures?matchTypes=",
                          type, "&tournamentTypes=I%2CWI&teamTypes=", comp, "&matchStates=C&page=0&pageSize=300&sort=desc")

    initial <- fromJSON(initial_url)
    # find how many pages of completed matches exist
    page_info <- tibble(value = initial$pageInfo$numPages) %>%
      distinct() %>%
      mutate(value = value - 1)

    # create the tibble from the 1st page
    suppressMessages(
    match_info <- setNames(tibble(initial$content[c("timestamp", "label", "tournamentLabel")],
                                  initial$content$scheduleEntry["matchType"],
                                  initial$content$tournamentId[c("id", "name")],
                                  initial$content$scheduleEntry$matchId["id"],
                                  initial$content$scheduleEntry$venue[c("fullName", "city", "country")],
                                  initial$content$scheduleEntry$matchStatus["text"],
                                  initial$content$scheduleEntry$team1$team[c("fullName")],
                                  initial$content$scheduleEntry$team2$team[c("fullName")], .name_repair = "universal"),
                           c("timestamp", "label", "tournamentLabel", "matchType",
                             "tournamentId", "tournamentName", "matchId",
                             "venueFullName", "venueCity", "venueCountry",
                             "resultText", "team1FullName", "team2FullName"))
    )

    # loop through remaining pages and bind the tibbles together
    suppressMessages(
      try(
      for(i in 1:page_info$value) {
        page_info_url <- paste0("https://cricketapi-icc.pulselive.com/fixtures?matchTypes=",
                                type, "&tournamentTypes=I%2CWI&teamTypes=", comp, "&matchStates=C&page=",
                                i, "&pageSize=300&sort=desc")
        dat <- fromJSON(page_info_url)
        content <- setNames(tibble(dat$content[c("timestamp", "label", "tournamentLabel")],
                                   dat$content$scheduleEntry["matchType"],
                                   dat$content$tournamentId[c("id", "name")],
                                   dat$content$scheduleEntry$matchId["id"],
                                   dat$content$scheduleEntry$venue[c("fullName", "city", "country")],
                                   dat$content$scheduleEntry$matchStatus["text"],
                                   dat$content$scheduleEntry$team1$team[c("fullName")],
                                   dat$content$scheduleEntry$team2$team[c("fullName")], .name_repair = "universal"),
                            c("timestamp", "label", "tournamentLabel", "matchType",
                              "tournamentId", "tournamentName", "matchId",
                              "venueFullName", "venueCity", "venueCountry",
                              "resultText", "team1FullName", "team2FullName"))

        match_info <- bind_rows(match_info, content)
      }, silent = T)
    )
    return(match_info)
  } else if(type == "IPL" & competition == "MEN") { # IPL matches use the BCCI page to acces the json format data

    ipl_season_ids <- c(22399, 18790, 10192, 7749, 5815, 3957, 2785, 2374, 605, 1) # Season ids for IPL data pages
    working <- tibble(timestamp = "") # create working tibble to bind to in the loop

    # loop through the season ids and bind data into a single tibble
    for(i in ipl_season_ids){

      initial_url <- paste0("https://cricketapi.platform.iplt20.com//fixtures?tournamentIds=",
                            i, "&pageSize=100")
      dat <- fromJSON(initial_url)
      suppressMessages(
        content <- setNames(tibble(dat$content[c("timestamp", "label", "tournamentLabel")],
                                   dat$content$scheduleEntry["matchType"],
                                   dat$content$tournamentId[c("id", "name")],
                                   dat$content$scheduleEntry$matchId["id"],
                                   dat$content$scheduleEntry$venue[c("fullName", "city", "country")],
                                   dat$content$scheduleEntry$matchStatus["text"],
                                   dat$content$scheduleEntry$team1$team[c("fullName")],
                                   dat$content$scheduleEntry$team2$team[c("fullName")], .name_repair = "universal"),
                            c("timestamp", "label", "tournamentLabel", "matchType",
                              "tournamentId", "tournamentName", "matchId",
                              "venueFullName", "venueCity", "venueCountry",
                              "resultText", "team1FullName", "team2FullName"))
      )
      working <- bind_rows(working, content)
    }

    match_info <- working %>%
      na.omit()
    return(match_info)

  } else if(type == "IPL" & competition == "WOMEN") {

    ipl_season_ids <- c(21511, 11365, 8337) # Season ids for Women's IPL data pages
    working <- tibble(timestamp = "") # create working tibble to bind to in the loop

    # loop through the season ids and bind data into a single tibble
    for(i in ipl_season_ids){

      initial_url <- paste0("https://cricketapi.platform.iplt20.com//fixtures?tournamentIds=",
                            i, "&pageSize=100")
      dat <- fromJSON(initial_url)
      suppressMessages(
        content <- setNames(tibble(dat$content[c("timestamp", "label", "tournamentLabel")],
                                   dat$content$scheduleEntry["matchType"],
                                   dat$content$tournamentId[c("id", "name")],
                                   dat$content$scheduleEntry$matchId["id"],
                                   dat$content$scheduleEntry$venue[c("fullName", "city", "country")],
                                   dat$content$scheduleEntry$matchStatus["text"],
                                   dat$content$scheduleEntry$team1$team[c("fullName")],
                                   dat$content$scheduleEntry$team2$team[c("fullName")], .name_repair = "universal"),
                            c("timestamp", "label", "tournamentLabel", "matchType",
                              "tournamentId", "tournamentName", "matchId",
                              "venueFullName", "venueCity", "venueCountry",
                              "resultText", "team1FullName", "team2FullName"))
      )
      working <- bind_rows(working, content)
    }

    match_info <- working %>%
      na.omit()
    return(match_info)
  }
}

