#' Sea Round
#'
#' Simulates one round of sea combat
#'
#' @param offense_submarines Number of submarines on the attacking side, which hit when the dice roll is a 2 or less
#' @param offense_destroyers Number of destroyers on the attacking side, which hit when the dice roll is a 2 or less
#' @param offense_carriers Number of carriers on the attacking side, which hit when the dice roll is a 1
#' @param offense_cruisers Number of cruisers on the attacking side, which hit when the dice roll is a 3 or less
#' @param offense_battleships Number of battleships on the attacking side, which hit when the dice roll is a 4 or less
#' @param offense_fighters Number of fighters on the attacking side, which hit when the dice roll is a 3 or less
#' @param offense_bombers Number of bombers on the attacking side, which hit when the dice roll is a 4 or less
#' @param defense_submarines Number of submarines on the defending side, which hit when the dice roll is a 1
#' @param defense_destroyers Number of destroyers on the defending side, which hit when the dice roll is a 2 or less
#' @param defense_carriers Number of carriers on the defending side, which hit when the dice roll is a 2 or less
#' @param defense_cruisers Number of cruisers on the defending side, which hit when the dice roll is a 3 or less
#' @param defense_battleships Number of battleships on the defending side, which hit when the dice roll is a 4 or less
#' @param defense_fighters Number of fighters on the defending side, which hit when the dice roll is a 4 or less
#' @param write_to_console If true, writes the output to the console, if false, returns as a vector
#'
#' @return Number of offensive air hits, offensive submarine hits, offensive other hits, defensive air hits, defensive submarine hits, and defensive other hits in several lines of text
#' @export
#'
#' @examples sea_round(offense_submarines = 1, offense_bombers = 1, defense_battleships = 1)
sea_round <- function(offense_submarines = 0, offense_destroyers = 0, offense_carriers = 0, offense_cruisers = 0, offense_battleships = 0, offense_fighters = 0, offense_bombers = 0, defense_submarines = 0, defense_destroyers = 0, defense_carriers = 0, defense_cruisers = 0, defense_battleships = 0, defense_fighters = 0, write_to_console = TRUE){

  offense_air_hits = rbinom(1, offense_fighters, 3/6) + rbinom(1, offense_bombers, 4/6)
  offense_submarine_hits = rbinom(1, offense_submarines, 2/6)
  offense_other_hits = rbinom(1, offense_destroyers, 2/6) + rbinom(1, offense_cruisers, 3/6) + rbinom(1, offense_carriers, 1/6) + rbinom(1, offense_battleships, 4/6)

  defense_air_hits = rbinom(1, defense_fighters, 4/6)
  defense_submarine_hits = rbinom(1, defense_submarines, 1/6)
  defense_other_hits = rbinom(1, defense_destroyers, 2/6) + rbinom(1, defense_cruisers, 3/6) + rbinom(1, defense_carriers, 2/6) + rbinom(1, defense_battleships, 4/6)

  if(write_to_console == TRUE){
  return(writeLines(c(paste("Offense Air Hits: ", offense_air_hits), paste("Offense Submarine Hits: ", offense_submarine_hits), paste("Offense Other Hits: ", offense_other_hits), paste("Defense Air Hits: ", defense_air_hits), paste("Defense Submarine Hits: ", defense_submarine_hits), paste("Defense Other Hits: ", defense_other_hits))))
  }else{
  return(c("Offense Air Hits: ", offense_air_hits, "Offense Submarine Hits: ", offense_submarine_hits, "Offense Other Hits: ", offense_other_hits, "Defense Air Hits: ", defense_air_hits, "Defense Submarine Hits: ", defense_submarine_hits, "Defense Other Hits: ", defense_other_hits))
  }

}
