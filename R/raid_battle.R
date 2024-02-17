#' Strategic Bombing Raid
#'
#' Simulates one strategic bombing raid on an enemy industrial complex
#'
#' @param offense_fighters Number of fighters brought to the strategic bombing raid if using optional rules for strategic bombing raids which include fighters
#' @param offense_bombers Number of bombers brought to bombing raid
#' @param defense_fighters Number of fighters defending in the strategic bombing raid if using optional rules for strategic bombing raids which include fighters
#' @param aagun Is an anti aircraft gun present on the defending side
#' @param write_to_console If true, writes the output to the console, if false, returns as a vector
#'
#' @return IPC Damage done by strategic bombing raid to industrial complex, number of offense fighters left if using optional rules, number of bombers left, number of defense fighters left if using optional rules in lines of text
#' @export
#'
#' @examples raid_battle(offense_bombers = 3, aagun = TRUE)
raid_battle <- function(offense_fighters = 0, offense_bombers = 1, defense_fighters = 0, aagun = FALSE, write_to_console = TRUE){

  if(aagun){
   offense_fighters = rbinom(1, offense_fighters, 5/6)
   offense_bombers = rbinom(1, offense_bombers, 5/6)
  }

  offense_hits = rbinom(1, offense_fighters, 1/6)
  defense_hits = rbinom(1, defense_fighters, 2/6)

  while(defense_hits > 0){
    defense_hits = defense_hits -1

    if(offense_fighters > 0){
      offense_fighters = offense_fighters - 1
    } else if(offense_bombers > 0){
      offense_bombers = offense_bombers - 1
    }

  }

  while(offense_hits > 0){
    offense_hits = offense_hits -1
    defense_fighters = defense_fighters - 1

  }


  damage = 0

  for(i in 1:offense_bombers){
  damage = damage + floor(runif(1, 1, 7))
  }

  if(write_to_console == TRUE){
  return(writeLines(c(paste("Damage: ", damage), paste("Bombers Remaining: ", offense_bombers), paste("Offense Fighters Remaining: ", offense_fighters), paste("Defense Fighters Remaining: ", defense_fighters))))
  }else{
  return(c("Damage: ", damage, "Bombers Remaining: ", offense_bombers, "Offense Fighters Remaining: ", offense_fighters, "Defense Fighters Remaining: ", defense_fighters))
  }
}

