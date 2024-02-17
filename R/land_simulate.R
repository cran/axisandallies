#' Simulate Land Battles
#'
#' Simulates a number of land battles and gives the percentage of them won and lost. Use to find the probability of winning a particular land battle.
#'
#' @param offense_infantry Number of infantry on the attacking side, which hit when the dice roll is a 1
#' @param offense_artillery Number of artillery on the attacking side, which hit when the dice roll is a 2 or less
#' @param offense_tanks Number of tanks on the attacking side, which hit when the dice roll is a 3 or less
#' @param offense_fighters Number of fighters on the attacking side, which hit when the dice roll is a 3 or less
#' @param offense_bombers Number of bombers on the attacking side, which hit when the dice roll is a 4 or less
#' @param defense_infantry Number of infantry on the defending side, which hit when the dice roll is a 2 or less
#' @param defense_artillery Number of artillery on the defending side, which hit when the dice roll is a 2 or less
#' @param defense_tanks Number of tanks on the defending side, which hit when the dice roll is a 3 or less
#' @param defense_fighters Number of infantry on the defending side, which hit when the dice roll is a 4 or less
#' @param defense_bombers Number of infantry on the defending side, which hit when the dice roll is a 1 or less
#' @param aagun Whether or not an Anti-Air gun is present, which rolls one time at the beginning of the battle for each attacking aircraft, and hits if the roll is a 1
#' @param bombarding_battleships Number of bombarding battleships, which bombard at the start of the battle, and hit at 4 or less
#' @param bombarding_cruisers Number of bombarding cruisers, which bombard at the start of the battle, and hit at 3 or less
#' @param sample_size Number of land battles simulated
#' @param decimals Number of decimal places the percentages are rounded to
#' @param write_to_console If true, writes the output to the console, if false, returns as a vector
#'
#' @return Percentage of the land battles won and lost.
#' @export
#'
#' @examples land_simulate(offense_infantry = 10, defense_infantry = 6, decimals = 2)
#'
#'
land_simulate <- function(offense_infantry = 0, offense_artillery = 0, offense_tanks = 0, offense_fighters = 0, offense_bombers = 0, defense_infantry = 0, defense_artillery = 0, defense_tanks = 0, defense_fighters = 0, defense_bombers = 0, aagun = FALSE, bombarding_battleships = 0, bombarding_cruisers = 0, sample_size = 10000, decimals = 1, write_to_console = TRUE){
  win_count = 0
  for(i in 1:sample_size){
    win_count = win_count + .land_simulate_hidden(offense_infantry, offense_artillery, offense_tanks, offense_fighters, offense_bombers, defense_infantry, defense_artillery, defense_tanks, defense_fighters, defense_bombers, aagun, bombarding_battleships, bombarding_cruisers)
  }

  if(write_to_console==TRUE){
  return(writeLines(paste("Offense wins: ", round(win_count/(sample_size*0.01), decimals), "%      Defense wins: ", round(100-win_count/(sample_size*0.01), decimals), "%")))
}else{
  return(c("Offense wins: ", round(win_count/(sample_size*0.01), decimals), "Defense wins: ", round(100-win_count/(sample_size*0.01), decimals)))
}

}





