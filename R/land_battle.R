#' Run a Land Battle
#'
#' Simulates one land battle for given attacking and defending units
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
#' @param write_to_console If true, writes the output to the console, if false, returns as a vector
#'
#' @return Offense Loses or Defense Loses and remaining units in lines of text
#' @export
#'
#' @examples land_battle(offense_tanks = 4, offense_fighters = 3, defense_tanks = 9, aagun = TRUE)
land_battle <- function(offense_infantry = 0, offense_artillery = 0, offense_tanks = 0, offense_fighters = 0, offense_bombers = 0, defense_infantry = 0, defense_artillery = 0, defense_tanks = 0, defense_fighters = 0, defense_bombers = 0, aagun = FALSE, bombarding_battleships = 0, bombarding_cruisers = 0, write_to_console = TRUE){

  if(write_to_console == TRUE){

  if(aagun){
    offense_fighters = rbinom(1, offense_fighters, 5/6)
    offense_bombers = rbinom(1, offense_bombers, 5/6)
  }

  defense_units = defense_infantry + defense_artillery + defense_tanks + defense_fighters + defense_bombers
  offense_units = offense_infantry + offense_artillery + offense_tanks + offense_fighters + offense_bombers
  offense_ground_units = offense_infantry + offense_artillery + offense_tanks

  bombardment = TRUE

  while(defense_units > 0 & offense_units > 0){
    if(offense_infantry <= offense_artillery){
      offense_hits = rbinom(1, offense_infantry, 2/6) + rbinom(1, offense_artillery, 2/6) + rbinom(1, offense_tanks, 3/6) + rbinom(1, offense_fighters, 3/6) + rbinom(1, offense_bombers, 4/6)
    } else if(offense_infantry > offense_artillery & offense_artillery > 0){
      offense_hits = rbinom(1, offense_infantry - offense_artillery, 1/6) + rbinom(1, offense_artillery*2, 2/6) + rbinom(1, offense_tanks, 3/6) + rbinom(1, offense_fighters, 3/6) + rbinom(1, offense_bombers, 4/6)
    } else{
      offense_hits = rbinom(1, offense_infantry, 1/6) + rbinom(1, offense_artillery, 2/6) + rbinom(1, offense_tanks, 3/6) + rbinom(1, offense_fighters, 3/6) + rbinom(1, offense_bombers, 4/6)
    }
    defense_hits = rbinom(1, defense_infantry, 2/6) + rbinom(1, defense_artillery, 2/6) + rbinom(1, defense_tanks, 3/6) + rbinom(1, defense_fighters, 4/6) + rbinom(1, defense_bombers, 1/6)

    if(bombardment){
      offense_hits = offense_hits + rbinom(1, bombarding_battleships, 4/6) + rbinom(1, bombarding_cruisers, 3/6)
      bombardment = bombardment - 1
    }

    while(defense_hits > 0){
      defense_hits = defense_hits -1

      if(offense_ground_units > 1){

      if(offense_infantry > 0){
        offense_infantry = offense_infantry - 1
      } else if(offense_artillery > 0){
        offense_artillery = offense_artillery - 1
      } else if(offense_tanks > 0){
        offense_tanks = offense_tanks - 1
      } else if(offense_fighters > 0){
        offense_fighters = offense_fighters - 1
      } else if(offense_bombers > 0){
        offense_fighters = offense_bombers - 1
      } else{
        return(writeLines(c("Offense Loses",paste("Offense Infantry: ", offense_infantry), paste("Offense Artillery: ", offense_artillery), paste("Offense Tanks: ", offense_tanks), paste("Offense Fighters: ", offense_fighters), paste("Offense Bombers: ", offense_bombers), paste("Defense Infantry: ", defense_infantry), paste("Defense Artillery: ", defense_artillery), paste("Defense Tanks: ", defense_tanks), paste("Defense Fighters: ", defense_fighters), paste("Defense Bombers: ", defense_bombers))))
        break
      }

      }else{

        if(offense_fighters > 0){
          offense_fighters = offense_fighters - 1
        } else if(offense_bombers > 0){
          offense_bombers = offense_bombers - 1
        } else if(offense_infantry > 0){
          offense_infantry = offense_infantry - 1
        } else if(offense_artillery > 0){
          offense_artillery = offense_artillery - 1
        } else if(offense_tanks > 0){
          offense_tanks = offense_tanks - 1
        } else{
          return(c("Offense Loses","Offense Infantry: ", offense_infantry, "Offense Artillery: ", offense_artillery, "Offense Tanks: ", offense_tanks, "Offense Fighters: ", offense_fighters, "Offense Bombers: ", offense_bombers, "Defense Infantry: ", defense_infantry, "Defense Artillery: ", defense_artillery, "Defense Tanks: ", defense_tanks, "Defense Fighters: ", defense_fighters, "Defense Bombers: ", defense_bombers))
          break
        }

      }

    }

    while(offense_hits > 0){
      offense_hits = offense_hits -1
      if(defense_bombers > 0){
        defense_bombers = defense_bombers - 1
      } else if(defense_infantry > 0){
        defense_infantry = defense_infantry - 1
      } else if(defense_artillery > 0){
        defense_artillery = defense_artillery - 1
      } else if(defense_tanks > 0){
        defense_tanks = defense_tanks - 1
      } else if(defense_fighters > 0){
        defense_fighters = defense_fighters - 1
      } else{
        return(c("Defense Loses","Offense Infantry: ", offense_infantry, "Offense Artillery: ", offense_artillery, "Offense Tanks: ", offense_tanks, "Offense Fighters: ", offense_fighters, "Offense Bombers: ", offense_bombers, "Defense Infantry: ", defense_infantry, "Defense Artillery: ", defense_artillery, "Defense Tanks: ", defense_tanks, "Defense Fighters: ", defense_fighters, "Defense Bombers: ", defense_bombers))
        break
      }
    }

    defense_units = defense_infantry + defense_artillery + defense_tanks + defense_fighters + defense_bombers
    offense_units = offense_infantry + offense_artillery + offense_tanks + offense_fighters + offense_bombers

    if(offense_units == 0){
      return(c("Offense Loses","Offense Infantry: ", offense_infantry, "Offense Artillery: ", offense_artillery, "Offense Tanks: ", offense_tanks, "Offense Fighters: ", offense_fighters, "Offense Bombers: ", offense_bombers, "Defense Infantry: ", defense_infantry, "Defense Artillery: ", defense_artillery, "Defense Tanks: ", defense_tanks, "Defense Fighters: ", defense_fighters, "Defense Bombers: ", defense_bombers))
    } else if(defense_units == 0){
      return(c("Defense Loses","Offense Infantry: ", offense_infantry, "Offense Artillery: ", offense_artillery, "Offense Tanks: ", offense_tanks, "Offense Fighters: ", offense_fighters, "Offense Bombers: ", offense_bombers, "Defense Infantry: ", defense_infantry, "Defense Artillery: ", defense_artillery, "Defense Tanks: ", defense_tanks, "Defense Fighters: ", defense_fighters, "Defense Bombers: ", defense_bombers))
    }

  }

  }else{

    if(aagun){
      offense_fighters = rbinom(1, offense_fighters, 5/6)
      offense_bombers = rbinom(1, offense_bombers, 5/6)
    }

    defense_units = defense_infantry + defense_artillery + defense_tanks + defense_fighters + defense_bombers
    offense_units = offense_infantry + offense_artillery + offense_tanks + offense_fighters + offense_bombers
    offense_ground_units = offense_infantry + offense_artillery + offense_tanks

    bombardment = TRUE

    while(defense_units > 0 & offense_units > 0){
      if(offense_infantry <= offense_artillery){
        offense_hits = rbinom(1, offense_infantry, 2/6) + rbinom(1, offense_artillery, 2/6) + rbinom(1, offense_tanks, 3/6) + rbinom(1, offense_fighters, 3/6) + rbinom(1, offense_bombers, 4/6)
      } else if(offense_infantry > offense_artillery & offense_artillery > 0){
        offense_hits = rbinom(1, offense_infantry - offense_artillery, 1/6) + rbinom(1, offense_artillery*2, 2/6) + rbinom(1, offense_tanks, 3/6) + rbinom(1, offense_fighters, 3/6) + rbinom(1, offense_bombers, 4/6)
      } else{
        offense_hits = rbinom(1, offense_infantry, 1/6) + rbinom(1, offense_artillery, 2/6) + rbinom(1, offense_tanks, 3/6) + rbinom(1, offense_fighters, 3/6) + rbinom(1, offense_bombers, 4/6)
      }
      defense_hits = rbinom(1, defense_infantry, 2/6) + rbinom(1, defense_artillery, 2/6) + rbinom(1, defense_tanks, 3/6) + rbinom(1, defense_fighters, 4/6) + rbinom(1, defense_bombers, 1/6)

      if(bombardment){
        offense_hits = offense_hits + rbinom(1, bombarding_battleships, 4/6) + rbinom(1, bombarding_cruisers, 3/6)
        bombardment = bombardment - 1
      }

      while(defense_hits > 0){
        defense_hits = defense_hits -1

        if(offense_ground_units > 1){

          if(offense_infantry > 0){
            offense_infantry = offense_infantry - 1
          } else if(offense_artillery > 0){
            offense_artillery = offense_artillery - 1
          } else if(offense_tanks > 0){
            offense_tanks = offense_tanks - 1
          } else if(offense_fighters > 0){
            offense_fighters = offense_fighters - 1
          } else if(offense_bombers > 0){
            offense_fighters = offense_bombers - 1
          } else{
            return(c("Offense Loses","Offense Infantry: ", offense_infantry, "Offense Artillery: ", offense_artillery, "Offense Tanks: ", offense_tanks, "Offense Fighters: ", offense_fighters, "Offense Bombers: ", offense_bombers, "Defense Infantry: ", defense_infantry, "Defense Artillery: ", defense_artillery, "Defense Tanks: ", defense_tanks, "Defense Fighters: ", defense_fighters, "Defense Bombers: ", defense_bombers))
            break
          }

        } else{

          if(offense_fighters > 0){
            offense_fighters = offense_fighters - 1
          } else if(offense_bombers > 0){
            offense_bombers = offense_bombers - 1
          } else if(offense_infantry > 0){
            offense_infantry = offense_infantry - 1
          } else if(offense_artillery > 0){
            offense_artillery = offense_artillery - 1
          } else if(offense_tanks > 0){
            offense_tanks = offense_tanks - 1
          } else{
            return(c("Offense Loses","Offense Infantry: ", offense_infantry, "Offense Artillery: ", offense_artillery, "Offense Tanks: ", offense_tanks, "Offense Fighters: ", offense_fighters, "Offense Bombers: ", offense_bombers, "Defense Infantry: ", defense_infantry, "Defense Artillery: ", defense_artillery, "Defense Tanks: ", defense_tanks, "Defense Fighters: ", defense_fighters, "Defense Bombers: ", defense_bombers))
            break
          }

        }

      }

      while(offense_hits > 0){
        offense_hits = offense_hits -1
        if(defense_bombers > 0){
          defense_bombers = defense_bombers - 1
        } else if(defense_infantry > 0){
          defense_infantry = defense_infantry - 1
        } else if(defense_artillery > 0){
          defense_artillery = defense_artillery - 1
        } else if(defense_tanks > 0){
          defense_tanks = defense_tanks - 1
        } else if(defense_fighters > 0){
          defense_fighters = defense_fighters - 1
        } else{
          return(c("Defense Loses","Offense Infantry: ", offense_infantry, "Offense Artillery: ", offense_artillery, "Offense Tanks: ", offense_tanks, "Offense Fighters: ", offense_fighters, "Offense Bombers: ", offense_bombers, "Defense Infantry: ", defense_infantry, "Defense Artillery: ", defense_artillery, "Defense Tanks: ", defense_tanks, "Defense Fighters: ", defense_fighters, "Defense Bombers: ", defense_bombers))
          break
        }
      }

      defense_units = defense_infantry + defense_artillery + defense_tanks + defense_fighters + defense_bombers
      offense_units = offense_infantry + offense_artillery + offense_tanks + offense_fighters + offense_bombers

      if(offense_units == 0){
        return(c("Offense Loses","Offense Infantry: ", offense_infantry, "Offense Artillery: ", offense_artillery, "Offense Tanks: ", offense_tanks, "Offense Fighters: ", offense_fighters, "Offense Bombers: ", offense_bombers, "Defense Infantry: ", defense_infantry, "Defense Artillery: ", defense_artillery, "Defense Tanks: ", defense_tanks, "Defense Fighters: ", defense_fighters, "Defense Bombers: ", defense_bombers))
      } else if(defense_units == 0){
        return(c("Defense Loses","Offense Infantry: ", offense_infantry, "Offense Artillery: ", offense_artillery, "Offense Tanks: ", offense_tanks, "Offense Fighters: ", offense_fighters, "Offense Bombers: ", offense_bombers, "Defense Infantry: ", defense_infantry, "Defense Artillery: ", defense_artillery, "Defense Tanks: ", defense_tanks, "Defense Fighters: ", defense_fighters, "Defense Bombers: ", defense_bombers))
      }

    }




  }



}
