#' Unit Information
#'
#' Gives basic information about the units in axis and allies
#'
#' @param unit A unit in axis and allies spring 1942 in all lowercase letters
#' @param write_to_console If true, writes the output to the console, if false, returns as a vector
#'
#' @return Describes the unit's attack, defense, movement, and cost, and other details in several lines of text
#' @export
#'
#' @examples info_units("artillery")
info_units <- function(unit, write_to_console=TRUE){

  if(write_to_console == TRUE){

      if(unit == "infantry"){
        return(writeLines(c(paste("Attack: 1"), paste("Defense: 2"), paste("Movement: 1"), paste("Cost: 3"))))
      } else if(unit == "artillery"){
        return(writeLines(c(paste("Attack: 2"), paste("Defense: 2"), paste("Movement: 1"), paste("Cost: 4"), paste("Notes: Each artillery can increase one infantry's attack from 1 to 2 in battle"))))
      } else if(unit == "tank"){
        return(writeLines(c(paste("Attack: 3"), paste("Defense: 3"), paste("Movement: 2"), paste("Cost: 5"))))
      } else if(unit == "fighter"){
        return(writeLines(c(paste("Attack: 2"), paste("Defense: 2"), paste("Movement: 4"), paste("Cost: 4"))))
      } else if(unit == "bomber"){
        return(writeLines(c(paste("Attack: 4"), paste("Defense: 1"), paste("Movement: 6"), paste("Cost: 12"), paste("Notes: Can conduct strategic bombing raids"))))
      } else if(unit == "aa gun"){
        return(writeLines(c(paste("Attack: N/A"), paste("Defense: 1"), paste("Movement: 1"), paste("Cost: 5"), paste("Notes: Only fires in air defense phase, rolls one dice for each attacking aircraft"))))
      } else if(unit == "aagun"){
        return(writeLines(c(paste("Attack: N/A"), paste("Defense: 1"), paste("Movement: 1"), paste("Cost: 5"), paste("Notes: Only fires in air defense phase, rolls one dice for each attacking aircraft"))))
      } else if(unit == "industrial complex"){
        return(writeLines(c(paste("Attack: N/A"), paste("Defense: N/A"), paste("Movement: N/A"), paste("Cost: 15"), paste("Notes: Units can only be placed on a territory with an industrial complex"))))
      } else if(unit == "complex"){
        return(writeLines(c(paste("Attack: N/A"), paste("Defense: N/A"), paste("Movement: N/A"), paste("Cost: 15"), paste("Notes: Units can only be placed on a territory with an industrial complex"))))
      } else if(unit == "submarine"){
        return(writeLines(c(paste("Attack: 2"), paste("Defense: 1"), paste("Movement: 2"), paste("Cost: 6"), paste("Notes: Can submerge or conduct surprise strike"))))
      } else if(unit == "destroyer"){
        return(writeLines(c(paste("Attack: 2"), paste("Defense: 2"), paste("Movement: 2"), paste("Cost: 8"), paste("Notes: Cancels enemy submarine abilities"))))
      } else if(unit == "cruiser"){
        return(writeLines(c(paste("Attack: 3"), paste("Defense: 3"), paste("Movement: 2"), paste("Cost: 12"), paste("Notes: Can participate in bombardment"))))
      } else if(unit == "battleship"){
        return(writeLines(c(paste("Attack: 4"), paste("Defense: 4"), paste("Movement: 2"), paste("Cost: 20"), paste("Notes: Can participate in bombardment, in sea combat requires two hits to sink instead of one"))))
      } else if(unit == "aircraft carrier"){
        return(writeLines(c(paste("Attack: 1"), paste("Defense: 2"), paste("Movement: 2"), paste("Cost: 14"), paste("Notes: Can carry up to two fighters"))))
      } else if(unit == "carrier"){
        return(writeLines(c(paste("Attack: 1"), paste("Defense: 2"), paste("Movement: 2"), paste("Cost: 14"), paste("Notes: Can carry up to two fighters"))))
      }

  }else{

  if(unit == "infantry"){
    return(c("Attack:", 1, "Defense:", 2, "Movement:", 1, "Cost:", 3, "Notes: N/A"))
  } else if(unit == "artillery"){
    return(c("Attack:", 2, "Defense:", 2, "Movement:", 1, "Cost:", 4, "Notes: Each artillery can increase one infantry's attack from 1 to 2 in battle"))
  } else if(unit == "tank"){
    return(c("Attack:", 3, "Defense:", 3, "Movement:", 2, "Cost:", 5, "Notes:"))
  } else if(unit == "fighter"){
    return(c("Attack:", 2, "Defense:", 2, "Movement:", 4, "Cost:", 4, "Notes: N/A"))
  } else if(unit == "bomber"){
    return(c("Attack:", 4, "Defense:", 1, "Movement:", 6, "Cost:", 12, "Notes: Can conduct strategic bombing raids"))
  } else if(unit == "aa gun"){
    return(c("Attack:", "N/A", "Defense:", 1, "Movement:", 1, "Cost:", 5, "Notes: Only fires in air defense phase, rolls one dice for each attacking aircraft"))
  } else if(unit == "aagun"){
    return(c("Attack:", "N/A", "Defense:", 1, "Movement:", 1, "Cost:", 5, "Notes: Only fires in air defense phase, rolls one dice for each attacking aircraft"))
  } else if(unit == "industrial complex"){
    return(c("Attack:", "N/A", "Defense:", "N/A", "Movement:", "N/A", "Cost:", 15, "Notes: Units can only be placed on a territory with an industrial complex"))
  } else if(unit == "complex"){
    return(c("Attack:", "N/A", "Defense:", "N/A", "Movement:", "N/A", "Cost:", 15, "Notes: Units can only be placed on a territory with an industrial complex"))
  } else if(unit == "submarine"){
    return(c("Attack:", 2, "Defense:", 1, "Movement:", 2, "Cost:", 6, "Notes: Can submerge or conduct surprise strike"))
  } else if(unit == "destroyer"){
    return(c("Attack:", 2, "Defense:", 2, "Movement:", 2, "Cost:", 8, "Notes: Cancels enemy submarine abilities"))
  } else if(unit == "cruiser"){
    return(c("Attack:", 3, "Defense:", 3, "Movement:", 2, "Cost:", 12, "Notes: Can participate in bombardment"))
  } else if(unit == "battleship"){
    return(c("Attack:", 4, "Defense:", 4, "Movement:", 2, "Cost:", 20, "Notes: Can participate in bombardment, in sea combat requires two hits to sink instead of one"))
  } else if(unit == "aircraft carrier"){
    return(c("Attack:", 1, "Defense:", 2, "Movement:", 2, "Cost:", 14, "Notes: Can carry up to two fighters"))
  } else if(unit == "carrier"){
    return(c("Attack:", 1, "Defense:", 2, "Movement:", 2, "Cost:", 14, "Notes: Can carry up to two fighters"))
  }
  }
}



