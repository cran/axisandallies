.land_simulate_hidden <- function(off_infantry = 0, off_artillery = 0, off_tanks = 0, off_fighters = 0, off_bombers = 0, def_infantry = 0, def_artillery = 0, def_tanks = 0, def_fighters = 0, def_bombers = 0, aagun = FALSE, bombarding_battleships = 0, bombarding_cruisers = 0){
  if(aagun){
    off_fighters = rbinom(1, off_fighters, 5/6)
    off_bombers = rbinom(1, off_bombers, 5/6)
  }

  def_units = def_infantry + def_artillery + def_tanks + def_fighters + def_bombers
  off_units = off_infantry + off_artillery + off_tanks + off_fighters + off_bombers
  off_ground_units = off_infantry + off_artillery + off_tanks

  bombardment = TRUE

  while(def_units > 0 & off_units > 0){
    if(off_infantry <= off_artillery){
      off_hits = rbinom(1, off_infantry, 2/6) + rbinom(1, off_artillery, 2/6) + rbinom(1, off_tanks, 3/6) + rbinom(1, off_fighters, 3/6) + rbinom(1, off_bombers, 4/6)
    } else if(off_infantry > off_artillery & off_artillery > 0){
      off_hits = rbinom(1, off_infantry - off_artillery, 1/6) + rbinom(1, off_artillery*2, 2/6) + rbinom(1, off_tanks, 3/6) + rbinom(1, off_fighters, 3/6) + rbinom(1, off_bombers, 4/6)
    } else{
      off_hits = rbinom(1, off_infantry, 1/6) + rbinom(1, off_artillery, 2/6) + rbinom(1, off_tanks, 3/6) + rbinom(1, off_fighters, 3/6) + rbinom(1, off_bombers, 4/6)
    }
    def_hits = rbinom(1, def_infantry, 2/6) + rbinom(1, def_artillery, 2/6) + rbinom(1, def_tanks, 3/6) + rbinom(1, def_fighters, 4/6) + rbinom(1, def_bombers, 1/6)

    if(bombardment){
      off_hits = off_hits + rbinom(1, bombarding_battleships, 4/6) + rbinom(1, bombarding_cruisers, 3/6)
      bombardment = bombardment - 1
    }

    while(def_hits > 0){
      def_hits = def_hits -1

      if(off_ground_units > 1){

        if(off_infantry > 0){
          off_infantry = off_infantry - 1
        } else if(off_artillery > 0){
          off_artillery = off_artillery - 1
        } else if(off_tanks > 0){
          off_tanks = off_tanks - 1
        } else if(off_fighters > 0){
          off_fighters = off_fighters - 1
        } else if(off_bombers > 0){
          off_fighters = off_bombers - 1
        } else{
          return(0)
          break
        }

      } else{

        if(off_fighters > 0){
          off_fighters = off_fighters - 1
        } else if(off_bombers > 0){
          off_bombers = off_bombers - 1
        } else if(off_infantry > 0){
          off_infantry = off_infantry - 1
        } else if(off_artillery > 0){
          off_artillery = off_artillery - 1
        } else if(off_tanks > 0){
          off_tanks = off_tanks - 1
        } else{
          return(0)
          break
        }

      }

    }

    while(off_hits > 0){
      off_hits = off_hits -1
      if(def_bombers > 0){
        def_bombers = def_bombers - 1
      } else if(def_infantry > 0){
        def_infantry = def_infantry - 1
      } else if(def_artillery > 0){
        def_artillery = def_artillery - 1
      } else if(def_tanks > 0){
        def_tanks = def_tanks - 1
      } else if(def_fighters > 0){
        def_fighters = def_fighters - 1
      } else{
        return(1)
        break
      }
    }

    def_units = def_infantry + def_artillery + def_tanks + def_fighters + def_bombers
    off_units = off_infantry + off_artillery + off_tanks + off_fighters + off_bombers

    if(off_units == 0){
      return(0)
    } else if(def_units == 0){
      return(1)
    }

  }

}

