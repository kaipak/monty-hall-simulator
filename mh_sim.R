
SimulateRound <- function(switch=FALSE) {
  
  game.sim <- c(rep(FALSE, 3))
  game.sim[sample.int(3, 1)] <- TRUE
  
  player.guess <- sample.int(3, 1)
  
  # Player chooses to hold
  if (switch == FALSE) {
    if (game.sim[player.guess] == TRUE) {
      return(1)
    } else {
      return(0)
    }
    # Player Switches
  } else {
    
    if (game.sim[player.guess] == TRUE) {
      return(0) 
    } else {
      return(1)
    }
  }
}

SimulateNGames <- function(switch=FALSE, n=10) {
  win.loss <- replicate(n, SimulateRound(switch=switch))
  return(win.loss)
}