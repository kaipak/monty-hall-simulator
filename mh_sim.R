library(plotly)

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


VisualizeGames <- function(num_games=10) {
  wl.hold <- SimulateNGames(switch=FALSE, n=num_games)
  outcome <- c("win", "loss")
  record <- c(sum(wl.hold), length(wl.hold) - sum(wl.hold))
  hold.rec <- data.frame(outcome, record)
  wl.switch <- SimulateNGames(switch=TRUE, n=num_games)
  record <- c(sum(wl.switch), length(wl.switch) - sum(wl.switch))
  switch.rec <- data.frame(outcome, record)
  
  print(hold.rec[hold.rec$outcome == 'wins']$record)
  print(sum(hold.rec$record))
  fig1 <- plot_ly(
    domain = list(x = c(0, 0.45), y = c(0, 1)),
    value = hold.rec[hold.rec$outcome == 'win',]$record / sum(hold.rec$record),
    title = list(text = "Win Percentage (Hold!)"),
    type = "indicator",
    mode = "gauge+number",
    gauge = list(
      axis = list(range = list(NULL, 1.0))
    )) 
  fig1 <- fig1 %>% layout(margin = list(l=20,r=30))
  
  fig2 <- plot_ly(
    domain = list(x = c(0.55, 1), y = c(0, 1)),
    value = switch.rec[switch.rec$outcome == 'win',]$record / sum(switch.rec$record),
    title = list(text = "Win Percentage (Switch!)"),
    type = "indicator",
    mode = "gauge+number",
    gauge = list(
      axis = list(range = list(NULL, 1.0))
    )) 
  fig2 <- fig2 %>% layout(margin = list(l=20,r=30))
  
  fig <- subplot(fig1, fig2)
  
  return(fig)
}