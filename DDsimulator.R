## Simple hit point rolling simulator
## you and computer start with equal hit points
## and roll 2D6 until you quit and do that much damage
## if you roll a 1 on either die, you miss and do no damage

DDsimulator <- function() {
  player.hp <- 100
  computer.hp <- 100
  player.turn <- TRUE  ##Player gets to go first
  this.roll <- 0
  
  cat("Welcome to dice roller.", '\n')
  cat("You go first.", '\n')
  
  repeat {
    cat("\n")
    if(player.turn) cat("Your turn", '\n') else cat("computer's turn", '\n')
    cat("press enter to continue", '\n')
    readline()
    this.roll <- take.a.turn(player.turn, player.hp, computer.hp)
    if(player.turn) {
      computer.hp <- computer.hp - this.roll
      if(computer.hp < 0) return("You win!")
    } else {
      player.hp <- player.hp - this.roll
      if(player.hp <0) return("You Lose")
    }
    ifelse(player.turn, player.turn <- FALSE, player.turn <- TRUE)
  }
}


## generic take a turn function
## rolls the dice once and checks for a miss then 
## decides if desired to roll again based on stop.roll which
## is set but subroutines
## If player.turn is TRUE, asks the player if they want to keep going
## if player.turn is FALSE, calls the ai decision making function
## if miss is rolled, this returns 0
##if player or computer stops, the sum of the rolls is returned

take.a.turn <- function(player.turn, p.score, c.score) {

roll.n <- c(0,0)
total <- 0
stop.roll <- FALSE

while(!stop.roll) {
  roll.n <- roll.dice()
  cat("your HP:", p.score, "computer HP:", c.score, "\n")
  if(roll.n[1] == 0) {
    cat("Missed!", "\n")
    total <- 0
    stop.roll <- TRUE
  } else {
    total <- total + sum(roll.n)
    cat("The roll is ")
    cat(roll.n, "\n")
    cat("The total for this turn is ")
    cat(total, "\n")
    ifelse(player.turn, stop.roll <- stop.rolling(), stop.roll <- ai.decision(p.score, c.score, total))
    if(!player.turn && stop.roll) cat("\n", "computer stops with ", total, "\n")
  }
}
return(total)
}

## Function to roll two dice once and check for a 1
## returns a two element vector of numbers or 0

roll.dice <- function() {
  roll <- c(7,7)
  
  roll <- sample(6, 2, TRUE)
  if((roll[1] == 1) || (roll[2] == 1)) roll <- 0
  return(roll)
}


## when called, "stop.rolling" will query user to stop rolling or not
## function returns a boolean.  If user wants to stop, returns "true"

stop.rolling <- function() {

  keep.rolling <- 0
  stop.roll <- FALSE

while(!((keep.rolling=="1")||(keep.rolling=="2"))) {
  keep.rolling <- readline("press 1 to keep rolling or 2 to stop ")
}
if(keep.rolling == 2) stop.roll = TRUE
return(stop.roll)
}


##function that decides if the computer will stop rolling based on the
##current value of its roll and the current hit points of player and AI
##attitude is 1, 2, or 3 (low, medium, high riskiness factor)
##based on the delta in hit point values.  If risky value is high,
##AI will push for a a higher score.
##if AI decides to stop rolling, return TRUE

ai.decision <- function(p.score, c.score, current.roll) {
  attitude <- 2

  if(current.roll > p.score) return(TRUE)
  if(p.score < 17) return(FALSE)
  
  if((c.score < 35) && (p.score > c.score)) attitude <- 3 else if((c.score - p.score) > 20) attitude <- 1

  if(attitude == 1) {
    if(current.roll > 16) return(TRUE)
    return(FALSE)
  } else {
    if(attitude == 3) {
      if((p.score - current.roll) < 9) return(FALSE)
      if(current.roll > 40) return(TRUE)
      return(FALSE)
    } else {
      ## normal attitude = 2 case
      ## add a 1 in 20 chance to roll again regardless
      ## and a a 1 in 20 chance of stopping regardless
      x <- sample(20,1,TRUE)
      if(x == 1) return(FALSE)
      if(x == 20) return(TRUE)
      if(current.roll > 26) return(TRUE)
      return(FALSE)
    }
  }
}

