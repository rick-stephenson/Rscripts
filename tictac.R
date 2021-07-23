tictac <- function() {
## Tic tac toe game

##initialize board

board <- matrix(c(1:9), nrow = 3, ncol = 3, byrow = TRUE)

##player's first move
ppick <- getPlayerMove(board)
board <- replace(board, board == ppick, 100)
displayBoard(board)

##computer turn 1.........................................................1
##pick center or a random corner if player picks center
readline("Computer's First move.  Hit Enter")
chance <- sample(10,1)
if(chance == 1) {
  x <- 0
  while(!any(board == x)) x <- sample(9, 1)
  cPick <- which(board == x, arr.ind = TRUE)
  board <- replace(board, board == x, -100)
} else {

if(board[2,2] == 5) {
  board <- replace(board, board == 5, -100)
  cPick <- c(2,2)
} else {
  x <- sample(c(1,3,7,9), 1)
  cPick <- which(board == x, arr.ind = TRUE)
  board <- replace(board, board == x, -100)
}
}

##player's second move
ppick <- getPlayerMove(board)
lastPick <- which(board == ppick, arr.ind = TRUE)
board <- replace(board, board == ppick, 100)
displayBoard(board)

##computer's second turn..................................................2
##check for block, then go for block forks
readline("Computer's Second move.  Hit Enter")
##check for a possible win by player
##if possible player win, block it!
## possiblity looks like ("R1", "Player")
## may turn this into a function??
boardState <- getBoardState(board)
possibility <- checkPossibilities(boardState)
if(possibility[2] == "player") {
  block <- blockPlayerWin(board, possibility[1])
  board <- replace(board, board == block, -100)
} else {
  chance <- sample(5,1)
  if(chance == 1) {
    x <- 0
    while(!any(board == x)) x <- sample(9, 1)
    board <- replace(board, board == x, -100)
  } else {
  
##boardState:
##r1,r2,r3,c1,c2,c3,d1,d2
## 1, 2, 3, 4, 5, 6, 7, 8

##if computer has middle, try for two in a row side
##else pick the short (forking) corner
if(board[2,2] == -100){
  ##see if a win possiblity side exists
  ##if not pick the opposite corner
  if((boardState[2] > 0) && (boardState[5] > 0)) {
    if(board[1,2] == 100) {
      if(board[2,1] == 100) board[1,1] <- -100 else board[1,3] <- -100
    } else {
      if(board[2,1] == 100) board[3,1] <- -100 else board[3,3] <- -100
    }
  } else {
  ##pick a random side, see if it has a win possibility then pick one
  ##of those spots
  swap <- TRUE
  while(swap) {
      swap <- FALSE
      guess <- sample(4,1)
      switch(guess,
      if((boardState[5] == -90) && (board[1,2] == 2)) board[1,2] <- -100 else swap <- TRUE,
      if((boardState[5] == -90) && (board[3,2] == 8)) board[3,2] <- -100 else swap <- TRUE,
      if((boardState[2] == -90) && (board[2,1] == 4)) board[2,1] <- -100 else swap <- TRUE,
      if((boardState[2] == -90) && (board[2,3] == 6)) board[2,3] <- -100 else swap <- TRUE)
    }
  }
} else {
###########computer does not have middle (or need to block)
##only way to get here is for player to pick middle and corner opposite computer
  guess <- sample(2,1)
  if(sum(cPick) == 4) switch(guess,
                            board[1,1] <- -100,
                            board[3,3] <- -100) else {
      switch(guess,
             board[1,3] <- -100,
             board[3,1] <- -100)                        
      }
                        
}
}
} ## <------end of "can player win" else statement.  Rest of comp turn two goes above here

##player's third move
ppick <- getPlayerMove(board)
lastPick <- which(board == ppick, arr.ind = TRUE)
board <- replace(board, board == ppick, 100)
displayBoard(board)

##check for player win
boardState <- getBoardState(board)
if(any(boardState == 300)) {
  print("You Win!")
  return()
}

##Computer's third move......................................................3
##check for win condition or block condition

readline("Computer's Third move.  Hit Enter")

##Check for chance to win first, then
##check for a possible win by player
##if possible player win, block it!
## possiblity looks like ("R1", "Player")

##get board state and determine if computer can win or block
boardState <- getBoardState(board)
possibility <- checkPossibilities(boardState)

##if computer can win, it takes the win
if(possibility[2] == "computer") {
  
  ##boardState:
  ##r1,r2,r3,c1,c2,c3,d1,d2
  ## 1, 2, 3, 4, 5, 6, 7, 8
  
##switch reads the line name for the possible win
##finds the spot that's open and writes "-100" to that spot
  switch(possibility[1],
         R1 = board[1, which(board[1,] != -100)] <- -100,
         R2 = board[2, which(board[2,] != -100)] <- -100,
         R3 = board[3, which(board[3,] != -100)] <- -100,
         C1 = board[which(board[,1] != -100), 1] <- -100,
         C2 = board[which(board[,2] != -100), 2] <- -100,
         C3 = board[which(board[,3] != -100), 3] <- -100,
         D1 = board[which(c(board[1,1], board[2,2], board [3,3]) != -100), 
                    which(c(board[1,1], board[2,2], board [3,3]) != -100)] <- -100,
         D2 = board <- replace(board, board == diagC(board), -100))
  displayBoard(board)
  return("computer wins")
  
} else {   ## <-------------end of "can computer win" if
  
###find the player possible win and block it
  if(possibility[2] == "player") {
    block <- blockPlayerWin(board, possibility[1])
    board <- replace(board, board == block, -100)
  } else{  ##<------------------end of "block player" if
    
    ##Can't win, can't block player, pick a random spot
    x <- 0
    while(!any(board == x)) x <- sample(9, 1)
    board <- replace(board, board == x, -100)
  }
} ## <--------end of computer turn 3 else

##player's fourth move
ppick <- getPlayerMove(board)
lastPick <- which(board == ppick, arr.ind = TRUE)
board <- replace(board, board == ppick, 100)
displayBoard(board)

##check for player win
boardState <- getBoardState(board)
if(any(boardState == 300)) {
  return("You Win!")
}

## computer's fourth move............................................4
##check for win condition or block condition

readline("Computer's Fourth move.  Hit Enter")

##Check for chance to win first, then
##check for a possible win by player
##if possible player win, block it!
## possiblity looks like ("R1", "Player")

##find the win and take it
boardState <- getBoardState(board)
possibility <- checkPossibilities(boardState)
if(possibility[2] == "computer") {
  
  ##boardState:
  ##r1,r2,r3,c1,c2,c3,d1,d2
  ## 1, 2, 3, 4, 5, 6, 7, 8
  
  ##switch reads the line name for the possible win
  ##finds the spot that's open and writes "-100" to that spot
  switch(possibility[1],
         R1 = board[1, which(board[1,] != -100)] <- -100,
         R2 = board[2, which(board[2,] != -100)] <- -100,
         R3 = board[3, which(board[3,] != -100)] <- -100,
         C1 = board[which(board[,1] != -100), 1] <- -100,
         C2 = board[which(board[,2] != -100), 2] <- -100,
         C3 = board[which(board[,3] != -100), 3] <- -100,
         D1 = board[which(c(board[1,1], board[2,2], board [3,3]) != -100), 
                    which(c(board[1,1], board[2,2], board [3,3]) != -100)] <- -100,
         D2 = board <- replace(board, board == diagC(board), -100))
  displayBoard(board)
  return("computer wins")
  
} else {   ## <-------------end of "can computer win" if
  
  ###find the player win and block it
  if(possibility[2] == "player") {
    block <- blockPlayerWin(board, possibility[1])
    board <- replace(board, board == block, -100)
  } else{  ##<------------------end of "block player" if
    
    ##Can't win, can't block player, pick a random spot
    x <- 0
    while(!any(board == x)) x <- sample(9, 1)
    board <- replace(board, board == x, -100)
  }
} ## <--------end of computer turn 4 else

##player's fifth move
ppick <- getPlayerMove(board)
lastPick <- which(board == ppick, arr.ind = TRUE)
board <- replace(board, board == ppick, 100)
displayBoard(board)

##check for player win
boardState <- getBoardState(board)
if(any(boardState == 300)) {
  return("You Win!")
}
cat("\n")
print("It's a draw!")

} ## <------------end of main function



###########################################################################
##function that takes in the board and the line name string
##and returns the number of the location needed to block win
blockPlayerWin <- function(board, line) {
  switch(line,
         R1 = play <- board[1,which(board[1,] < 100)],
         R2 = play <- board[2,which(board[2,] < 100)],
         R3 = play <- board[3,which(board[3,] < 100)],
         C1 = play <- board[which(board[,1] < 100),1],
         C2 = play <- board[which(board[,2] < 100),2],
         C3 = play <- board[which(board[,3] < 100),3],
         D1 = play <- diagA(board),
         D2 = play <- diagB(board))
        return(play)
}

###########################################################
## Evaluate value of location to block player win along
## diagnal A and returns that value
diagA <- function(board) {
  play <- which(c(board[1,1], board[2,2], board [3,3]) < 100)
  switch(play,
         play <- 1,
         play <- 5,
         play <- 9)
  return(play)
}


###########################################################
## Evaluate value of location to block player win along
## diagonal B and returns that value
diagB <- function(board) {
  play <- which(c(board[1,3], board[2,2], board [3,1]) < 100)
  switch(play,
         play <- 3,
         play <- 5,
         play <- 7)
  return(play)
}

###########################################################
## Finds the value in the second diagonal that is not -100
## and returns that value
diagC <- function(board) {
  play <- which(c(board[1,3], board[2,2], board [3,1]) != -100)
  switch(play,
         play <- 3,
         play <- 5,
         play <- 7)
  return(play)
}

########################################################################
##asks for a player move and returns the
##number of the location
getPlayerMove <- function(board) {
  displayBoard(board)
  invalid <- TRUE
  while(invalid) {
    pick <- readline("Your turn, pick a spot \n")
    invalid <- !any(board == pick)
    if(invalid) cat("please pick a valid square", "\n")
  }
  return(pick)
}
  



############################################################################
##Add rows, columns and diags
##Rows are in postions r1, r2, r3, columns are c1,c2,c3, diags are d1, d2

getBoardState <- function(board) {
  r1 <- sum(board[1,])
  r2 <- sum(board[2,])
  r3 <- sum(board[3,])
  c1 <- sum(board[,1])
  c2 <- sum(board[,2])
  c3 <- sum(board[,3])
  d1 <- sum(board[1,1], board[2,2], board [3,3])
  d2 <- sum(board[1,3], board[2,2], board [3,1])
  return(c(r1, r2, r3, c1, c2, c3, d1, d2))
}
  
############################################################################
##Takes in a vector of line sums
##Checks first for a player possible win (line = 200) and
##returns the line and "player" in a vector
##Then checks for computer possible win (line = -200) and returns
##line and "computer" in a vector
##"computer" overwrites player because this gets called on computer's turn

##checks for player possible win
checkPossibilities <- function(state) {
  
possiblWin <- c("A", "B")  ##<---------may not need this?
    
  if (state[1] > 199) possiblWin <- c("R1", "player")
  if (state[2] > 199) possiblWin <- c("R2", "player")
  if (state[3] > 199) possiblWin <- c("R3", "player")
  if (state[4] > 199) possiblWin <- c("C1", "player")
  if (state[5] > 199) possiblWin <- c("C2", "player")
  if (state[6] > 199) possiblWin <- c("C3", "player")
  if (state[7] > 199) possiblWin <- c("D1", "player")
  if (state[8] > 199) possiblWin <- c("D2", "player")
  
  ##checks for computer possible win
  if (state[1] < -190) possiblWin <- c("R1", "computer")
  if (state[2] < -190) possiblWin <- c("R2", "computer")
  if (state[3] < -190) possiblWin <- c("R3", "computer")
  if (state[4] < -190) possiblWin <- c("C1", "computer")
  if (state[5] < -190) possiblWin <- c("C2", "computer")
  if (state[6] < -190) possiblWin <- c("C3", "computer")
  if (state[7] < -190) possiblWin <- c("D1", "computer")
  if (state[8] < -190) possiblWin <- c("D2", "computer")
  
  return(possiblWin)
}

############################################################################
##Displays a board with x's and o's
displayBoard <- function(board) {
  displaybrd <- as.data.frame(board)
  displaybrd <- replace(displaybrd, displaybrd == -100, "O")
  displaybrd <- replace(displaybrd, displaybrd == 100, "X")
  colnames(displaybrd) <- c(".", ".", ".")
  print(displaybrd, row.names = FALSE)
}


