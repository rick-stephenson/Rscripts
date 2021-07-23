## The RunKitty function is equivalent to the Loop in the Sketch
## or "main" in C.  This is the top level
RunKitty <- function() {
  currentLocation <- 90
  positive <- TRUE
  y <- 0
  
  ##watchForMovement function  When it triggers, turn on laser, move laser
  for(i in 1:20) {
    y <- GetNewLocation(currentLocation, positive)
    currentLocation <- moveLaser(y, currentLocation, positive)
    positive <- !positive
  }  
  
}

##function to move the laser to the new location
##takes in a boolean for positive or negative,
##and the new location to run to
moveLaser <- function(y, currentLocation, positive) {
  stepSize <- 2
  ##stepSpeed <- 500
  
  if(positive) {
    while(currentLocation < y) {
      currentLocation <- currentLocation + stepSize
      ##pause for stepSpeed lenght of time
      cat(".")
    }
    print(currentLocation)
  } else {
    while(currentLocation > y) {
      currentLocation <- currentLocation - stepSize
      ##pause for stepSpeed lenght of time
      cat(".")
    }
    print(currentLocation)
  }
  return(currentLocation)
}



##This function takes in a boolean for positive or negative
##and a current location.  It then adds or subtracts from the location
##by at least 20 degrees

GetNewLocation <- function(y, positive) {
  distance <- 0
  
  distance <- sample(20:100, 1, TRUE)
  cat(distance)
  if(positive){
    y <- (y + distance)
    if(y > 150) y <- 150
  } else {
    y <- (y - distance)
    if(y < 30) y <- 30
  }
  return(y)
}