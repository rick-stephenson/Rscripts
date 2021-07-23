i <- 0
success <- 1:100000

d <- 1
total.roll <- 0

monster <- as.integer(readline("Enter monster strength or press 2 for dice roll."))

dice.num <- as.integer(readline("How many dice are you rolling? "))
sides <- 1:dice.num
sides[1:dice.num] <- 0
roll <- 1:dice.num
roll[1:dice.num] <- 0

for(d in 1:dice.num) {
  cat("How big is die ",d)
  sides[d] <- as.integer(readline("? "))
}

plusses <- as.integer(readline("What is the sum of all the plusses? "))

if(monster != 2) {
  for(i in 1:100000) {
    for(d in 1:dice.num) {
      roll[d] <- sample(1:sides[d],1,TRUE)
    }
    total.roll <- (sum(roll[1:dice.num]) + plusses)
    if(total.roll >= monster) success[i] <- 1 else success[i] <- 0
  }
  result <- sum(success)
  print(result/1000)
  
} else {
  for(d in 1:dice.num) {
    roll[d] <- sample(1:sides[d],1,TRUE)
  }
  total.roll <- (sum(roll[1:dice.num]) + plusses)
  cat("You rolled a ", total.roll)
} #end else


