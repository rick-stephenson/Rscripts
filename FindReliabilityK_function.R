findReliabilityK <- function(x, alpha, target, side) {
  
## inputs are a vector x to find the reliability from sample
## x needs to be normal (shapiro test performed and warning thrown if fails at .05)
## target is one sided spec
## side tells us if we want upper or lower spec
## side = 1 means lower spec, side = 2 means upper spec
## calculation is made at the 95% confidence level
  
  
step <- 0.01
guess <- .9

if(!(side == 1 || side == 2)) stop("enter 1 for lower spec or 2 for upper")
normal <- shapiro.test(x)
if(normal[2] < .05) warning("data may not be normal", immediate.= TRUE)

if((side == 1)) {
  ## looking for a lower spec, ie data is above spec.
  ## driving P higher drives the calculated lower spec down
  where <- 4
for (i in 1:1000){
  swap <- normtol.int(x, alpha, guess, 1)
  if (swap[where]>target){  #Need to add to guess, direction needs to be postive    
    if (step < 0) { #if just came down (ie guess is neg), we overshot
      step <- step * (-.1) #direction is now positive and 1/10th
    }  #if direction + we keep going in same direction, so no else
  } else { #else means calculated lower end < target, so guess needs to decrease
    if (step > 0) { #if just came up (ie guess is pos), we overshot
      step <- step * (-.1) #direction is now negative and 1/10th
    }  ## if step is negative, reliabilty is going the right direction, so no else
  }
  guess.spec <- as.numeric(swap[where])
  if(isTRUE(all.equal(target, guess.spec))) break
  guess <- guess+step
  if (guess >= 1) {
    step <- step * (-.1)
    guess <- guess+step
  }
}
} else {
  where <- 5
  for (i in 1:1000){
    swap <- normtol.int(x, alpha, guess, 1)
    if (swap[where]<target){  #Need to add to guess, direction needs to be pos    
      if (step < 0) { #if just decreased (ie step is neg), we overshot
        step <- step * (-.1) #direction is now negative and 1/10th
      }  #if direction positive we keep going in same direction, so no else
    } else { #else means upper > target, so guess needs to decrease
      if (step > 0) { #if just came up (ie step is pos), we overshot
        step <- step * (-.1) #direction is now negative and 1/10th
      }
    }
    guess.spec <- as.numeric(swap[where])
    if(isTRUE(all.equal(target, guess.spec))) break
    guess <- guess+step
    if (guess >= 1) {
      step <- step * (-.1)
      guess <- guess+step
    }
}
}
return(swap)
}

