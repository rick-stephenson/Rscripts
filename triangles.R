cornerA <- c(0,0)
cornerB <- c(500,0)
cornerC <- c(250, 433)
plot(1:500, 1:500, type = "n")
points(cornerA[1], cornerA[2])
points(cornerB[1], cornerB[2])
points(cornerC[1], cornerC[2])
here <- c(200, 54)
points(here[1], here[2])
for(i in 1:5000) {
  nv <- sample(3,1)
  switch(nv,
         here <- c(here[1]/2, here[2]/2),
         here <- c((here[1] + ((500 - here[1])/2)), here[2]/2),
         here <- c(((250 - here[1])/2) + here[1],(here[2] + ((433- here[2])/2))))
  points(here[1], here[2])  
}
