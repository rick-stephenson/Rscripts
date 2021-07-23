## Saving kate and KateCol functions into one source file "smooch"
## updated to add ability to choose plot to view
## in source file muchasSmooches

##import data into a working data frame to run functions

##set up as a standalone function that gets called and outputs
##a .csv file to the working directory

require(manipulate)

##******************************************************************
## pass in a time column and an energy column
## output is a vector with the cumulitive energy under the curve
## using trapezoid method
## instantaneous (ie output of each trapezoid) is stored in "energy"
## but not returned
kateCol <- function(x,y)
{
  l <- length(x)
  energy <- (1:l-1)
  cumul <- (1:l-1)
  energy <- 0
  cumul <- 0
  
  for(i in 2:l){
    energy[i-1] <- ((x[i] - x[i - 1]) * (y[i] + y[i - 1])/2)
    cumul[i-1] <- sum(energy)
  }
  return(cumul)
}

##**********************************************************************
## a function that can
##take an arbitraily long data frame with
##time stamps in the first column
##and data in the rest
##if time is not in first column, data and column headings will be wonky

##function "kate" in script file smooch
kate <- function(x)
{
##Loops through and itegrates every column in the data frame
##using the "kateCol" function in KateOneColumnFunction script

  cols <- ncol(x)
  rows <- nrow(x)
  walkOutput <- data.frame(matrix(ncol = cols-1, nrow = rows-1))
  grtitles <- colnames(x)  ##grtitles(i) = has "time" column heading

##loops through each column to calculate and plot cumulative integral
  for (i in 2:cols) {
    walkOutput[[i-1]] <- kateCol(x[,1], x[,i])
    ##plot(walkOutput[[i-1]], type = "l", ylab = "energy", main = grtitles[i])
    
    ##uncomment above plot if you want to see some graphs
    ##rstudio limit is 30 graphs, so it only plots the last 30 columns
    ##and it takes a while.
    ##to make an individual plot, uncomment "return" at end of function
    ##and ouput to a variable
    ##e.g.  "OutputVariable <- kate(data)"
    ##to get a working data table
  
  }
colnames(walkOutput) <- grtitles[2:cols]
write.csv(walkOutput, file = "KateEnergyOutput.csv", row.names = FALSE)

##uncomment return if you want to output to a variable
##if it's there and not sent to a variable, it overwhelms the console print
##return(walkOutput)
print("Success!")
print("data can be found in file: KateEnergyOutput.csv in working directory")
print("Change this file name now, as the next run will over write it.")
print("Choose a plot on the right to view.")

##Choose which plot to view
Picks<- as.list(colnames(walkOutput))
manipulate(
  plot(as.matrix(walkOutput[,factor]),type = "l", ylab = "energy", main = factor),
  factor = picker(Picks, label = "pick a graph")
)

}
