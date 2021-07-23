Duh <- TRUE
while(Duh) {
  suppressWarnings(x <- as.numeric(readline("first number :")))
  ifelse(is.na(x), Duh <- TRUE, Duh <- FALSE) 
}
Duh <- TRUE
while(Duh) {
  allowed <- c("+", "-", "*", "/")
  operation <- readline("operation (+, -, *, or /): ")
  Duh <- (!any(allowed == operation))  
}  
Duh <- TRUE
while(Duh) {
  suppressWarnings(y <- as.numeric(readline("second integer :")))
  ifelse(is.na(y), Duh <- TRUE, Duh <- FALSE) 
}
switch(operation,
       "+" = ans <- (x + y),
       "-" = ans <- (x - y),
       "*" = ans <- (x * y),
       "/" = ans <- (x / y))
print(ans)
