## birthday problem

bday <- function(n){
  bday <- 1
  nm1 <- n - 1
  for(j in 1:nm1){
    bday <- bday*((365-j)/365)
  }
  bday <- 1 - bday
  return(bday)
}

bday(23)

plot(2:50, sapply(2:50, bday), type = 'l')
abline(h = 0.5, col = 'red')

## matching problem

match <- function(n){
  series <- (-1)^(0:n)/factorial(0:n)
  prob <- 1 - sum(series)
  return(prob)
}


plot(2:10, sapply(2:10, match), type = 'l')
