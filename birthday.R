## birthday problem ------------------------------------------------------------

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

