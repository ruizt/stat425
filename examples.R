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

## sensitivity/specificity

fn <- function(sens, spec, prev){
  prob1 <- sens*prev/(sens*prev + (1 - spec)*(1 - prev))
  prob2 <- spec*(1 - prev)/(spec*(1 - prev) + (1 - sens)*prev)
  out <- c(present.positive = prob1,
           absent.negative = prob2)
  return(out)
}

# example (a) from class
fn(sens = 0.95, spec = 0.99, prev = 0.005)

# example (b) from class
fn(sens = 0.99, spec = 0.89, prev = 0.005)

# p(present|positive) as function of prevalence and sensitivity in case (a)
prev_grid <- seq(from = 0.001, to = 0.1, length = 100)
sens_grid <- seq(from = 0.6, to = 0.99, length = 10)
col_grid <- colorRampPalette(c('#3041fc', '#370878'))(10) |> rev()
for(j in 1:length(sens_grid)){
  y <- sapply(prev_grid, function(.x){
    fn(sens = sens_grid[length(sens_grid) - j + 1], 
       spec = 0.99, 
       prev = .x)[1]
  })
  if(j == 1){
    plot(prev_grid, y, type = 'l', 
         xlab = 'prevalence', 
         ylab = 'P(C|T)',
         col = col_grid[j])
  }else{
    points(prev_grid, y, type = 'l', col = col_grid[j])
  }
}

# p(present|positive) as function of prevalence and specificity in case(b)
prev_grid <- seq(from = 0.001, to = 0.5, length = 100)
spec_grid <- seq(from = 0.8, to = 0.95, length = 10)
col_grid <- colorRampPalette(c('#3041fc', '#370878'))(10) |> rev()
for(j in 1:length(spec_grid)){
  y <- sapply(prev_grid, function(.x){
    fn(sens = 0.99, 
       spec = spec_grid[length(spec_grid) - j + 1], 
       prev = .x)[1]
  })
  if(j == 1){
    plot(prev_grid, y, type = 'l', 
         xlab = 'prevalence', 
         ylab = 'P(C|T)',
         col = col_grid[j])
  }else{
    points(prev_grid, y, type = 'l', col = col_grid[j])
  }
}

## gambler's ruin

aj <- function(j, k, p){
  if(p == 1/2){
    prob <- j/k
  }else{
    odds <- (1-p)/p
    prob <- (1 - odds^j)/(1 - odds^k)
  }
  return(prob)
}

aj(5, 10, 1/3)

x <- 1:20
y <- aj(x, 20, 0.55)
plot(x, y, type = 'l')
