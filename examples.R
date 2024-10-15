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

## matching problem ------------------------------------------------------------

match <- function(n){
  series <- (-1)^(0:n)/factorial(0:n)
  prob <- 1 - sum(series)
  return(prob)
}


plot(2:10, sapply(2:10, match), type = 'l')

## sensitivity/specificity example ---------------------------------------------

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
spec_val <- 0.7
prev_grid <- seq(from = 0.001, to = 0.2, length = 100)
sens_grid <- seq(from = 0.6, to = 0.99, length = 5)
col_grid <- colorRampPalette(c('#3041fc', '#370878'))(5)
probs <- c(expression('P(C | T)'), 
           expression(paste('P', bgroup('(', paste(C^C, '|', T^C, sep = ''), ')'), sep = '')))
par(mfrow = c(1, 2), mar = c(4, 5, 4, 1))
for(j in 1:length(sens_grid)){
  y <- sapply(prev_grid, function(.x){
    fn(sens = sens_grid[length(sens_grid) - j + 1], 
       spec = spec_val, 
       prev = .x)[1]
  })
  if(j == 1){
    plot(prev_grid, y, type = 'l', 
         xlab = 'prevalence', 
         ylab = probs[1],
         col = col_grid[length(sens_grid) - j + 1],
         main = paste('specificity', spec_val))
  }else{
    points(prev_grid, y, type = 'l', 
           col = col_grid[length(sens_grid) - j + 1])
  }
}
legend(x = 'bottomright',
       legend = sens_grid |>
         rev() |>
         round(2) |>
         as.character(), 
       title = 'sensitivity',
       col = col_grid |> rev(),
       lwd = 1)
for(j in 1:length(sens_grid)){
  y <- sapply(prev_grid, function(.x){
    fn(sens = sens_grid[j], 
       spec = spec_val, 
       prev = .x)[2]
  })
  if(j == 1){
    plot(prev_grid, y, type = 'l', 
         xlab = 'prevalence', 
         ylab = probs[2],
         col = col_grid[j],
         main = paste('specificity', spec_val))
  }else{
    points(prev_grid, y, type = 'l', 
           col = col_grid[j])
  }
}
legend(x = 'bottomleft',
       legend = sens_grid |>
         rev() |>
         round(2) |>
         as.character(), 
       title = 'sensitivity',
       col = col_grid |> rev(),
       lwd = 1)

## gambler's ruin --------------------------------------------------------------
par_default <- par()

# simulating a run
sim_fn <- function(x0, p, k, labs = T){
  # initialize
  x <- x0
  n <- 1
  
  # play until hitting 0 or k
  while((x[n] > 0) && (x[n] < k)){
    x <- append(x, x[n] + 2*rbinom(1, 1, p) - 1)
    n <- length(x)
  }
  
  if(x[n] == 0){
    outcome <- 'loss'
    col_rslt <- 'blue'
  }else{
    outcome <- 'win'
    col_rslt <- 'red'
  }
  
  # plot
  if(labs){
  plot(x, type = 's', 
       col = col_rslt,
       ylim = c(0, k),
       xlab = 'round',
       ylab = 'fortune',
       main = outcome)
  }else{
    plot(x, type = 's',
         col = col_rslt,
         ylim = c(0, k),
         xlab = NULL,
         ylab = NULL,
         main = NULL,
         xaxt = 'n',
         yaxt = 'n')
  }
    
  # output run
  return(x)
}

# play once
par(par_default)
sim_fn(5, 0.5, 20)

# repeat many times
par(mfrow = rep(10, 2), mar = rep(0, 4))
lapply(1:100, function(i){
  sim_fn(x0 = 5, p = 0.5, k = 20, labs = F)
})

# how many simulations until a win?
par(par_default)
play_until_win <- function(x0, p, k){
  run <- 0
  n <- 0
  while(rev(run)[1] == 0){
    run <- sim_fn(x0, p, k)
    n <- n + 1
    print(n)
  }
}
play_until_win(5, 0.5, 20)

# solution
aj <- function(j, k, p){
  if(p == 1/2){
    prob <- j/k
  }else{
    odds <- (1-p)/p
    prob <- (1 - odds^j)/(1 - odds^k)
  }
  return(prob)
}

# compute solution for one setting
aj(5, 20, 0.45)

# compare solution paths
k_val <- 20
p_grid <- seq(from = 0.4, to = 0.6, by = 0.01)
col_grid <- colorRampPalette(c('blue', 'grey', 'red'))(length(p_grid))
for(i in 1:length(p_grid)){
  x <- 1:k_val
  y <- aj(x, k_val, p_grid[i])
  if(i == 1){
    plot(x, y, type = 'l',
         xlab = 'starting value',
         ylab = paste('P(make it to ', k_val, ')', sep = ''),
         col = col_grid[i])
  }else{
    points(x, y, type = 'l', col = col_grid[i])
  }
}
