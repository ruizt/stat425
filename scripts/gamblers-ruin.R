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
  sim_fn(x0 = 5, p = 0.55, k = 20, labs = F)
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

