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