#### Skript 11.1


SimulateARIMA <- function(n, d = 1, alpha = 0, phi = c(), theta = c(), sigma = 1, init.obs = c()) {
  
  p <- length(phi) # rad autoregresni posloupnosti
  q <- length(theta) # rad posloupnosti klouzavych souctu
  
  init.n <- length(init.obs) # pocet pocatecnich pozorovani
  
  e <- rnorm(n = n, mean = 0, sd = sigma) # simulace normalniho bileho sumu
  
  x <- rep(NA, n) 
  if (init.n > 0) {
    x[1:init.n] <- init.obs
  }
  
  df <- cumsum(diff(x))
  
  for (t in (init.n + 1):n-1) {
    df[t] <- alpha + sum(phi * df[(t - 1):(t - p)]) + sum(theta * e[(t - 1):(t - q)]) + e[t]  ### diference radu d
  }
  
  for(i in 1:length(df)){
    x[i] <- cumsum(df[i:(i+d)])[d+1]
  }
  
  res <- data.frame(t = 1:n, e = e, df = c(0,df), x = c(x)) 
  
  return(res)
}

as<- SimulateARIMA(n = 100, d=2)

