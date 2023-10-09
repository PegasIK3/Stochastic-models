# Ukol ze cviceni 6

P <- rbind(c(0.8, 0.2), c(0.4, 0.6)) # matice pravdepodobnosti prechodu
R <- rbind(c(10000, -7000), c(10000, -3000)) # matice oceneni prechodu


hruby_odhad <- function(n, P, R, v0) {
  s <- nrow(P) # pocet stavu
  
  # soustava rovnic alpha*P=alpha, neboli (P-I)'alpha'=0    #### funkce pro nalezeni alpha
  soq.a <- t(P - diag(s)) # matice na leve strane (P-I)'
  soq.b <- rep(0, s) # vektor pravych stran 0
  
  soq.a[1, ] <- rep(1, s)
  soq.b[1] <- 1
  
  alpha <- solve(soq.a, soq.b)                             ####
  
  q <- rowSums(P * R)
  
  v <- rep(n*alpha %*% q, nrow(P))
  return(v+v0)
}
hruby_odhad(20, P, R, v0 = c(0,0))


presnejsi_odhad <- function(n, P, R, v0) {
  s <- nrow(P)
  soq.a <- t(P - diag(s))                                         ### Limitni matice A
  soq.b <- rep(0, s) 
  soq.a[s, ] <- rep(1, s)
  soq.b[s] <- 1
  alpha <- solve(soq.a, soq.b) 
  maticeA_6 <- matrix(data = rep(alpha, s), ncol = s, byrow = TRUE)  ####
  
  q <- matrix(rowSums(P * R), ncol = 1)                                              ### q
  
  I <- diag(nrow(P))
  Z <- solve((I-P+maticeA_6))                                       ### Z
  
  v <- t(Z %*% q +(n-1)*maticeA_6 %*% q) + v0
  return(v)
  
}

presnejsi_odhad(20, P, R, v0 = c(0,0))


########################################################   Porovname s funkci ze 6. cviceni

MarkovChainProfit <- function(P, R, v0, n) {
  
  s <- length(v0) # pocet stavu
  
  v <- matrix(NA, nrow = n + 1, s) # matice ocekavanych vynosu s pocty sledovanych kroku v radcich a s vychozimi stavy ve sloupcich
  v[1, ] <- v0 # celkove vynosy za 0 kroku (jednorazove vynosy)
  
  q <- rowSums(P * R) # vektor primych vynosu
  
  for (m in 1:n) {
    v[m + 1, ] <- q + P %*% v[m, ] # vektor celkovych vynosu za m kroku
  }
  
  rownames(v) <- paste("doba", 0:n, sep = ".")
  colnames(v) <- paste("poc", "stav", 1:s, sep = ".")
  
  return(v)
  
}  

MarkovChainProfit(P, R, v0 = c(0,0), n=5)[6, ]
hruby_odhad(n = 5, P, R, v0 = c(0,0))
presnejsi_odhad(n = 5, P, R, v0 = c(0,0))
### Presnejsi metoda je dost blizko skutecnych vynosu, odchylka je < 1%, u hrube metody
### vysledky jsou mnohem horsi

MarkovChainProfit(P, R, v0 = c(0,0), n=100)[101, ]
hruby_odhad(n = 100, P, R, v0 = c(0,0))
presnejsi_odhad(n = 100, P, R, v0 = c(0,0))

### Hruba metoda ma presnejsi vysleky nez s malym t, ale ty nejsou porad dokonale. 
### Presnejsi metoda uz ma stejne vysledky jako ve skutecnosti.
