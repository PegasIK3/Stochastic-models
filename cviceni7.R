### Ukol ze cviceni 7

#7.1

Prochazka <- function(n, start = 0) {                                       #### funkce ktera simuluje prochazku
  proba <- c(0.4, 0.4, 0.2)                                                 #### nema smy
  xvek <- c(start)
  skokx <- start
  for (i in 1:n) {
    skokx = sample(c(skokx+1, skokx-1, skokx-5),size = 1, replace = TRUE, prob = proba)
    xvek <- append(xvek, skokx)
  }
  
  data <- data.frame(t <- seq(from = 0, to = n), xvek)
  return(data)
}

Prochazka(n = 40)


##################################################################################################


Prochazka_s_barierami <- function(n, start = 0, horni_bariera = Inf, dolni_bariera = -Inf) {
 
  db <- dolni_bariera
  hb <- horni_bariera
  
  if (start >= hb | start <= db) {stop("start je mimo bariery")}
  
  proba <- c(0.4, 0.4, 0.2)                                         ### vektor pravdepodobnosti
  
  kroky <- rep(NA, n+1)                                             ### kroky pro n
  kroky[1] <- start
  
  skoky <- rep(NA, n)
  
  for(i in 1:n){
    skok <- sample(c(1, -1, -5),size = 1,  prob = proba)
    skoky[i] <- skok
    kroky[i+1] <- kroky[i] + skok
    if (kroky[i+1] >= hb){
      kroky[i+1] <- kroky[i]
    }
    if (kroky[i+1] <= db) {
      kroky[i+1] <- db + abs(skok + (abs(abs(kroky[i])-abs(db))-1)) 
                                                  }
  }
  
  data <- data.frame(t = 0:n, kroky, c(skoky,0))
  return(data)
}

data.walk <- Prochazka_s_barierami(n = 100, start = 7, dolni_bariera = 0)

plot(data.walk$t, data.walk$kroky, type = "l")

 