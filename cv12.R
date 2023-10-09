### Cviceni 12

### mu - stradni hodnota skoku
### sigma - smerodatna odchylka skoku
### lambda - prumerny pocet skoku

simulateSkoky <- function(mu = 10, sigma = 1, lambda = 100, to = 1){

  t = 0
  repeat{
    t <- c(t, rexp(n = 1, rate = lambda))        ### nasimulujeme doby, kdy dojde ke skokum
    if(sum(t) > to){                             ### tato funkce skonci, kdy uz t je vice nez 1
      t <- head(t, -1)                           ### a proto posledni element smazeme
      break
    }
  }
  
 ln <- length(t)                                 ### pocet skoku

 Jvek <- rnorm(n = ln, mean = mu, sd = sigma)    ### skoky
 
 x <- rep(0, ln)                                 ### nasimulujeme hodnoty x
 for(i in 1:ln){
   x[i] <- sum(Jvek[1:i])
 }
 
 res <- data.frame(cas = cumsum(t), skok = Jvek, x = x)
 
 return(res)
}


skoky <- simulateSkoky()
plot(skoky$x, skoky$cas, type = 's')

