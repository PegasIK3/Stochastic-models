### Ukol ze cviceni 5
library('expm')

P <- matrix(c(0.5, 0.5, 0,
              1/6, 1/3, 0.5,
              2/3, 0, 1/3), byrow = TRUE, nrow =3)  


maticeA <- function(A, t){  ### Funkce vypoctu A^t.  A je prechodova matice, t je stupen.
  s1 <- diag(nrow(A))
  for (i in 1:t) {
    s1 <- s1 + A %^% i
  }
  maticeAt <- s1/(t+1)
  return(maticeAt)
}

### Abychom videli konvergenci vypocteme i matice A.

LimitniMatice <- function(P) {
  s <- nrow(P)
  soq.a <- t(P - diag(s)) 
  soq.b <- rep(0, s) 
  soq.a[s, ] <- rep(1, s)
  soq.b[s] <- 1
  alpha <- solve(soq.a, soq.b) 
  maticeA_5 <- matrix(data = rep(alpha, s), ncol = s, byrow = TRUE)
  return(maticeA_5)
}

### Vytvorime periodicky model obnovy

RenewalModel <- function(a) {            ### funkce ze 5. cviceni
  s <- length(a) 
  r <- rev(cumsum(rev(a))) 
  P <- cbind(a / r, rbind(diag(r[-1] / r[-s]), rep(0, s - 1))) 
  return(P)
}

Vek.period <- c(0, 0.2, 0, 0.8)

P5 <- RenewalModel(Vek.period)

P5 %^% 999
P5 %^% 1000  ### To je periodicky model. Neni regularni.

maticeA(P5, 100)  ### Vypocteme matice A^t pro t = 10, 100 a 1000. A porovnejme ji s limitni matici
LimitniMatice(P5)
maticeA(P5, 10) - LimitniMatice(P5)
maticeA(P5, 100) - LimitniMatice(P5)
maticeA(P5, 1000) - LimitniMatice(P5) # Rozdily uz jsou skoro nula. Takze s rostoucim t, matice A^t konverguje
                                      # k matici A
# Je videt ze matice P5^100 porad obsahuje nuly (To je periodicky retezec). Ale kdyz udelame prumer z P^100 a P^101 (Po sobe nasledujici)
# Pak dostaneme matici, ktera take konverguje k limitni matici.
(P5 %^% 100 + P5 %^% 101) / 2

z <- (P5 %^% 10000 + P5 %^% 10001) / 2  ### Velky pocet kroku

all.equal(z, LimitniMatice(P5), maticeA(P5, 1000)) # a porovnejme s limitni matici.

# Jsou uz pro eRko stejne. A je videt, ze matice opravdu konverguje.
 


