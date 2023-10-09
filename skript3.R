# Ukol ze cviceni 3


OpilcovaProchazka <- function(s) { # s je pocet stavu
  P <- matrix(0, nrow= s, ncol = s)
  
  for (i in 2:(s-1)){
    P[i,1] <- 0.1 # zavola taxika
    P[i,i] <- 0.2 # zustane na miste
    P[i, i-1] <- 0.3 # pujde doleva
    P[i, i+1] <- 0.4 # pujde doprava
  }
  P[2,1] <- 0.4 # pravdepodobnost ze pujde doleva, nebo zavola taxika
  P[2,2] <- 0.2
  P[2, 3] <- 0.4
  P[1,1] <- 1
  P[s, s] <- 1
  return(P)
}

JeStohasticka <- function(matice3) {
  return(all(matice3>=0) & all(rowSums(matice3) == 1) & is.matrix(matice3))
}


matice3_1 <- OpilcovaProchazka(8)
matice3_1
JeStohasticka(matice3_1)

#####


P = matrix(c(0.4, 0.6, 0, 0,
             1/6, 0, 5/6, 0,
             0.6, 0, 0, 0.4,
             1, 0, 0, 0), byrow = TRUE, nrow = 4)
P
rvek = c(1, 0.6, 0.5, 0.2)
a=1/(sum(rvek))*rvek
a
1/sum(rvek)
sum(rvek)
sum(a)

P <- matrix(c(0.5, 0.5, 0,
              1/6, 1/3, 0.5,
              2/3, 0, 1/3), byrow = TRUE, nrow =3)
#install.packages("xlsx")
library("expm")

P %^% 99
rowSums(P)

P1 <- matrix(c(1/3, 1/2, 0, 1/6,
              0, 1/3, 1/2, 1/6,
              0, 0, 2/6, 4/6,
              1,0,0,0), byrow = TRUE, nrow= 4)
P1 %^% 100
a3 <- 1/(20/18+12/18+24/18+18/18)
a2 <- 4/3*a3
a1 <- 5/6*a2+2/3*a3
ve <- c(a1, a2, a3)
5/6*4/3
1/(20/18+12/18+24/18+18/18)
9/37
18
36/117
pr <- c(40000, 60000, 80000)
pr %*% as.matrix(ve) * 100
