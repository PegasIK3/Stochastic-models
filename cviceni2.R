# Domaci ukol. 2

# install.packages('expm')
library(expm)
ReachabilityMatrix <- function(P) {
  s <- nrow(P) # pocet stavu
  reach <- matrix(FALSE, nrow = s, ncol = s) # inicializace matice
  for (t in 0:s) { # dosazitelnost staci overit pro pocet kroku 0 az s
    reach <- reach | (P %^% t > 0) # bud dosazitelnost za t-1 kroku nebo za t kroku
  }
  return(reach)
}

maticeB <- matrix(data=c(0.5, 0.5, 0,
                         0.5, 0.5, 0,
                         0.5, 0, 0.5), nrow = 3, byrow = TRUE)       # Zde piseme matici,



Rekurent <- function(maticeA) {
matice_dosazitelnosti <- ReachabilityMatrix(maticeA)
Rekurentni_stavy <- seq(1:nrow(maticeA))
for (i in 1:nrow(maticeA)) {
  for (j in 1:ncol(maticeA)) {
    if (matice_dosazitelnosti[i,j] == TRUE) {
      if (matice_dosazitelnosti[j,i] == FALSE) {Rekurentni_stavy[i] <- "Tranzientni"}
    }
  }
}
for (i in 1:length(Rekurentni_stavy)){
  if (Rekurentni_stavy[i] != "Tranzientni") {Rekurentni_stavy[i] = "Rekurentni"}
}
return(as.data.frame(Rekurentni_stavy))
}

Rekurent(maticeB)

