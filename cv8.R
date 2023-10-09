### Domaci ukol 8

###
#   o  - pocet operatoru
#   w  - kapacita fronty
#   a  - intenzita ztraty trpelivosti
#   lambda - intenzita zavolani
#   mu - intenzita vyrizeni hovoru

Telefonni <- function(o = 5, w = 5, a = 4, lambda = 3, mu = 5){
  
  s <- o + 1 + w  ### pocet stavu je pocet operatoru plus kapacita fronty plus prazdna linka
  
  Q <- matrix(0, nrow = s, ncol = s) ## prazdna matice
  
  Q[1, 1] <- -lambda
  Q[1, 2] <- lambda
  
  for (i in 2:(s-1)) {
    Q[i, i - 1] <- (i - 1) * mu
    Q[i, i + 1] <- lambda
    Q[i, i] <- -(Q[i, i - 1] + Q[i, i + 1])
    if (i > (o+1)) {
      Q[i, i-1] <- o*mu + (i-(o+1)) * a          ### uz vse operatory maji volajiciho a tak intenzita ukonceni hovoru neroste
      Q[i, i] <- -(Q[i, i - 1] + Q[i, i + 1])    ### ale roste intenzita toho, ze volajici ztrati trpelivost.
    }
  }         
  
  
  Q[s, s - 1] <- o * mu + w * a
  Q[s, s] <- -(o * mu + w * a)
  
  nazvy <- paste('hovory', 0:o)
  nazvy <- append(nazvy, paste('fornta', 1:w))
  colnames(Q) <- nazvy
  rownames(Q) <- nazvy 
  
  return(Q)
}

Telefonni()

### Ted' zkusime s ruznymi parametry
Telefonni(o = 2, w = 10, a = 5, lambda = 5, mu = 6)

### Nebo mame 5 operatoru, kteri vyrizuji 5 hovoru za hodinu. Intenzita zavolani je 6 a kapacita fronty je 7.
### intenzita ztraty trpelivosti je 15.
Telefonni(o = 5, w = 7, a = 15, lambda = 6, mu = 5)





