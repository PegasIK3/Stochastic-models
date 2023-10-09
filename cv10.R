### Cviceni 10

### Funkce pro simulace modelu M/M/c

# lambda - intenzita prichodu
# mu - intenzita obsluhy
# cn - pocet obsluznych linek
# max.time - celkova doba simulace

SimulateMMC <- function(lambda, mu, cn, max.time = 100) {
  
  cur.time <- 0 # aktualni cas - zacnu v 0
  cur.event <- "open" # aktualni typ udalosti - zacnu otevrenim obchodu
  cur.state <- 0 # aktualni stav - zacnu v prazdnem obchode
  cur.customer <- 0 # aktualne prichozi - na zacatku nikdo
  
  next.arrive <- Inf # dalsi prichod, na zacatku nevim a dam nekonecno
  next.depart <- Inf # dalsi odchod, na zacatku nevim a dam nekonecno
  
  in.service <- c() # zakaznik v obsluze
  in.queue <- c() # zakaznici ve fronte
  
  events <- data.frame(time = numeric(), event = character()) # vysledky - udalosti
  states <- data.frame(state = integer(), time.start = numeric(), time.end = numeric()) # vysledky - navstivene stavy
  customers <- data.frame(id = integer(), time.arrive = numeric(), time.cash = numeric(), time.depart = numeric(), queue.length = integer()) # vysledky - info o zakaznicich
  
  repeat{
    
    events <- rbind(events, data.frame(time = cur.time, event = cur.event))
    if (cur.event == "open"){
      next.arrive <- rexp(n = 1, rate = lambda) # cas prvniho prichodu
      
    } else if (cur.event == "arrive") {  ### nekdo prisel
      cur.customer <- cur.customer + 1 # cislo prichoziho
      if (cur.state <= cn){
        queue <- 0
      } else {
        queue <- (cur.state - cn)}
      customers <- rbind(customers, data.frame(id = cur.customer, time.arrive = cur.time, time.cash = NA, time.depart = NA, queue.length = queue)) # ulozim prichod zakaznika
      next.arrive <- cur.time + rexp(n = 1, rate = lambda) # cas dalsiho prichodu
      if (cur.state <= cn) { # prichozi jde rovnou do obsluhy
        in.service <- cur.customer
        next.depart <- cur.time + rexp(n = 1, rate = mu) # cas dalsiho odchodu
        customers$time.cash[in.service] <- cur.time # ulozim cas, kdy zakaznik prijde na radu
        customers$time.depart[in.service] <- next.depart # ulozim cas odchodu
      } else { # prichozi jde do fronty
        in.queue <- c(in.queue, cur.customer) # pridam do fronty
      }
      cur.state <- cur.state + 1 # zvysime pocet lidi v obchode o jedna
      
    } else if (cur.event == "depart") { # nekdo odesel
      
      if (cur.state == 1) { # obslouzeny clovek je posledni v obchode
        in.service <- c()
        next.depart <- Inf # obchod je prazdny a dalsi clovek odejde v nedohlednu
      } else {
        in.service <- in.queue[1] # prvni clovek ve fronte prijde na radu - FIFO
        in.queue <- in.queue[-1] # odeberu cloveka z fronty - FIFO
        next.depart <- cur.time + rexp(n = 1, rate = mu) # cas dalsiho odchodu
        customers$time.cash[in.service] <- cur.time # ulozim cas, kdy zakaznik prijde na radu
        customers$time.depart[in.service] <- next.depart # ulozim cas odchodu
      }
      cur.state <- cur.state - 1 # snizime pocet lidi v obchode o jedna
      
    } else if (cur.event == "close") { # obchod zavrel
      
      customers$time.cash[is.na(customers$time.cash) | customers$time.cash > max.time] <- max.time
      customers$time.depart[is.na(customers$time.depart) | customers$time.depart > max.time] <- max.time
      if (cur.state > 0) {
        states <- rbind(states, data.frame(state = seq(from = cur.state - 1, to = 0, by = -1), time.start = max.time, time.end = max.time))
      }
      break() # ukoncime cyklus
    }
      possible.time <- c(next.arrive, next.depart, max.time) # jake udalosti mam pred sebou
      next.time <- min(possible.time) # v jaky cas nastane nasledujici udalost
      possible.event <- c("arrive", "depart", "close") # mozne nasledujici udalosti
      next.event <- possible.event[which.min(possible.time)] # typ nasledujici udalosti
      
      states <- rbind(states, data.frame(state = cur.state, time.start = cur.time, time.end = next.time)) # ulozim vysledky o navstivenem stavu
      
      cur.time <- next.time # z casu nasledujici udalosti udelam soucasny
      cur.event <- next.event # z typu nasledujici udalosti udelam soucasny
      
    }
    
    states <- cbind(states, duration = states$time.end - states$time.start) # pridam sloupec s dobou trvani stavu
    customers <- cbind(customers, duration.queue = customers$time.cash - customers$time.arrive, duration.service = customers$time.depart - customers$time.cash, duration.total = customers$time.depart - customers$time.arrive) # pridam sloupce s dobami trvani
    
    return(list(events = events, states = states, customers = customers))
  }

lambda <- 14
mu <- 24
cn <- 2
results <- SimulateMMC(lambda = lambda, mu = mu, cn = cn, max.time = 9)

events <- results$events
states <- results$states
customers <- results$customers


