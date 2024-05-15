# Exercise 1 (Tossing & Counting) --------------------------------------------------------------


# globe tossing function

sim_tosses <- function(n, p){
  
  sample(c("L", "W"), size=n, replace=TRUE, prob=c(p, 1-p))
  
}


# counting function

counter <- function(data, cp){ 
  
  sides <- length(cp)-1
  
  L <- sum(data=="L")
  W <- sum(data=="W")
  ways <- sapply( cp , function(cp) (cp*sides)^L * ((1-cp)*sides)^W ) 
  data.frame(cp, ways) 
  
}

counter(c("L", "W", "L"), cp=seq(0,1,.25)) # reproduces example from slides
counter(sim_tosses(10, .1), cp=seq(0,1,.1)) 
