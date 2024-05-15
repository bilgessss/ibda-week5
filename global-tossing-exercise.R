library(stats)

simulate_global_tosses <- function(n,p) {
  
  outcomes <- sample(c("L","W"), size = n, replace= TRUE, prob = c(p,1-p))
  return(outcomes)
}


counter <- function(outcomes, candidate_proportions){
  
  n <- length(outcomes)
  count_L <- sum(outcomes == "L")
  
  ways <- sapply(candidate_proportions, function(cp) {dbinom(count_L, n, cp)
    
 names(ways) <- candidate_proportions
 return(ways)
 
  })
  
}

counter(simulate_global_tosses(10, .1), cp = seq(0.1,.1))

