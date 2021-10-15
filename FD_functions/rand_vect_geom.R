# Function used for null models
# Generates random assemblages of species within FEs and FEs whithin assemblages

rand_vect_geom <- function(N, M) {
  vec <- rgeom(N, N/M)
  vec <- vec + 1
  if(sum(vec)<M){
    while(sum(vec)!=M){
      i=sample(1:N, 1)
      vec[i] <- vec[i]+1
    }
  } else {
    while(sum(vec)!=M){
      i=sample(1:N, 1)
      vec[i] <- vec[i]-1
    }
  }
  while (any(vec <= 0)) {
    negs <- vec <= 0
    pos  <- vec > 1
    vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
    vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
  }
  return(vec)
}