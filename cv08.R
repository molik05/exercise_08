
FindSorted <- function(permutation){
  pos <- 1
  l <- length(permutation)-1
  for (i in 2:l){
    if (permutation[i] == pos){
      pos <- pos+1
    }
    else {return(pos)}
  }
  return(pos)
}

IndicateAscending <- function(permutation){
  l <- length(permutation)
  vec <- numeric(l)
  vec[1] <- 1
  vec[l] <- 1
  for (j in 1:(l-1)){
    if ((permutation[j]+1) == permutation[j+1]){
      vec[j] <- 1
      vec[j+1] <- 1
    }
  }
  return(vec)
}

BreakPointSort <- function(permutation){
  #nastaveni zakladu
  l <- length(permutation)
  permutation <- c(0,permutation,(l+1))
  minimum <- max(permutation)
  l <- length(permutation)
  
  #cyklus ktery opakuju nez neco
  #while ((FindSorted(permutation+1)) != l){
  while ((FindSorted(permutation)+1) != l){ 
    sorted <- FindSorted(permutation)
    vector <- IndicateAscending(permutation)
    for (k in 1:l){
      if (vector[k] == 0){
        if (permutation[k] < minimum){
          minimum <- permutation[k]
          minid <- k}
      }
    }
    sorted_reg <- permutation[1:sorted]
    reversed_reg <- rev(permutation[(sorted+1):minid])
    rest_reg <- permutation[(minid+1):l]
    permutation <- c(sorted_reg,reversed_reg,rest_reg)
    print(permutation)
  }
  return(permutation)
}

BreakPointSort(permutation)

permutation <- c(5,8,7,1,2,4,6,3)

