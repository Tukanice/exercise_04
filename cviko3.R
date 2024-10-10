#Task1
#pseudocode
#1.

#implementation
ReturnCoins <- function(M) {
  # Step by step breakdown
  pd <- M %/% 50         # Number of 50 CZK coins
  remainder <- M %% 50    # Remaining amount after 50 CZK coins
  
  dc <- remainder %/% 20  # Number of 20 CZK coins
  remainder <- remainder %% 20  # Remaining amount after 20 CZK coins
  
  ds <- remainder %/% 10  # Number of 10 CZK coins
  remainder <- remainder %% 10  # Remaining amount after 10 CZK coins
  
  p <- remainder %/% 5    # Number of 5 CZK coins
  remainder <- remainder %% 5  # Remaining amount after 5 CZK coins
  
  d <- remainder %/% 2    # Number of 2 CZK coins
  remainder <- remainder %% 2  # Remaining amount after 2 CZK coins
  
  j <- remainder %/% 1    # Number of 1 CZK coins
  
  # Return the result as a named list
  return(list(
    "50 CZK" = pd,
    "20 CZK" = dc,
    "10 CZK" = ds,
    "5 CZK" = p,
    "2 CZK" = d,
    "1 CZK" = j
  ))
}

# Example usage
ReturnCoins(123)


#Task2
#UniversalReturnCoins(M, c[])
#1. Initialize i[] as an array of zeros of the same length as c[]
#2. For each denomination c[k] in array c[]:
#  i[k] = M // c[k]
#M = M % c[k]
#3. Return i[]

# implementation
UniversalReturnCoins <- function(M, c) {
  # Initialize the result array with zeros
  i <- rep(0, length(c))
  
  # Loop over each denomination
  for (k in seq_along(c)) {
    i[k] <- M %/% c[k]  # Number of coins for the current denomination
    M <- M %% c[k]      # Remaining amount after giving out coins
  }
  
  # Return the result array (i) which contains the counts of each denomination
  return(i)
}

# Example usage
M <- 123
c <- c(50, 20, 10, 5, 2, 1)
UniversalReturnCoins(M, c)


#Task 3
#pseudocode
#Chocolate(M, r, c)
#1.  if r = number of rows in M
#2.     return M[r, c]
#3.  else
#  4.     bars ??? M[r, c]
#5.     down ??? Chocolate(M, r + 1, c)
#6.     diagonal ??? Chocolate(M, r + 1, c + 1)
#7.     return max(down, diagonal) + bars

#implementation
Chocolate <- function(M, r, c) {
  if (r == nrow(M)) {
    return(M[r, c])
  }
  else{
  bars <- M[r, c]
  down <- Chocolate(M, r + 1, c)
  diagonal <- Chocolate(M, r+1, c+1)
  return(max(down, diagonal) + bars)
  }
}

# Example usage
M <- matrix(c(2, 3, 1, 7,
              9, 8, 5, 6,
              3, 4, 2, 7,
              8, 1, 9, 4), nrow = 4, byrow = TRUE)

# Start at the top left corner (1, 1)
Chocolate(M, 1, 1)

# Task4
#pseudocode
#HanoiTowers(n, fromPeg, toPeg)
#1.  if n = 1
#2.      print "Move disc from peg fromPeg to peg toPeg"
#3.      return
#4.  else
#  5.      unusedPeg ??? 6 - fromPeg - toPeg
#6.      HanoiTowers(n - 1, fromPeg, unusedPeg)
#7.      print "Move disc from peg fromPeg to peg toPeg"
#8.      HanoiTowers(n - 1, unusedPeg, toPeg)
#9.  return


#implementation
HanoiTowers = function(n, fromPeg=1, toPeg=1){
  if (n == 1){
    print(sprintf('Move disc from peg %s to peg %s', fromPeg, toPeg))
    return()
  }
  unusedPeg = 6 - fromPeg - toPeg
  HanoiTowers(n - 1, fromPeg, unusedPeg)
  print(sprintf('Move disc from peg %s to peg %s', fromPeg, toPeg))
  HanoiTowers(n - 1, unusedPeg, toPeg)
  return()
}

HanoiTowers(5,1,3)

