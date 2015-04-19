# Programming Assignment 2
# The purpose of the assignment is to create two functions: 
#   1: To create a 'special' kind of matrix based on the input parameter (which takes in a matrix)
#   2: To get the inverse matrix of (1), and get the inversed matrix if the inverse calculation has taken place before
# to show the usage of cache in R

# This is function #1, input: a matrix, output: a list of functions of the 'special' matrix object
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  
  get <- function() {
    x
  }
  
  getInverseCache <- function() {
    cachedInverse
  }
  
  setInverseCache <- function(inversedMatrix) {
    cachedInverse <<- inversedMatrix
  }
  
  list(get=get, getInverseCache=getInverseCache,
       setInverseCache=setInverseCache)
}

# This is function #2, input: 'special' matrix that is the output of (1), 
# output: the inverse of the input, it'll calculate if it hasn't been calculated before, else will get it from cache
cacheSolve <- function(x, ...){
  if (!is.null(x$getInverseCache())){
    if (x$getInverseCache() != x$get()) {
      print("Getting from cache..")
      x$getInverse()
    } else {
      solvedMatrix <- solve(x$get())
      x$setInverseCache(solvedMatrix)
      print("Getting from newly calculated inversed matrix..")
      solvedMatrix
    }    
  } else {
    solvedMatrix <- solve(x$get())
    x$setInverseCache(solvedMatrix)
    print("Getting from newly calculated inversed matrix..")
    solvedMatrix
  }
}


# Test case:
# > n <- 1000
# > mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
# > matCached <- makeCacheMatrix(mat)
# > time1 <- system.time(matSolved1 <- cacheSolve(matCached))
# > time2 <- system.time(matSolved2 <- cacheSolve(matCached))
# > time1["user.self"]
# > time2["user.self"]

# From here you will see the difference in the calculation speed between calculating the inverse directly,
# or from getting the calculation result from the cache