## The following two functions help caches calculation of a matrix inverse
## to help speed up calculations in cases where it is called many times


## This function ceates a matrix which also can store a cache of its inverse
makeCacheMatrix <- function(xmatrix = matrix()) {
  inverse <- NULL
  set <- function (x) {
    xmatrix <<- x
    inverse <<- NULL
  }
  get <- function() return(xmatrix)
  setInv <- function(inv) inverse <<- inv
  getInv <- function() return(inverse)
  list(set = set
       , get = get
       , setInv = setInv
       , getInv = getInv
       )
}

## This function checks if the matrix inverse have been cached
## by checking if is empty. if its not empty, return the matrix.
## Else create the inverse of the matrix and store it in cache

cacheSolve <- function(xmatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInv(inverse)
  inverse
}
