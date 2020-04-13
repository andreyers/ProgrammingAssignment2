## This pair of functions saves computation time by setting a matrix object
## with the capability to cache its inverse.
## E.g. get the inverse matrix of x by typing cachesolve(makeCacheMatrix(x))


## This functions outputs a list of functions operating on an input matrix
## The functions set and return the matrix & sets and caches its inverse when calculated.

makeCacheMatrix <- function(x = matrix()) {
  
  s <- NULL              # initialize "s" (the inverse matrix)
  set <- function(y) {   # fxn sets value of matrix
      x <<- y
      s <<- NULL
  }
  get <- function() x    # fxn returns value of matrix
  setsolve <- function(solve) s <<- solve  # fxn sets inverse of matrix
  getsolve <- function() s                 # fxn returns inverse matrix

  list (set = set, get = get, setsolve = setsolve, getsolve = getsolve)
  
}

## This function calls the inverse matrix from the matrix object, if already calculated
##  Otherwise, the function will calculate and set the inverse matrix.

cacheSolve <- function(x, ...) {
  
  s <- x$getsolve() # check if inverse is already calculated
  
  if(!is.null(s)) { # if inverse is already calculated, return s
    message("getting cached data")
    return(s)
  }
  
  # if statement does not run, get the matrix and calculate its inverse:
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s

}
