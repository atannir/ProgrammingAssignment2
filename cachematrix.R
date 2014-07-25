## Constructor-type function, getter-type function
##
## Essentially, memoize the result of the expensive operation
## 
##
## Curious how this will merge with my 
## version at home that I haven't committed yet
##
## This actually shows measurable performance lag when testing:
## m <- matrix(data = rnorm(1000*1000), nrow=1000, ncol = 1000)
## cm <- solve(m)

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## strongly based on the example from the problem description
  minv <- NULL
  
  set <- function(y) {
    ## use of <<- to set values out of current scope / environment
    x <<- y ## x created in scope when makeCacheMatrix called
    minv <<- NULL ## cached inverse
  }
  
  get <- function() x ## return value either created or set
  setinv <- function(inv) minv <<- inv
  getinv <- function() minv
  
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
  
  ## not quite how I expected OO/closures to look in R
  
}


## compute inverse of matrix and store for future reference

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## If the matrix hasn't changed, return the pre-computed object
  m <- x$getinv() ## if null, don't worry
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get() 
  m <- solve(data) ## description states to assume invertible matrix
  x$setinv(m) ## stored solved value
  m
}
