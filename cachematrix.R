## Put comments here that give an overall description of what your
## functions do

## This function establishes a cached matrix datatype

makeCacheMatrix <- function(x = matrix()) {
  mI <- NULL
  set <- function(y) {
    x <<- y
    mI <<- NULL
  }
  get <- function() x
  setInverse <- function(matInverse) mI <<- matInverse
  getInverse <- function() mI
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function will utilize the cached matrix datatype to
## either compute a new inverse or return a cached inverse if the 
## function is passed the same matrix as the previous one

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ##
  mI <- x$getInverse()
  if(!is.null(mI)) {
    message("Getting cached data.")
    return(mI)
  }
  data <- x$get()
  ## Use the solve function to find the inverse
  ##
  mI <- solve(data)
  x$setInverse(mI)
  mI
}
