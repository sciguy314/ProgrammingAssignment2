## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
