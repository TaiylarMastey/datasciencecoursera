
## The first function makeCacheMatrix creates a special matrix, which is a list containg a function to:
##1)Set the value of the matrix
##2)Get the value of the matrix
##3)Set the value of the Inverse
##4)Get the value of the Inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The following function calculate the inverse of the special matrix created with the above function.
##However, it first checks to see if the mean has already been calculated.  If so, it gets the inverse from the
##cache and skips to the computation.  Otherwise, it calculates teh mean of the data and sets the value of the
##inverse in the cache via the setInverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
