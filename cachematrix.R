## Given a matrix, these functions create a list to 
## cache the matrix and solve for its inverse and 
## cache the inverse matrix as well.


## Creates a special "matrix" object that can 
## cache its inverse. It is essentially a list
## containing a function to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(invs) m <<- invs
  getInverse <- function() m
  
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse
       )
}


## Computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse 
## has already been calculated (and the matrix
## has not changed), then cacheSolve retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  
  if (!is.null(m))
  {
    message("Getting cached matrix")
    return (m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
