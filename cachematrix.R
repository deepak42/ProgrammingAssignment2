## The makeCacheMatrix function creates a cache of a Matrix and it's inverse
## The cacheSolve function checks if the inverse of a matrix has been created


## The makeCacheMatrix creates a special matrix that contains functions to set the matrix,
## get the matrix, computes the inverse of the matrix and get's the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve(x)
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## The cacheSolve function first checks if the matrix's inverse has been computed.
## If it has, the function returns the inverse from the cache 
## otherwise it computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
        message("cached Inverse")
        return(m)
  }
  data <- x$get()
  m <<- solve(data, ....)
  x$setInverse(m)
  m
}
