## The makeCacheMatrix creates a matrix object that can cache a matrix 
## and it's inverse
## The cacheSolve function computes the inverse of the special
##  "matrix" returned by `makeCacheMatrix` above.

## makeCacheMatrix  creates a matrix object with 4 functions.
## Since the makeCacheMatrix returns a list of functions which point to
## its parent environment the matrix information is cached

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
## The set function uses the <<- assignment operator to assign the new matrix to x
## The function then assigns the value m to null, clearing the value of m. 
  set <- function(y){
    x <<- y
    m <<- NULL
  }
## The get function is used to bring the value of the matrix from the parent environment
## since x is not defined within the get function.
  get <- function() x

## The setinverse function uses <<- to assign the input matrix to m in parent environment 
  setinverse <- function(invmatrix) m <<- invmatrix
  
## the getinverse is used to retreive the inverse matrix
  getinverse <- function() m

## The makeCacheMatrix returns the functions as a list.
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function is used to either retrieve the inverse of the matrix
## or calculate the inverse of the matrix. The cacheSolve accepts input of the 
## objcect type makeCacheMatrix that is defined above.

cacheSolve <- function(x, ...) {
 ## Retrieves the inverse of the matrix and assigns it to m
    m <- x$getinverse()
 ## Checks if the inverse was previously calculated. The inverse will be null if 
 ## it was not previously calculated. If the inverse is not null, the function returns
 ## m. 
    if(!is.null(m)){
        message("cached inverse")
        return(m)
    }
 ## If the inverse was not calculated, the matrix is retrieved and assigned to data
 ## the Solve function is used to calculate the inverse of the matrix.
    data <- x$get()
    m <- solve(data)
 ## The inverse element in the makeCacheMatrix object is set using the setinverse function.
    x$setinverse(m)
    m
}
