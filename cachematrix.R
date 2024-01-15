## This code contains two functions:
## 1. makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

## makeCacheMatrix starts by initializing two objects, matrix 'x', and
## its inverse 'inv' (empty at this stage).
## Then it defines four functions. 
## The 'set' function, assigns the input 'aMatrix' to the 'x' object 
## in the parent environment. It also clears any value of 'inv' that 
## had been cached by a prior execution of makeCacheMatrix()
## The 'get' function retrieves matrix 'x' from the parent environment 
## of makeCacheMatrix().
## The 'setinv' function defines the setter for the inverse matrix,
## assigning it to the parent environment. Note that 'solve' calculates
## the inverse of x. 
## The 'getinv' function defines the getter for the inverse matrix,
## looking in the parent environment for a cached version. 
## The last section of code assigns each of these four functions as a named 
## element within a list(), and returns it to the parent environment.
## When the makeCacheMatrix() function ends, it returns a fully formed 
## object of type makeCacheMatrix() to be used by downstream R code. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(aMatrix) {
    x <<- aMatrix
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve first tries to retrieve the cached inverse matrix. 
## If it can find this matrix ("is NOT null"), it will get the cached data. 
## If not, it will calculate the inverse matrix using the input data x,
## caching the result, and finally printing it. 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
