## Here is a pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.
## clevinthal 
makeCacheMatrix <- function(x = matrix()) {
  ##Initialize inverse property
  inv <- NULL

  ##Use method to set matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }

  ##Use method to get matrix
  get <- function() x

  ##Set inverse of matrix method
  setInverse <- function(solveMatrix) inv <<- solveMatrix

  ##Get inverse of matrix method
  getInverse <- function() inv

  ##Return list of methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function evaluates the inverse of the special "matrix" returned
## by the makeCacheMatrix function above.
cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()

  ##Return the inverse if already set
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }

  ##Get matrix from from object
  data <- x$get()

  ##Compute inverse with matrix multiplication
  inv <- solve(data)

  ##set inverse of object
  x$setInverse(inv)

  ##Finally, return the matrix
  inv
}
