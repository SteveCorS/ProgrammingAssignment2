## A pair of functions to cache the inverse of a matrix
## to avoid costly repeated computation

## Creates a special matrix object that caches its inverse. 
## Use get and set to get and set the matrix values. 
## Use getInv and setInv to get and set the inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL ##inverse of the matrix
  
  get <- function() x
  
  set <- function(y) {
    x <<- y
    inv <<- NULL ## invalidate inverse
  }
  
  setInv <- function(inverse) inv <<- inverse
  
  getInv <- function() inv
  
  list(set = set, get = get, setInv = setInv, getInv = getInv) 
}


## Get the inverse of a special matrix object created by 
## makeCacheMatrix(). Calculates the inverse if there is 
## no valid cached version. Supply additional arguments 
## for the solve function to calculate the inverse after x.
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) { ##cached version exists?
    message("getting cached inverse of matrix")
    return(inv)
  }  
  mat <- x$get()
  inv <- solve(mat, ...) ##calculate inverse
  x$setInv(inv) 
  inv
}
