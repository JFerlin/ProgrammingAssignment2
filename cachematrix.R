## makeCacheMatrix creates a special matrix object that can cache the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(z){
    x <<- z
    inv <<- NULL
  }
  get <- function() x
  setInvrs <- function(inverse) inv <<- inverse
  getInvrs <- function() inv
  list(set = set, get = get, setInvrs = setInvrs, getInvrs = getInvrs)
}

## Computes inverse of special matrix returend from makeCacheMatrix, if the inverser has already been calculated it will return cached results
cacheSolve <- function(x, ...) {
  inv <-x$getInvrs()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mtrx <- x$get()
  inv <-solve(mtrx, ...)
  x$setInvrs(inv)
  inv

}
