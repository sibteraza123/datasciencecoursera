## Lexical Scoping: creating functions that works on different environment
## makeCacheMatrix function, that contains free variables,
## assign the matrix to the cache memory so that it can be solve with another function
## cacheSolve function is defined to solve the above function


## The function makeCacheMatrix set and get the inverse of a
## matrix in the environment of cachesolve.

## NOTE: matrix should be typed are all invertible
## This function doesn't appreciate non-invertible matrix

makeCacheMatrix <- function(x=matrix()){
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## the function cachesolve Return the inverse of
## invertible matrix defined in makeCaheMatrix function

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
