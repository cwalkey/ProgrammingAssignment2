## Caches inverse of a matrix to save computational time 

## Makes a cached matrix object for storing and retrieving the matrix inverse
## Matrix must be invertible
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  list(set=set, get=get, 
       setInverse=setInverse, 
       getInverse=getInverse)
}

## Return a matrix that is the inverse of 'x'
## If inverse has been cached, returns cached value
## If inverse has not been cached, computes inverse, caches, and then returns value
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setInverse(inv)
  inv
}
