##  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  r <- NULL
  set <- function(y) {                    ## set original matrix
    x <<- y
    r <<- NULL
  }
  get <- function() x                       ## get original matrix
  setinverse <- function(solve) r <<- solve ## set inverse matrix
  getinverse <- function() r                ## get inverse matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) { 
  r <- x$getinverse()     ## get cached value
  if(!is.null(r)) {       ## check if it's exist
    message("getting cached data")
    return(r)             ## if it's exist, return it
  }
  message("getting NON-cached data")
  data <- x$get()         
  r <- solve(data, ...)   ## if it's not - calculate inverse matrix 
  x$setinverse(r)         ## ... and cache it
  r
}
