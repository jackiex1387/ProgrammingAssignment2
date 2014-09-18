## the functions makeCacheMatrix and cacheSolve allow
## the inverse of a matrix to be calculated the first time
## it is needed, stored in a cache, and retrieved 
## from the cache if needed again.

## the first function makeCacheMatrix matrix takes a 
## 2x2 matrix and creates a special object that stores 
## its value and a cache for its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
    ## creates empty object "m" to cache matrix inverse
  set <- function(y) {
    x <<- y
    m <- NULL
  }
    ## set() stores matrix into "x"
  get <- function() x 
    ## get() retrieves matrix from stored value "x"
  setinverse <- function(solve) m <<- solve 
    ## setinverse() caches inverse in object "m" 
  getinverse <- function() m 
    ## getinverse() retrieves cached inverse from object "m"
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
    ##list is printed to show set, get, setinverse, getinverse
    ## functions
}


## the second function cacheSolve checks if the inverse
## of a matrix has been cached and returns the cached value.
## if the inverse has not been cached, it computes the
## inverse and caches the value in makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
    ## checks if there is a cached value for the inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    ## returns cached value for the inverse if it exists
  }
  data <- x$get()
    ## if no cached value exists, matrix is retrieved
  m <- solve(data, ...)
    ## inverse is calcuated 
  x$setinverse(m)
    ## inverse calucation is cached into makeCacheMatrix 
  m
    ## Return a matrix that is the inverse of 'x'
}
