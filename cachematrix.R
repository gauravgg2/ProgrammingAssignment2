## set function will initialise/reset matrix x with y
## get function will return the currently stored matrix in object x
## setinverse function will store the inversed matrix returned from cachesolve function on object m
## getinverse function will retrieve the value stored in object m, which is NULL if cacheMatrix not yet called or the inversed matrix if already called and computed using solve function

## this makeCacheMatrix will be used for setting and getting the data of matrix x if inversed of the same has already computed.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve function here first retrieve the data using getinverse function which is stored in global environment as a list, then 
##if it is not null then the inveresed matrix stored in m is retreived using global environment object m
## if inversed is not already stored then the same is computed using of the data (original matrix) then solve function is used to compute the inverse matrix
## here it will generate an error if it is not a inversible matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    print(data)
    m <- solve(data)
    x$setinverse(m)
    m
}
