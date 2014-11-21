## Create new type of matrix which can be cached inverse

## makeCacheMatrix is used to create new type of matrix
## input: matrix
## output: cachedMatrix
## set : set the matrix
## get : get the matrix
## setinverse: set the inverse of matrix
## getinverse: get the inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    # if matrix was modified, then reset inv
    inv <<- NULL 
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is used to get cached inverse
## input: cachedMatrix
## output: inverse of cachedMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv## Return a matrix that is the inverse of 'x'
}
