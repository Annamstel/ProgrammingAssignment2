## Sets up a matrix that is invertible and returns its inverse
##1.set the value of the vector
##2.get the value of the vector
##3.set the value of the mean
##4.get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  
  x <<- y
  inv <<- NULL
 
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the special matrix created by makeCacheMatrix. 
##If the inverse exists, retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
  
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}
