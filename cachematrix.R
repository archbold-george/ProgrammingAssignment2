## The basic idea is to write a pair of functions that cache the inverse of a matrix.
## using functions makeCacheMatrix and cacheSolve

## The function to create the special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_matrix <<- inverse
  getinverse <- function() inv_matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_matrix <- x$getinverse()
        if(!is.null(inv_matrix)) {
          message("getting cached matrix inverse")
          return(inv_matrix)
        }
        data <- x$get()
        inv_matrix <- solve(data, ...)
        x$setinverse(inv_matrix)
        inv_matrix
}
