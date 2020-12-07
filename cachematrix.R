## Put comments here that give an overall description of what your
## functions do

## This function helps to cache the inverse of a matrix with cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL #Reserve the inverse matrix value
  
  set <- function(another_vector){ ##This function reset the values
    x <<- another_vector
    inverse_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(y) inverse_matrix <<- y
  getinverse <- function() inverse_matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getinverse()
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$setinverse(inverse_matrix)
  inverse_matrix
}
