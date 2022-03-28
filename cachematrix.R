## Those two functions can be used to invert a matrix and cache the outcome in the process so it doesn't have to 
## be recalculated in case the same matrix is entered. 

## The output of this function is a list of functions that makes it possible to cache the result in case it is used again. 

makeCacheMatrix <- function(x = matrix()) {
  
  m_in <- NULL
  
  set <- function(y) {
    x <<- y
    m_in <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solved_matrix) m_in <<- solved_matrix
  getinverse <- function() m_in
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix_get <- x$get()
  solved_matrix <- solve(matrix_get,...)
  x$setinverse(solved_matrix)
  solved_matrix
 
}


