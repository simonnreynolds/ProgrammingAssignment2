# Assignment: Caching the Inverse of a Matrix
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly (there
# are also alternatives to matrix inversion that we will not discuss here). Your
# assignment is to write a pair of functions that cache the inverse of a matrix.


#   makeCacheMatrix: This function creates a special "matrix" object that can
#   cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  

}


# cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve the
# inverse from the cache. 

cacheSolve <- function(x, ...) {
 
   m <- x$getInverse()
   
   if(!is.null(m)) {
     message("getting cached data")
     return(m)
   }
  
   data <- x$get()
   m <- solve(data, ...)
   x$setInverse(m)
   m
  

  
  ## Return a matrix that is the inverse of 'x'
  
  
}





