## The solution to the Programming Assignment 2 of the Coursera R Programming Course:
## 
## Solution consists of 2 functions: makeCacheMatrix and cacheSolve
##   makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##   cacheSolve: Computes the inverse of the special "matrix" returned by 
##              makeCacheMatrix above. If the inverse has already been calculated 
##               (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## makeCacheMatrix takes matrix as a parameter and returns special object which is a list of functions: 
##    set: function to set input cached matrix
##    get: function to get input cached matrix
##    setinverse: function to set computed inverse matrix
##    getinverse: function to retreive cached computed inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  cached_inverse <- NULL
  
  set <- function(y) {
    x <<- y
    cached_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cached_inverse <<- inverse
  getinverse <- function() cached_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


##   cacheSolve: This function computes the cached inverse of the special "matrix" returned by 
##              makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cached_x = x$get();
  
  ## Check is matrix cached already
  if (!is.null(cached_x)) {
    cached_inv <- x$getinverse()
  
    if(!is.null(cached_inv)) {
      return(cached_inv)
    }
  }
  ## if it's not cached - set to x, compute inverse and cache
  inv <- solve(cached_x, ...)
  x$setinverse(inv)
  inv
}
