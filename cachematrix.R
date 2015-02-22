## makeCacheMatrix caches the results of inverse calculations for matrices and 
## cacheSolve checks the cache for the inverse of a matrix and, if NULL, 
## calculates the inverse and stores a copy in the cache

## makeCacheMatrix is used to create a database of matrices and their inverses
## It returns an object of type makeCacheMatrix containing 
## a list of methods for getting and setting matrix and inverse values

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }  
  setinverse <- function(inverse) {
    inv <<- inverse
  }  
  getinverse <- function() {
    inv
  }
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve finds the inverse of a matrix
## x is an object of type makeCacheMatrix
## cacheSolve speeds up inverse solving by saving the inverse in a cache and 
## checking cache first for each cacheSolve call

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## First try to retrieve inverse from cache
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    ## If inverse stored in cache,
    ## return inverse and quit function
    return(inv)
  }
  
  ## If retrieving inverse from cache failed, 
  ## retrieve matrix and send to solve function
  data <- x$get()
  inv <- solve(data, ...)
  
  ## Store calculated inverse
  x$setinverse(inv)
  inv
}
