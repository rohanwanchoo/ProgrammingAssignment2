## Put comments here that give an overall description of what your
## functions do
##The functions create an inverse of a square matrix unless the inverse is already
## created in which case it picks it from cache

## Write a short comment describing this function
## makeCacheMatrix is a function that takes a square matrix as an input
## and creates a list of following functions
## set the matrix
## get the matrix
## set the inverse of matrix
## get the invserse of matrix

makeCacheMatrix <- function(m = matrix()) {
  p <- NULL
  set <- function(y) {
    m <<- y
    p <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) p <<- inverse
  getinverse <- function() p
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve is a function that returns an inverse of the square matrix. 
## If matrix has already been inverted then it grabs it from the cache

cacheSolve <- function(m, ...) {
  ## Return a matrix that is the inverse of 'm'
  p <- m$getinverse()
  if(!is.null(p)) {
    message("getting cached data")
    return(p)
  }
  data <- m$get()
  p <- solve(data, ...)
  m$setinverse(p)
  p
}