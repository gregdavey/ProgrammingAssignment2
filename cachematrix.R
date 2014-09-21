## These functions cache the inverse of a matrix


## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## For the purposes of this assignment it is assumed that the matrix supplied is always invertible.
# makeCacheMatrix containings functions to
#    set the value of the matrix
#    get the value of the matrix
#    set the inverse value of the matrix
#    get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { # set the value of the matrix (x) to (y)
    x <<- y
    inv <<- NULL
  }
  get <- function() x # get the value of the matrix (x)
  setinverse <- function(solve) inv <<- solve # set the inverse value of the matrix
  getinverse <- function() inv # get the inverse value of the matrix (inv)
  list(set = set, get = get, #return a list of functions
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the matrix returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## For the purposes of this assignment it is assumed that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() # set inv to inverse of the matrix in makeCacheMatrix
  if(!is.null(inv)) { # if inv is not null then return the value of the cached matrix inverse
    message("getting cached data")
    return(inv)
  }
  mat <- x$get() # if inv is null then get the value of the matrix from makeCacheMatrix 
  inv <- solve(mat) # calculate the inverse of the matrix
  x$setinverse(inv) # set the inverse of the matrix in makeCacheMatrix
  inv # returned the inverted matrix
}
