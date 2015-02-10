## R Programming class ~ Coursera
## February 2015
## Programming Assignment 2

## Usage:  for square invertible matrix "a",
#    cached_a <- makeCacheMatrix(a)
#    cacheSolve(cached_a)

#   shortcut:  cacheSolve(makeCacheMatrix(a))


#  References: 
#   [1] Programming Assignment 2 instructions & sample code for vector mean cache
#   [2] http://stackoverflow.com/questions/11995832/inverse-of-matrix-in-r


# Assignment instructions are in README.rd  (original README file, no further edits by me)


#  makeCacheMatrix: create a "matrix" object that can cache its inverse, 
#  similar to makeVector function example in assignment instructions
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


# cacheSolve: compute the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), then 
#   cachesolve should retrieve that previously calculated inverse matrix from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Solve function arguments:  if argument "b" is missing, solve() returns inverse of "a" argument
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("retrieving cached inverted data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}



