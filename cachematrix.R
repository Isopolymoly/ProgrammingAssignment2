## R Programming class ~ Coursera
## February 2015
## Programming Assignment 2



## Usage:  for square invertible matrix "a",
#    cached_a <- makeCacheMatrix(a)
#    cacheSolve(cached_a)

#   shortcut:  cacheSolve(makeCacheMatrix(a))


#  References: 
#   Programming Assignment 2 instructions & sample code for vector mean cache
#   http://stackoverflow.com/questions/11995832/inverse-of-matrix-in-r



# Instructions from https://class.coursera.org/rprog-011/human_grading/view/courses/973492/assessments/3/submissions 
# " Assignment: Caching the Inverse of a Matrix
# 
# Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.
# 
# Write the following functions:
#   
#   makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# 
# Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# 
# For this assignment, assume that the matrix supplied is always invertible."


## example makeVector from Programming Assignment 2
## 
# makeVector <- function(x = numeric()) {
#   m <- NULL
#   set <- function(y) {
#     x <<- y
#     m <<- NULL
#   }
#   get <- function() x
#   setmean <- function(mean) m <<- mean
#   getmean <- function() m
#   list(set = set, get = get,
#        setmean = setmean,
#        getmean = getmean)
# }



#   makeCacheMatrix: create a "matrix" object that can cache its inverse, 
#  similar to makeVector function


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


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## example cachemean from assignment 
# cachemean <- function(x, ...) {
#   m <- x$getmean()
#   if(!is.null(m)) {
#     message("getting cached data")
#     return(m)
#   }
#   data <- x$get()
#   m <- mean(data, ...)
#   x$setmean(m)
#   m
# }


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Solve function arguments:  if argument "b" is missing, solve returns inverse of a
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



