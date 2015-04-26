## Two functions in this file -
## 1) makeCacheMatrix is designed to take an input matrix and store it
##	and create 4 functions to a) store the matrix; b) output the matrix if called;
##	c) cache a copy of the inverse of the matrix;
##	d) store the matrix return the matrix
## 2) cacheSolve calls the functions in makeCacheMatrix to report the inverse of the matrix
##	stored by makeCacheMatrix


## makeCacheMatrix takes an input matrix x and creates/stores 4 functions
##  (1) Function set initiates variables and variable m to contain inverse of input matrix
##  (2) Fuction get returns matrix
##  (3) Function setinverse creates inverse
##  (4) Function getinverse returns inverse

makeCacheMatrix <- function(x = matrix(), ...) {
  # initiate m so that function calling makeCacheMatrix will know if this is first time called
  m <- NULL
  
  # creates new matrix if called directly
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # return matrix, this matrix generated when makeCacheMatrix called first time
  get <- function() x
  
  # creates inverse of matrix and store in parent environment
  setinverse <- function(inverse) m <<- inverse
  
  # returns inverse of matrix 
  getinverse <- function() m
  
  # creates list of functions when makeCacheMatrix called
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve takes the list of functions contained in the 
# input variable and either returns the stored/cached inverse or
# calculates and returns the inverse of the matrix passed
cacheSolve <- function(x, ...) {
  ## Generate a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## when cacheSolve is called return cached inverse if available
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## when cacheSolve is called and cached inverse has not been created
  ## create and return inverse of matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}