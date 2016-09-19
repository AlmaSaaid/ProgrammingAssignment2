## Programming Assignment 2: Lexical Scoping
## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
## Return values: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
## This list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  } #end of set function
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set = set, get = get, 
       setinv = setinv, getinv = getinv)
} #end of makeCacheMatrix function


## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
## Return: inverse of the original matrix input to makeCacheMatrix().
## Use solve function. 
## If X is a square invertible matrix, then solve(X) returns its inverse.

cacheSolve <- function(x, ...) 
{
  
  #get the inverse from special "matrix"
  inv = x$getinv()
  
  # if the inverse had been calculated
  if (!is.null(inv))
  {
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # else calculates the inverse 
  matrix.data = x$get()
  inv = solve(matrix.data, ...)
  
  # sets the value of the inverse in the cache using the setinv function.
  x$setinv(inv)
  
  return(inv)
} # end of cacheSolve function