## Programming Assignment 2 - Caching the Inverse of a Matrix
# See https://github.com/hpmcwill/ProgrammingAssignment2
#
# Functions to calculate the inverse of a square matrix which add support for 
# caching the result for subsequent reuse instead of recalculation.
#
# Example usage:
#
# sqmatrix <- matrix(rnorm(400), 20, 20) # Generate a square matrix.
# matrix <- makeCacheMatrix(sqmatrix) # Add support for matrix inverse caching.
# print(cacheSolve(matrix)) # Calculate the matrix inverse
# print(cacheSolve(matrix)) # Fetch the matrix inverse from the cache.

## makeCacheMatrix()
#
# Create a list containing a set of methods for accessing the square matrix 
# input data 'sqmatrix' and its cached inverse value.
#
# Usage:
#   matrixWithMethods <- makeCacheMatrix(sqmatrix)
#
# The returned list contains the functions:
# * set(data) - set the matrix data
# * get() - fetch the matrix data
# * setmean(mean) - set the cached inverse matrix
# * getmean() - fetch the cached inverse matrix
#
makeCacheMatrix <- function(sqmatrix = matrix()) {
  # Cached value placeholder.
  inversematrix <- NULL
  # Set data
  set <- function(y) {
    sqmatrix <<- y # Assign data to sqmatrix.
    inversematrix <<- NULL # Clear cached value.
  }
  # Fetch data.
  get <- function() sqmatrix
  # Set the cached value
  setinverse <- function(inverse) inversematrix <<- inverse
  # Fetch the cached value.
  getinverse <- function() inversematrix
  # Return list of functions.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve()
#
# Caculate the inverse of the square matrix 'x'.
#
# If the inverse of 'x' has already been caculated the cached value will be 
# returned instead of recaculating the inverse of the matrix. See 
# 'makeCacheMatrix()' for details of how to create the input matrix in a form 
# which supports the caching mechanism.
#
# Usage:
#   inverseMatrix <- cacheSolve(matrixWithMethods)
#
cacheSolve <- function(x, ...) {
  # Fetch cached value from object.
  inverse <- x$getinverse()
  # If value set in cache use it.
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # Otherwise...
  # Fetch data and calculate inverse.
  data <- x$get()
  inverse <- solve(data, ...)
  # Store caculated value in cache.
  x$setinverse(inverse)
  # Return calculated value.
  inverse
}

