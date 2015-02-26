## Below are two functions that are used to create a special object
## that stores a matrix and cache's its inverse
## The first function, makeCacheMatrix() creates a special "matrix",
## which is really a list containing a function to:
## set the value of the matrix x
## get the value of the matrix x
## set the value of the inverse i
## get the value of the inverse i

makeCacheMatrix <- function(x = matrix()) {

  ## Initialize the inverse matrix
  inv <- NULL
  
  ## Set the value of the matrix
  set <- function(y) {
  x <<- y
  inv <<- NULL
  }

  ## Get the value of the matrix
  get <- function() {
  x #Return the matrix
  }

  ## Set the value of inverse of the matrix
  set.Inv <- function(inverse) {
  inv <<- inverse
  }

  ## Get the value of the inverse of the matrix
  get.Inv <- function() {
  inv #Return the inverse matrix
  }

  ## Return the list
  list(set = set, get = get, set.Inv = set.Inv, get.Inv = get.Inv)
}

## The second function, cacheSolve() calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse has
## already been calculated. If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it calculates the inverse of the data and sets
## the value of the inverse in the cache via the set.Inv function.
## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  inv <- x$get.Inv()
  
  ## Return the inverse if it is already set
  if(!is.null(inv)) {
  message("getting cached data")
  return(inv)
  }

  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse
  inv <- solve(data)
  
  ## Set the inverse to the object
  x$set.Inv(inv)
  
  ## Return the inverse
  inv
}

## Sample run:
## x = rbind(c(1, -1/4), c(-1/4, 1))
## x
## [,1] [,2]
## [1,] 1.00 -0.25
## [2,] -0.25 1.00
## m = makeCacheMatrix(x)
## cacheSolve(m)
## [,1] [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
