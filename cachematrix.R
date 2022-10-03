## Put comments here that give an overall description of what your
## functions do
##please see comments throughout code which describe each step

## Write a short comment describing this function
## This function creates a special "matrix" object which can be used to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL ##setting the inverse to NULL as a placeholder for a future value
  set <- function(y) { ## defines a function to set the matrix, x, to a new matrix, y, and resets the inverse, inv, to NULL
    x <<- y
    inv <<- NULL
  }
  get <- function() x  ##x returns the matrix, x
  setInverse <- function(inverse) inv <<- inverse ##inverse sets the inverse, inv, to inverse
  getInverse <- function() inv ##inv returns the inverse, inv
  list(set = set, ## returns the 'special matrix' containing all of the functions just defined
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  }




## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        
  inv <- x$getInverse()
  if (!is.null(inv)) { ##checking whether the inverse is null
    message("getting cached data")
    return(inv) ##returns the inverse
  }
  data <- x$get()
  inv <- solve(data, ...) ##calculate inverse
  x$setInverse(inv)
  inv ## Return a matrix that is the inverse of 'x'
}
