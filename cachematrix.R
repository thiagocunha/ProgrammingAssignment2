## This file was created as part of programming assignment 2, 
## coursera R programming course

## This method will create a cached version of a Matrix
## that should return the results faster after the first call

makeCacheMatrix <- function(x = matrix()) {
  # Initiallizing the inverse matrix
  inv <- NULL
  
  # Allow setting the base matrix 
  set <- function(y) {
    x <<- y
    # Reset the inverse matrix
    inv <<- NULL
  }
  
  # Return the base matrix
  get <- function() x
  
  # Allow manually setting the inverse matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getInverse <- function() inv
  
  # Encapsulate into a list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)	
}

## Return a matrix that is the inverse of 'x'.
## This method will handle the call and use of the 
## matrix creation, caching and reusing

cacheSolve <- function(x, ...) {
  
  ## Is there a cached version?
  inv <- x$getInverse()
  
  # If exists
  if(!is.null(inv)) {
    # Return the cached inverse		
    message("Cached matrix")
    return(inv)
  }
  
  # Else
  # Get the matrix 
  data <- x$get()
  
  # Find the inverse
  inv <- solve(data, ...)
  
  # Cache the result
  x$setInverse(inv)
  
  # Return the result
  inv    
}
