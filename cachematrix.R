## Coursera R Programming Assignment 
## week 3 assignment March 19 2018 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      invMatrix <- NULL	
    	
      
      setMatrix <- function(y) {	
           x <<- y	
           invMatrix <<- NULL	
         }	
      	
      getMatrix <- function() x                              	
      setInverse <- function(inverse) invMatrix <<- inverse   
      getInverse <- function() invmatrix
      list(setMatrix = setMatrix,getMatrix = getMatrix,	
      setInverse = setInverse,
      getInverse = getInverse)	
        
  }
         



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
       message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
  }	


