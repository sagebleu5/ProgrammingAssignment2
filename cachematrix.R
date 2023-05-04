## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                                ## Initialize the inv matrix to NULL
    set <- function(y) {                       ## Function to set the matrix 
      x <<- y
      inv <<- NULL
    }
    get <- function() {                        ## Function to get the matrix 
      x
    }
    setinv <- function(inverse) {              ## Function to set the inverse of the matrix
      inv <<- inverse
      getinv <- function() inv                   ## Function to get the inverse of the matrix
      list(set = set, get = get, getinv = getinv, setinv = setinv) ## List of functions
    }
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()                         ## Get the cache inverse
  if(!null(inv)) {                          ## Checks if inv is null, if inverse is not null, inverse value is returned
    message("getting cached inverse")          
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)                    ## Inverse value is calculated
  x$setinv(inv)
  inv                                      
}
