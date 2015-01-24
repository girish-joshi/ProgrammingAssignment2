## Put comments here that give an overall description of what your
## functions do
## This Program is to compute inverse of a Matrix using cache operations.
## If an inverse of Matrix object was computed before, 
## then the program will not compute the inverse again. 
## Instead it will return the cached inverse in this case.


## Write a short comment describing this function
## This function creates a special Matrix from the input Matrix
## This special matrix contains a list of functions defined over the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## This functions returns the matrix x
  get <- function() x
  
  ## This functions stores the input matrix into "m";
  ## such that "m" is stored in parent environment
  ## i.e. "m" is cached by this function
  setMatrix <- function(Matrix) m <<- Matrix
  
  ## This functions returns the matrix "m" from parent environment or from cache;
  getMatrix <- function() m
  
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}


## Write a short comment describing this function
## This function solves inverse of matrix and returns it
## It first checks if the inverse is already computed or not
## if it is already computed, then it retunrs the cached inverse
## else if computes the inverse, and stores the same in cache

cacheSolve <- function(x, ...) {
        
  m <- x$getMatrix()
  if(!is.null(m)) {
    ## If inverse is computed already, program will enter in this part
    message("getting cached data")
    return(m)
  }
  ## Inverse was not computed ealier
  data <- x$get()
  ## Compute inverse
  m <- solve(data, ...)
  ## Store in cache
  x$setMatrix(m)
  ## Return a matrix that is the inverse of 'x
  m
}
