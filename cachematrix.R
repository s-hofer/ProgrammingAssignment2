## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inversematrix <- NULL                                   # The inverse matrix will be stored here, set to null for now
  
  makeinverse <- function(y) {                              # This function set the value of vector
    x <<- y                                   # update the old matrix to the new matrix
    inversematrix <<- NULL                          # reset the inverse of the matrix of the new matrix.
  }
  getmat <- function() x                               # This function get the actual matrix
  setinverse <- function(solve) inversematrix <<- solve   # Here the inverse of the matrix is computed
  getinverse <- function() inversematrix                 # This one gets the inverse of the matix
  
  
  
  list(makeinverse = makeinverse, getmat = getmat,                        # Available functions
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()                     # holds the inverse of the matrix
  
  
  if(!is.null(inverse)) {                       # First check if the inverse of the matrix is already calculated.
    message("Inverse has already been calculated - getting cached data.")        # Prints a message and
    return(inverse)                       # then returns the inverse of the matrix and skips the computation.
  }
  
  
  #If the inverse has not been calculated:
  data <- x$getmat()                            # The actual matrix is stored in Data.     
  inverse <- solve(data, ...)                   # Calculating the inverse of the matrix using solve
  x$setinverse(inverse)                         # Last update the variable that holds the inverse of the matrix
  inverse                     
}
