## The functions that are defined below are described in detail in the 
## cachematrix_help.html file. It provides a detailed overview and provides
## examples on the use of the functions

## The makeCacheMatrix function adds a number of functions to the provided matrix
## that allows the inverse of the matrix to be stored with it
makeCacheMatrix <- function(x = matrix()) {
  
  #Ensure that a matrix was provided
  if (class(x) != "matrix") {
    stop("x must be a matrix")
  }
  
  m <- NULL

  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #When get is called will return the original matrix
  get <- function() x
  
  #Store the inverse of the matrix for later use
  setInverse <- function(inverse) m <<- inverse
  
  #Retrieve the stored inversed matrix 
  getInverse <- function() m
  
  #Provide info on the functions in the 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##Takes a matrix and provides the inverse thereof.
##The inverse operation will only be done the first time the function is called
##Subsequent calls will return the cached version of the inversed matrix
cacheSolve <- function(x, ...) {
  
  if (class(x) == "matrix") {
    stop("Please first call the makeCacheMatrix function")
  }
  
  m <- x$getInverse()
  
  #Check if the inverse was already calculated if it was then return the cached version
  if(!is.null(m)) {
    message("Retrieving from cache")
    return(m)
  }
  
  #Get the original matrix
  data <- x$get()
  
  #Get the inverse of the matrix
  m <- solve(data, ...)
  
  #Store the inverse for later use
  x$setInverse(m)
  m
}