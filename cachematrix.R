## makeCacheMatrix lets cache inverse of a matrix. It first assigns Null to a variable, sets the matrix value, << caches the matrix and assigns value from a different environment


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL  #Assign Null to a variable
  set <- function(y){
    x <<- y # Assigns value of y from the other environment
    m <<- NULL
  }
  
  get <- function() x  # To get the cached matrix value
  setinv <- function(inverse) m <<- inverse 
  getinv <- function() m  #To get saved value of inverse matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

## This function returns inverse of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)){  #Checks if inverse has already been calculated
    message("getting cached results")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) #calculates inverse
  x$setinv(m) #Sets inverse value of the matrix in cache
  m
}