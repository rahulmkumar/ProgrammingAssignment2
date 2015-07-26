## The functions below help with caching of the inverse calculation of an
## invertible matrix. Caching is useful when the inverse calculation 
## is required repeatedly in a program. It is better to get it from the cache 
## instead of actually re-calculating it every time it is needed.

## The makeCacheMatrix implements four function that help us with
## 1. Setting the value of a matrix using set()
## 2. Returning the value of a matrix using get()
## 3. Setting the inverse of a matrix, thereby caching it, using setinv()
## 4. Returning the inverse of a matrix from the cache, using getinv()

makeCacheMatrix <- function(x = matrix()) {
  # Set the initial value of the inverse to NULL
  inv <- NULL
  
  # the set function initializes the value of the matrix and the inverse
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  # the get function gets/returns the value of the matrix
  get <- function() x
  
  # the setinv function sets the inverse of the matrix
  setinv <- function(inverse) inv <<- inverse
  
  # the getinv function returns the cached value of the matrix
  getinv <- function() inv
  
  # return a list of available functions
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The cacheSolve function gets the value of the inverse of a matrix
## If the value of the inverse is NULL, then it calculates it and
## caches it for future use and returns the inverse of the matrix passed

cacheSolve <- function(x, ...) {
  # get the cached value of the inverse of matrix x
  inv <- x$getinv()
  
  # Find out if the data is already calculated and cached
  # Return the data if found cached
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  # If data is not cached, get the matrix and calculate the inverse
  matrix_data <- x$get()
  inv <- solve(matrix_data)
  
  # once the inverse has beeen calculated, cache it for possible future use
  x$setinv(inv)
  
  # return the inverse
  inv
}
