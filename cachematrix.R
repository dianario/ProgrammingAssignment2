## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this first function creates a special "matrix", which is a 
## list containing functions to set and get its value, as well as set and get 
## the value of its inverse. eg. 'set' assigns the value of the input matrix to 
## the x matrix in the global environment; when the makeCaheMatrix 
## function is called, it will return a list of functions that can be used to 
## manipulate the vector and its associated mean value

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
         x <<-y    
         inv <<- NULL
         }
  get <- function() x
  setinvmtx <- function(invmatrix) {
    inv <<- invmatrix
  }
  getinvmtx <<-function() inv
  list(set = set, get = get, 
       setinvmtx = setinvmtx, getinvmtx = getinvmtx)
}




## Write a short comment describing this function
## This function first checked if the inverse of the matrix has been cached by 
## calling the getinvmxt method on it; If it has a value, it returns the cached 
## data, else it gets the matrix data, computes its inverse, stores it in the 
## inv variable, caches it by calling the setinvmtx method and returns it.

cacheSolve <- function(x, ...) {
  inv <- x$getinvmtx()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinvmtx(inv)
  inv
}
