
# makeCacheMatrix creates matrix object that stores a matrix and can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  # Decleare a store for inverse matrix
  s <- NULL
  # Rewrites matrix 'm' with new data for solving
  set <- function(n) {
    m <<- n
    # Clear cached inverse matrix
    s <<- NULL
  }
  # Brings 'm' (our matrix)
  get <- function() m
  # Sets new value of 's' from 'other.s'
  setsolve <- function(other.s) s <<- other.s
  # Brings 's' (cached inverse matrix)
  getsolve <- function() s
  # Return a list of functions to use in function cacheSolve
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


# This function computes inverse matrix for matrix object created by function 'makeCacheMatrix'

cacheSolve <- function(cmo, ...) {
  # Get cached inverse of 'cmo' matrix object
  s <- cmo$getsolve()
  # Check if cached inverse matrix is present
  if(!is.null(s)) {
    message("getting cached data")
    # Stop calculating and return cached inverse
    return(s)
  }
  # Else take new data for solving
  data <- cmo$get()
  # Solve inverse square matrix
  s <- solve(a=data, b=diag(max(dim(data)))...)
  # Write new inverse matrix from 's' to cache in 'cmo' object
  cmo$setsolve(s)
  # Return inverse matrix
  s
}
