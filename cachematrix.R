## These functions create a square invertible matrix and can compute
## and cache the inverse of the matrix

## Takes a square, invertible matrix as input and creates a special 
## "matrix" object that can cache its inverse
## The function returns a list containing functions to:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    # when set function is invoked, assign cache value of inv to NULL
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}

## Computes the inverse of the matrix returned by "makeCacheMatrix()
## If the inverse has already been calculated, it retrieves the inverse
## from the cache directly

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## x has to be the output of makeCacheMatrix
  
  inv <- x$getinv()
  
  # check if inverse has already been calcuated
  if(!is.null(inv)) {
    #if calculated, get it from cache and skip computation
    message("getting cached data")
    return(inv)
  }
  
  # else, calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # set value of the inverse in the cache via the setinv function
  x$setinv(inv)
  inv
}
