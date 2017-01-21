
# Coursera 
# Course: R Programming
# Assignment: Week 3
# 
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# 
# The explanation of each step in the code can be found here: https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {  #use to set new value of matrix: [matrix]$set([newmatrix()])
    x <<- y
    m <<- NULL
  }  
  get <- function() x  
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, #returns list of functions
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
