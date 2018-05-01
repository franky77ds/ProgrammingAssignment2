## This Function, makeCacheMatrix, creates a special Matrix which is really a list cointaining a function to
##1 set the value of the vector
##2 get the value of the vector
##3 set the value of the mean
##4 get the value of the mean


## It´s like makeVector Function, but with a Matrix and with inverse
##Note: solve(A) returns the inverse matrix of A



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## the cacheSolve function calculates the inverse of the special "matrix" created in the above function
#However it first checks to see if the inverse has already been calculated. If so, it gets the inverse
#from the cache and skips computation. Otherwise it calculates the inverse of the data and sets the value
#of the inverse in the cache via the setinverse function

#It´s like cacheSolve int the vector example, but with the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting Cached Data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


