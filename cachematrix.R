## This function takes a matrix as argument and returns a list of four functions that can be applied on the matrix
##set the value of the matrix
##get the value of the matrix
##set the inverse of matrix
##get the inverse of matrix

makeCacheMatrix <- function(mymatrix = matrix()) {
  myinverse <- NULL
  set <- function(y) {
    mymatrix <<- y
    myinverse <<- NULL
  }
  get <- function() mymatrix
  setInverse <- function(inverse) myinverse <<- inverse
  getInverse <- function() myinverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function compute the inverse of matrix created with the above function. 
##it first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. Otherwise, 
##it calculates the inverse of the matrix and sets it in the cache via the setInverse function.

cacheSolve <- function(mymatrix, ...) {
  myinverse <- mymatrix$getInverse()
  if(!is.null(myinverse)) {
    message("getting cached data")
    return(myinverse)
  }
  data <- mymatrix$get()
  myinverse <- solve(data)
  mymatrix$setInverse(myinverse)
  myinverse
}


## Above function can be tested using following. If the product of matrix x and the inverse matrix
## returned by cacheSolve function is identity matrix , then the answer is correct.

##x<-makeCacheMatrix(matrix(c(4,3,3,2),nrow=2,ncol=2))
##cacheSolve(x)
