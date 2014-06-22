## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: return a list of functions to:
##1.set the value of the vector
##2.get the value of the vector
##3.set the value of the mean
##4.get the value of the mean
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    #store cache of inverse matrix
    inv<- NULL
    #1. set value of the matrix
    set<-function(y){
      x<<-y
      invCache<<-NULL
    }
    #2.get value for matrix
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("just a sec, retreiving cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

