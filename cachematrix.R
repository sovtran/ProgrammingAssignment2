## This function creates a special "matrix" x, that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
  inv<-NULL                                    
  setMatrix<-function(y) {                   
    x<<-y                                   
    inv<<-NULL                                
  } 
  getMatrix<-function() x                    
  setinverse<-function(inverse) inv<<-inverse() 
  getinverse<-function() inv                
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" x returned by makeCacheMatrix aboved.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv
}
