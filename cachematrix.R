## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

### make matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){ ##set matrix
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinv<-function(inverse) inv<<-inverse
  getinv<-function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

### try to get cache for inversion of matrix

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)) { ## in case inversion already calculate
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}

