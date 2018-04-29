## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Matrix with internal cache for its inverse
#4 methods: set: change underlying matrix and reset cache
#           get: get underlying matrix
#           getinverse: get cache content (possibly empty)
#           setinverse: set cache content
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# using the custom matrix calculate the inverse of the
# matrix or get its cached value if present
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinverse(inv)
    inv
}


#tests using two by two matrices

m1 = matrix(c(1,0,0,2),nrow=2,ncol=2)
m2 = matrix(c(0,-1,5,0),nrow=2,ncol=2)
m3 = matrix(c(0,1,1,1),nrow=2,ncol=2)

ms=list(m1,m2,m3)


#test that the inverse is correct and that it gets correctly
#pulled from cache the second time it is calculated
for (m in ms){
  cm = makeCacheMatrix(m)
  inv1 = (cacheSolve(cm))
  cat(paste("Correct inverse",identical(m %*% inv1,diag(2))))
  inv2 = (cacheSolve(cm))
  cat(paste("Correct cache",identical(inv1,inv2),"\n\n"))
}

#same as above but with the matrix being technichally changed
#before the second call (the same value is used but the set function
# is called once). This way the cache should be erased and the inverse
# recomputed
for (m in ms){
  cm = makeCacheMatrix(m)
  inv1 = (cacheSolve(cm))
  cat(paste("Correct inverse",identical(m %*% inv1,diag(2)),'\n'))
  cm$set(m)
  inv2 = (cacheSolve(cm))
  cat(paste("Second call after modification. There should be no preceding cache message\n\n"))
}