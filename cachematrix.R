## Caching the inverse of a Matrix instead of calculating it everytime 
## is both time and cost saving.
## The following is mainly a pair of functions that are used to store a matrix and 
## cache its inverse.
## The first function,  makeCachMatrix  creates a special "Matrix",
## which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

## the second function cacheSolve calculates the inverse of the special "matrix" created with the above function.
## it first checks if the inverse matrix has already been calculated.
## If so, it  uses the cache and skips the computation.
## Otherwise, it calculates the inversion of the matrix
# and sets the value of the inverse matrix in the cache via the  setinverse  function.


## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse <-function(solve) m<<- solve
  getinverse <-function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## cacheSolve function computes the inverse of the special "matrix" returned by  makeCacheMatrix  above.
## If the inverse has already been calculated,
## then  cacheSolve  should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m<-solve(matrix, ...)
  x$setinverse(m)
  m
}
