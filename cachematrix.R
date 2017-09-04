## Matrix inversion is a computation and there might be some benfits  to caching the inverse of a matrix 
## rather than repeating the calculations. When the invert matrix is required to be calculated again, it 
## is retrived from cache directly.The following two functions are used to cache the inverse of 
## the matrix.

## makeCacheMatrix creats a list containing a function to 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of inverse of the matrix
## 4.get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
          x <<- y
          inv <<- NULL
      }
      get <- function()x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
 }

## It returns the inverted matrix stored in makeCacheMatrix().

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)){
               message("retrieving inverse from the cache")
               return(inverse)
        }
        message("calculating, setting and returning inverse")
        new_matrix <- x$get()
        inverse <- solve(new_matrix,...)
        x$setinverse(inverse)
        inverse
}