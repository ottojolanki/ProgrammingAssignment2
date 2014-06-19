## In this file are provided two functions. makeCacheMatrix, which creates 
## a matrix capable to cache its inverse matrix. The actual calculation and
## the(possible) caching is done in cacheSolve -function.


## The makeCacheMatrix creates a special matrix, which is a list containing
## a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv<<-inverse
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve calculates the inverse of a special matrix created by
## the makeCacheMatrix -function. It first checks if the said inverse 
## is calculated. If so, it gets the mean from the cache, skips the computation
## and returns the inverse from cache, if not it calculates the inverse, and
## returns it.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv<-x$getinv()
      ## Check if the cached value is not NULL, and if so, return it
      ## with message telling that a cached value is returned
      if(!is.null(inv)){
            message("Getting cached data.")
            return(inv)
      }
      ## So, the inverse was not cached and must be calculated. Lastly the 
      ## calculated value is going be set the cached value using setinv 
      ## -function, and returned.
      data <- x$get()
      inv <- solve(data)
      x$setinv(inv)
      return(inv)
}
