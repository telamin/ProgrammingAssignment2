
> # the point that can be made is that Matrix inversion is usually a costly computation
  > #
  > # and there may be some benefit
  > #
  > # to caching the inverse of a matrix rather than compute it many times
  > #
  > # The following two functions are used to cache the inverse of a matrix
  > # 
  > # makeCacheMatrix creates a list containing a function to
  > #
  > # 1
  > # set the value of the matrix
  > # 2
  > # get the value of the matrix
  > # 3
  > # set the value of inverse of the matrix
  > # 4
  > # get the value of inverse of the matrix
  > makeCacheMatrix <- function(z = matrix()) {
    + inv <- NULL
    + set <- function(y) {
      + z<<- y
      + inv <<- NULL
      + }
    + get <- function() z
    + setinverse <- function(inverse) inv <<- inverse
    + getinverse <- function() inv
    + list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    + }
> # The following function returns the inverse of the matrix. It first checks if
  > # the inverse has already been computed. If so, it gets the result and skips the
  > # computation. If not, it computes the inverse, sets the value in the cache via
  > # setinverse function.
  > # This function assumes that the matrix is always invertible.
  > cacheSolve <- function(z, ...) {
    + inv <- z$getinverse()
    + if(!is.null(inv)) {
      + message("getting cached data.")
      + return(inv)
      + }
    + data <- z$get()
    + inv <- solve(data)
    + z$setinverse(inv)
    + inv
    + }
> z = rbind(c(3, -5/8), c(-5/8, 3))
> m = makeCacheMatrix(z)
> m$get()
[,1]   [,2]
[1,]  3.000 -0.625
[2,] -0.625  3.000
> cacheSolve(m
             + )
[,1]       [,2]
[1,] 0.34845735 0.07259528
[2,] 0.07259528 0.34845735
> cacheSolve(m)
getting cached data.
[,1]       [,2]
[1,] 0.34845735 0.07259528
[2,] 0.07259528 0.34845735
> 
  