## makeCacheMatrix creates a special matrix object, and cacheSolve calculates its inverse.
## If the inverse matrix has already been calculated, it is obtained in the cache, instead of calculating it again.
makeCacheMatrix <- function(x = matrix()) {
inv_x <- NULL
set <- function(y) {
x <<- y
inv_x <<- NULL
}
get <- function() x
setinverse<- function(inverse) inv_x <<-inverse
getinverse <- function() inv_x
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
## The function cacheSolve returns the inverse of a matrix created with the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if not, it computes, caches, and returns it.
cacheSolve <- function(x, ...) {
inv_x <- x$getinverse()
if (!is.null(inv_x)) {
message("getting cached inverse matrix")
return(inv_x)
} else {
inv_x <- solve(x$get())
x$setinverse(inv_x)
return(inv_x)
}
} 
