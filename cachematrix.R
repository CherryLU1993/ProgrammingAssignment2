## caching the inverse of a matrix
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

##The first function, `makeCacheMatrix` creates a special "matrix", which is
##really a list containing a function to cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) inv <<- inverse
        get_inverse <- function() inv
        list(set = set,get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## The following function,`cacheSolve` computes the inverse of the special 
## "matrix" returned by `makeCacheMatrix` above. If the inverse has already 
## been calculated (and the matrix has not changed), then `cacheSolve` should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$get_inverse()
      if(!is.null(inv)){
             message("getting cached matrix")
             return(inv)
      }
      data <- x$get()
      inv <- solve(data,...)
      x$set_inverse(inv)
      inv
}
