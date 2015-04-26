## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following pair of functions cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(y){
        x <<- y
        cache <<- NULL
    }
    get <- function() x
    setMatrix <- function(inverse) 
        cache <<- inverse
    getInverse <- function() 
        cache
    list(set = set, get = get, setMatrix = setMatrix, getInverse = getInverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated then 
## cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    cache <- x$getInverse()
    if (!is.null(cache)) {
        message("retrieving cached data")
        return(cache)
    }
    matrix <- x$get()
    cache  <- solve(matrix)
    x$setMatrix(cache)
    cache
}