## Put comments here that give an overall description of what your
## functions do

## the makeCacheMatrix function will create a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse 
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function computes the inverse of the special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversematrix <- x$getinverse()
        if(!is.null(inversematrix)) {
                message("getting cached matrix")
                return(inversematrix)
        }
        mat <- x$get()
        inversematrix <- solve(mat, ...)
        x$setinverse(inversematrix)
        inversematrix
}
