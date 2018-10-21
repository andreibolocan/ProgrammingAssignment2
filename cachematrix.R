## The following functions work together to create a square invertible matrix
## and make the inverse of the matrix available in the cache environment

## This function creates a special "matrix" object that can cache its inverse; 
makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix()
cacheSolve <- function(x, ...) {
        inv = x$getinv()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat.data = x$get()
        inv = solve(mat.data, ...)
        x$setinv(inv)
        return(inv)
}

## Return a matrix that is the inverse of 'x'

##TEST
##> andreib <- matrix(c(3, 6, 9, 12), 2, 2)
##> andreib_1 <- makeCacheMatrix(andreib)
##> cachesolve(andreib_1)
##        [,1]       [,2]
##[1,] -0.6666667  0.5000000
##[2,]  0.3333333 -0.1666667

