## The following functions work together to create a square invertible matrix
## and make the inverse of the matrix available in the cache environment

## This function creates a special "matrix" object that can cache its inverse; 
makeCacheMatrix <- function(x = matrix()) { ##define the argument with model matrix
        inv = NULL ##the value of matrix inverse
        set = function(y) {  ##assign a new variable with function(y)
                x <<- y ##value of matrix
                inv <<- NULL ##if it is a new matrix, reset value to NULL
        }
        get = function() x ##assign a new variable with function(x)
        setinv = function(inverse) inv <<- inverse ##assign a new variable with (function(inverse), reset value inv to inverse
        getinv = function() inv ##get the valut of inv when is called
        list(set=set, get=get, setinv=setinv, getinv=getinv) ## run the function within the list
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix()
cacheSolve <- function(x, ...) { 
        inv = x$getinv()   ##check if inverse is cached 
        if (!is.null(inv)){
                message("getting cached data")
                return(inv) ##return the inverted matrix of x from cache
        }
        mat.data = x$get() ##if inverse has not been cached, get matrix to be inverted
        inv = solve(mat.data, ...) ##calculate inverse of the matrix using solve
        x$setinv(inv) ##update cache with the inverse
        return(inv) ##return the inverted matrix of x
}

## Return a matrix that is the inverse of 'x'

##TEST
##> andreib <- matrix(c(3, 6, 9, 12), 2, 2)
##> andreib_1 <- makeCacheMatrix(andreib)
##> cachesolve(andreib_1)
##        [,1]       [,2]
##[1,] -0.6666667  0.5000000
##[2,]  0.3333333 -0.1666667

