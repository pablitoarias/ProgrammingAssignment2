## These functions allows to calculate the inverse of a special matrix so if
## its value was previously calculated, a cached value will be returned
## The idea is to save computational time because the solve() function is
## expensive for large matrices

## This function creates an special "matrix" that could be used by the other
## cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULLL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, 
                setinv = setinv,
                getinv = getinv)
}


## This function will calculate the inverse of our special matrix. If 
## previously calculated it will use the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting ached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
