## those pair of functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## x: invertible matrix
        ## return: a list containing functions to
        ##     - set the matrix
        ##     - get the matrix
        ##     - set the inverse
        ##     - get the inverse
      
        
        inv = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # that is different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


cacheSolve <- function(x, ...) {
        ## x: output of makeCacheMatrix()

        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # if not, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache
        x$setinv(inv)
        
         ## Return a matrix that is the inverse of 'x'
        return(inv)
}
