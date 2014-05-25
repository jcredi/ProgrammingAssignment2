## The two functions below can be used to create a special "matrix" and to 
## calculate and cache its inverse value.


## The first function creates a special "matrix" object. Its inverse value can be 
## stored in a variable called "cached_inverse" and it can be retrieved by the 
## function "cacheSolve" below.

makeCacheMatrix <- function(x = matrix()) {
        
        cached_inverse <- NULL ## Cache initialized to NULL.
        
        ## The following subfuntction can be use to set the value of the matrix
        ## via console. The cache is emptied every time a new value is set.
        set <- function(y) {
                x <<- y
                cached_inverse <<- NULL
        }
        
        ## The following subfunction can be used to get the value of the matrix.
        get <- function() x
        
        ## The following subfunction is used by the function "cacheSolve" below 
        ## to cache the inverse of the matrix, once it has been computed.
        savecache <- function(value_to_be_cached) cached_inverse <<- value_to_be_cached
        
        ## The following subfunction is used by the function "cacheSolve" below
        ## to retrieve the cached value of the inverse matrix, if it is not NULL.
        readcache <- function() cached_inverse
        
        ## The output of this functions is a list of subfunctions, a.k.a. "methods".
        list(
                set = set, 
                get = get,
                savecache = savecache,
                readcache = readcache
        )
}


## The second function computes the inverse of the special "matrix" created above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the function retrieves the inverse from the cache, saving computing time.

cacheSolve <- function(x, ...) {
        
        local_inverse <- x$readcache() ## The cache is readed.
        
        ## The following control structures checks whether the cache is empty.
        ## If not, the function returns the cached value.
        if(!is.null(local_inverse)) {
                message("getting cached data")
                return(local_inverse)
        }
        
        data <- x$get() ## The matrix is loaded...
        local_inverse <- solve(data, ...) ## ... and its inverse is computed.
        
        x$savecache(local_inverse) ## The computed value is stored in the cache...
        local_inverse ## ... and then returned as output of the function.
}
