## Put comments here that give an overall description of what your
## functions do

## given a matrix m, makeCacheMatrix creates a list object for caching the
## inverse of m. Four functions are provided: a) set the matrix, b) get the matrix,
## c) set the inverse, d) get the inverse

makeCacheMatrix <- function(m = matrix()) {
    
    # Store the matrix m and its inverse
    
    m_inverse <- NULL
    
    set_matrix <- function(m_new)
    {
        m <<- m_new
        m_inverse <<- NULL   
    }
    
    get_matrix <- function()
    {
        return(m)   
    }

    set_inverse <- function(m_inv)
    {
        m_inverse <<- m_inv    
    }
    
    get_inverse <- function()
    {
        return(m_inverse)
    }
     
    list(set = set_matrix, get = get_matrix,
        setinv = set_inverse, getinv = get_inverse)
        
}


## Given an object x returned by makeCacheMatrix, cacheSolve(x) returns the
## the inverse of the matrix in x, first by checking for a cached value, and
## then (if no cached value is found) by computing the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
        
    x_inv <- x$getinv()

    if( is.null(x_inv) ) 
    {
        ## No cached solution available        
        x_inv <- solve(x$get(), ...)
        x$setinv(x_inv)
    }
    
    return(x_inv)
    
}
