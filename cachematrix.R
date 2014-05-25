## makeCacheMatrix - a matrix that caches its inverse solution

## Usage:

## Creates an empty cached matrix cmat
#  cmat <- makeCacheMatrix()

## Creates and initializes a cached matrix cmat from matrix 'm'
#  cmat <- makeCacheMatrix(m)

## Creates and Sets the value of a cached matrix cmat from matrix 'm'
#  cmat <- makeCacheMatrix()
#  cmat$set(m)

## Compute and cache the inverse of cached matrix cmat
#  inv_cmat <- cacheSolve(cmat)

## Get the inverse of cached matrix cmat
#  inv_cmat <- cmat$getinverse()

## Create a list of functions for operating on a cached matrix
makeCacheMatrix <- function(x = matrix()) 
{
    # m holds the matrix's inverse value
    # Its a "private" member variable
    m <- NULL                # inverse value not solved, set to NULL
    
    # define the Set function: set the matrix's value
    set <- function(y) {
        x <<- y              # Store matrix value in caller's environment
        m <<- NULL           # Store matrix inverse in caller's environment
    }
    
    # define the get function: Get the matrix's value
    #   x doesn't exist in this context, 
    #   so it returns x from the parent's environment
    get <- function() x      # Return the matrix value.
    
    # define the setinverse function: Set the inverse of the cached matrix
    setinverse <- function(solve) m <<- solve
    
    # define the getinverse function: Get the inverse of the cached matrix
    getinverse <- function() m
    
    # Return a list of these defined functions to the caller
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve - calculate the inverse of a cached matrix

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
