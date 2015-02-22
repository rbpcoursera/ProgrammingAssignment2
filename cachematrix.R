## Provides the facilities for constructing a matrix that caches its inverse
## as well as invalidating the inverse once the matrix changes

## Constructs a wrapper around a matrix which caches it's inverse
makeCacheMatrix <- function(val = matrix()) 
{
    cachedInverse <- NULL
  
    set <- function(new_val) 
    {
        val <<- new_val
        cachedInverse <<- NULL
    }
  
    get <- function() val
  
    setInverse <- function(inverse) cachedInverse <<- inverse
  
    # Personally, I think that cacheSolve should be defined here locally
    # but not be put into the function list. Instead, when getInverse is
    # invoked, it should invoke cacheSolve (which would access local data
    # directly rather than using the functions)
    getInverse <- function() cachedInverse
  
    list(
        set = set, 
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


## Return the inverse of the wrapped matrix defined above
## Returns either the cached result of the last invocation, or it will compute
## the inverse and cache it for future access (until the matrix changes)
cacheSolve <- function(mat, ...) 
{
    inverse <- mat$getInverse()
    
    if(!is.null(inverse)) 
    {
        print("Cached")
        return(inverse)
    }
    
    # The function template takes varargs, but I'm not going to forward them
    # here since that would potentially change the meaning of the function
    # (caching a simple Matrix multiply whose result is transient)
    inverse <- solve(mat$get())
    mat$setInverse(inverse)
    inverse
}
