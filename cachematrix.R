## stores functions and variables associated to the functions

makeCacheMatrix <- function(x){
    inv <- NULL
    set <- function(y){
        
        rows = length(y)/2
        columns = 2
    
        inv <<- NULL
        x <<- matrix(y, rows, columns)
        
        
    }
    get <- function() x
    setInverse <- function(i) inv <<- i
    getInverse <- function() inv
        
    list(get=get, 
         set = set,
         getInverse=getInverse, 
         setInverse=setInverse)
    
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x){
    mc <- makeCacheMatrix()
    mc$set(x)
    if ( is.null(mc$getInverse()) ){
        mc$setInverse(solve(mc$get()))
    }
    mc$getInverse()
}
