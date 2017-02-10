

### Cache Matrix is a funtion that creates a special matrix object that can cache its inverse
### Within this are:
### a funtion that sets the value of the matrix
### another funtion which will get the value of a matrix
### a function that obtains the cached, inverted matrix



makeCacheMatrix <- function(x = matrix())                        ######  set he value of the matirx
 {
    
	value <- NULL                                              ###### hold the cached value
    
	set <- function(y) 					           ####### set the value of the inverse of the matrix
{
        
	x <<- y                                                    ####### clear cache
        
	value <<- NULL
   
 }
    
	get <- function() x                                        ###### this returns the matrix that was previosuly stored
    
	setinverse <- function(inverse) i <<- inverse              ###### cache argument      
    
	getinverse <- function() value                             ###### get value out of the cache
    
	list(
        	set = set,                                           ###### returns lists for each of these funtions
        	
		get = get,
        	
		setinverse = setinverse,
        	
		getinverse = getinverse)
}












### cacheSolve is a function that computes the inverse of the special matrix returned by makeCacheMatrix above. 
### If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
### the inverse from the cache.

cacheSolve <- function(x, ...) 
{
                                     
	value <- x$getinverse()					      ##### getting the cahed value
    
		
		if(!is.null(value))                             ##### if a cached value is present, then return the value along with a message sayin so
{
        
		
		message("fetching cached matrix")
        
	
	return(value)
}
	
	data <- x$get()                                       #### calculating inverse
    
   		i <- solve(data, ...)
    
		x$setinverse(value)
    		
		value                                           #### reutrning inverse
}



