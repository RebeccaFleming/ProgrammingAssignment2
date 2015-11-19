## These functions can be used to cache the value of a matrix along 
## with its inverse.  The first time the inverse of the matrix is retrieved
## it is calculated and cached.  Each subsequent retrieval of the inverse
## of the matrix will get the cached value instead of trying to calculate the 
## inverse again

##################################################################
## makeCacheMatrix 
##
## creates a special "matrix", which is a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix 
## get the inverse of the matrix
##################################################################
makeCacheMatrix <- function(x = matrix()) {
  
        #initialise m (the inverse matrix) to null
        m <- NULL
                  
        #set function caches the value of the matrix 
        #for which the inverse will be found and resets
        #the inverse matrix to null
        set <- function(y) {
              x <<- y
              m <<- NULL
        }
                  
        #get functions returns the value of the matrix
        get <- function() x
                  
        #setInverse function caches the inverse of the matrix
        setInverse <- function(solve) m <<- solve
                  
        #getInverse function returns the inverse of the matrix
        getInverse <- function() m
                  
        #makeCacheMatrix returns a list containing the above
        #functions - set, get, setInverse and getInverse
        list(set = set
            ,get = get
            ,setInverse = setInverse
            ,getInverse = getInverse)

}


####################################################
##
## cacheSolve
##
## Retrieves the inverse of a special "matrix" that was 
## created using makeCacheMatrix.  The function first checks
## to see if the special "matrix" already holds the inverse 
## (i.e. it was created by a previous call) by calling getInverse
## If it does this value is returned and the computation skipped.  
## If not the inverse is found and cached for future
## use
####################################################################


cacheSolve <- function(x, ...) {
  
        ##check to see if the inverse of x has already been calculated and cached
        ##if it has return this value
        m <- x$getInverse()
        
        if(!is.null(m)) {
              message("getting cached data")
              return(m)
        }
        
        ##if the inverse hasn't been previously calculated retrieve the matrix
        ##find the inverse using solve() and cache the result
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
    
        ## Return a matrix that is the inverse of 'x'
        m
}
