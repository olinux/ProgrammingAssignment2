## These utility functions cache the results of computationally expensive functions on matrices

#' This function creates the caching structure for processed matrices.
#' 
#' @param x The original (unprocessed) matrix
#' @return A list of functions allowing to access and manipulate the original as well as the cached matrix
#' @examples
#' makeCacheMatrix(matrix(c(1,2, 11,12), nrow = 2, ncol = 2))
makeCacheMatrix <- function(x = matrix()) {
        
        cachedMatrix <- NULL 
        
        ## redefine the matrix to be processed
        set <- function(m){
                x <<- m 
                cachedMatrix <<- NULL
        }
        
        ## return the original matrix
        get <- function(){
                x
        }
        
        ## assigns the processed matrix to the cache
        setCachedMatrix <-function(processedMatrix){
                cachedMatrix <<- processedMatrix
        }  
        
        ## returns the cached matrix
        getCachedMatrix <- function(){
                cachedMatrix
        }
        
        list(set = set, get = get, setCachedMatrix = setCachedMatrix, getCachedMatrix = getCachedMatrix)
}

#' This method applies the 'solve' function on a given cacheMatrix
#' 
#' @param x A cacheMatrix containing an invertible matrix
#' @return The inverted matrix
#' @examples
#' cacheSolve(makeCacheMatrix(matrix(c(1,2, 11,12), nrow = 2, ncol = 2)))
cacheSolve <- function(x, ...) {
        result <- x$getCachedMatrix()
        if(!is.null(result)){ 
                message("getting cached data")
                return(result)
        }
        originalMatrix <- x$get()
        processedMatrix <- solve(originalMatrix, ...)
        x$setCachedMatrix(processedMatrix) 
        processedMatrix
}
