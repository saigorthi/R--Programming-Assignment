## Put comments here that give an overall description of what your
## functions do

###########################################################################
####Create a function in R to caluclate the inverse of a matrix(square)### 
##########################################################################
##Steps Involved:                                                      ###
##1.set the matrix                                                     ###
##2.get the matrix                                                     ###
##3.set the inverse of the matrix                                      ###
##4. get the inverse of the matrix                                     ###
##<<- operator- to to assign a value to an object in an                ###
##environment that is different from the current environment           ###
##########################################################################

## Write a short comment describing this function

#############################################################################
#makecachematrix-function creates an object to cache the inverse of a matrix#
#############################################################################


makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y) {
                x <<- y
                matinv<<- NULL
        }
        get   <- function() x
        setinv<- function(solvematrix) matinv<<-solvematrix
        getinv<- function() matinv
        list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## Write a short comment describing this function


##########################################################################
##cacheSolve-functions caluclates the inverse of the matrix,or retrieves##
##it if already caluclated#################################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matinv<- x$getinv()
        if(!is.null(matinv)){
                              message("retriving cached data")
                              return(matinv)
                            }
        data<-x$get()
        matinv<- solve(data)
        x$setinv(matinv)
        matinv
}
