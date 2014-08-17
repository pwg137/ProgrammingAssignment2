## These functions create a special type of matrix.  Computations performed
## on this matrix can be cashed in order to avoid repeating them.  This will
## be demonstrated with the inverse matrix.

## The function makeCacheMatrix creates the specialized matrix.

makeCacheMatrix <- function(x) {
        ## This creates a special matrix.
        m<-NULL
        set<-function(y){
               x<<-y
               m<<-NULL
        }
        get<-function()x
        setsolve<-function (solve)m<<-solve
        getsolve<-function()m
        list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## The function cacheSolve computes the inverse of the matrix and caches it
## or retrieves the inverse if it already exists.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getsolve()
        if(!is.null(m[1,1])){
            message("getting chached data")
            return(m)
        }
        data<-x$get()
        m<-solve(data)
        ## I am assuming that I only need to take the inverse,
        ## and that tolerance is not an issue.  So I am not 
        ## including other parameters.
        x$setsolve(m)
        m
  
}



