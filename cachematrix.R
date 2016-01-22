#R programming assignment 2

#This function creates a special "Matrix" - actually it is a list, where
#data are stored. Function makeCacheMatrix cooperates with function cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function()x
    setinverse<- function(inverse) i<<-inverse
    getinverse<-function()i
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


#This function computes inverse matrix of given matrix and then stores
#the result into a list created by makeCacheMatrix function. When cacheSolve is
#used to compute an inverse matrix of matrix computed before, then the function
#takes result from the list instead of computing it again. If the matrix has
#changed then it computes new inverse matrix and again stores it in list
#created by makeCacheMatrix fuction.

cacheSolve <- function(x, ...) {
    i<-x$getinverse()
    if(!is.null(i))
    {
        message("getting cached data")
        return(i)
    }
    data<-x$get()
    i<-solve(data, ...)
    x$setinverse(i)
    i
}
