is.letter <- function(x) grepl("[[:alpha:]]", x)
split.char<-function(x) 
{
       if(class(x)=="list")
       {
              a1<-unlist(x)
              a2<-strsplit(a1,"")
              a3<-unlist(a2)
              a4<-as.numeric(a3)
       }
       else if(class(x)=="numeric")
       {
              a1 <- as.character(x) 
              a2 <- strsplit(a1, "") 
              a3 <- unlist(a2) ;
              a4 <- as.numeric(a3)
       }
       return(a4)
       
}

isin<- function(x)
{
       
       if(nchar(x)!=12){
              stop("less than 12 characters")
       }
       
       isin<-substr(x,1,11)
       check<-as.numeric(substr(x,12,12))  
       
       single<-strsplit(isin,split=NULL)
       for(i in 1:nchar(isin))
       {
              if(is.letter(single[[1]][i]))
              {
                     index<-0
                     index<-grep(single[[1]][i], LETTERS)
                     single[[1]][i]<-index+9
              }
       }
       
       single2<-split.char(single)
       for(i in seq(from=length(single2), to=1, by=-2))
       {
              single2[i]<-single2[i]*2
       }
       single3<-split.char(single2)
       s<-sum(single3)
       
       rem<-s %% 10
       res<-10-rem
       c<-res %% 10
       if(c==check){
              valid<-"TRUE"
       }else{
              valid<-"FALSE"
       }
       return(valid)
}

