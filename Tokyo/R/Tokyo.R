# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

Tinah<-function(y2){
  g<-ncol(y2[sapply(y2,is.numeric)])
  y2$Total<-rowSums(y2[sapply(y2,is.numeric)])
  y2$Average<-(y2$Total)/g

  ####Introducing Grading here..........................
  y2$Grade<-ifelse(y2$Average>=70& y2$Average<=100,'A',
                   ifelse(y2$Average>=60 & y2$Average<70,'B',
                          ifelse(y2$Average>=50 & y2$Average<60,'C',
                                 ifelse(y2$Average>=40 & y2$Average<50,'D',
                                        ifelse(y2$Average>=0  & y2$Average<40,'E','NULL')))))


  ###commenting if First Class second class, e.tc........
  y2$Remark<-ifelse(y2$Grade=="A","FIRST CLASS",
                    ifelse(y2$Grade=="B","SECOND CLASS",
                           ifelse(y2$Grade=="C","SECOND UPPER CLASS",
                                  ifelse(y2$Grade=="D"," PASS",
                                         ifelse(y2$Grade=="E","FAILED ","RETAKE")))))

  ##In cases where we would like to sort the data in desceding order
  ##we must call this Library First
  library("dplyr")
  y4<-arrange(y2,desc(Average))
  y4
}

