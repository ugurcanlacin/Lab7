#' @title Linear Regression
#' @description You can have Reference Class containing some calculations by giving formula and data.
#' @field formula Formula
#' @field data A data frame
#' @export ridgereg
#' @exportClass ridgereg
ridgereg <- setRefClass("ridgereg",
                      fields = list(formula="formula", 
                                    Fits="numeric",
                                    Coef="numeric",
                                    data="data.frame",
                                    Call="character",
                                    lambda="numeric",mat="matrix"),
                      
                      methods = list(
                        initialize = function(formula = formula, data = data,lambda=0){
                          
                          x<-model.matrix(formula,data)
                          x[,2:ncol(x)]<-apply(x[,2:ncol(x)],2,function(a) (a-mean(a))/sd(a))
                          
                          y<-all.vars(formula)[1]
                          y<-as.matrix(data[,names(data)==y])
                          temp_data <- data.frame(cbind(x[,2:ncol(x)],y))
                          names(temp_data)[ncol(temp_data)]<-all.vars(formula)[1]
                          data <<- temp_data
                          
                          I_lambda<-matrix(nrow=ncol(x),ncol=ncol(x),data = 0)
                          b_hat<-(solve((t(x)%*%x)+I_lambda))%*%(t(x)%*%y)
                          y_fits<-x%*%b_hat
                          
                          coef<-as.numeric(b_hat)
                          names(coef)<-rownames(b_hat)
                          
                          y_fits<-as.numeric(y_fits)
                          names(y_fits)<-rownames(y_fits)
                          
                          Fits <<- y_fits
                          Coef <<- coef
                          formula<<-formula
                          data<<-data
                          
                        },
                        print = function(){
                          "This function prints regression coefficients by using given formula and data in initialization."
                          cat("Call:",sep="\n")
                          cat(paste("ridgereg(","formula = ",formula[2]," ",formula[1]," ",formula[3],", ","data = ",deparse(substitute(data)),")",sep=""), sep="\n")
                          cat(sep="\n")
                          cat("Coef:")
                          cat(sep="\n")
                          
                          beta<-Coef
                          namn<-names(beta)
                          names(beta)<-NULL
                          beta<-round(beta,4)
                          
                          for(i in 2:length(beta)){
                            beta[i]<-format(beta[i], width=max(nchar(beta[i]),nchar(namn[i])),justify = "right")
                          }
                          
                          beta[1]<-format(beta[1], width=max(nchar(beta[1]),nchar(namn[1]),nchar("Coef")),justify = "right")
                          namn[1]<-format(namn[1], width=max(nchar(beta[1]),nchar(namn[1]),nchar("Coef")),justify = "right")
                          
                          beta[1]<-paste(beta[1],"  ",sep="")
                          namn[1]<-paste(namn[1],"  ",sep="")
                          
                          beta[2]<-paste(beta[2]," ",sep="")
                          namn[2]<-paste(namn[2]," ",sep="")
                          
                          cat(" ")
                          cat(namn)
                          cat(" ")
                          cat(sep="\n")
                          cat(beta)
                          
                        },
                        predict = function(){
                          return(Fits)
                        },
                        coef = function(){
                          return(Coef)
                        }
                      ))

# library (MASS)
# lm.ridge(Petal.Length ~ Species, iris)
# 
# ridgereg <-  ridgereg$new(Petal.Length ~ Species, data=iris)
# ridgereg$print()
# ridgereg$predict()
# ridgereg$coef()






