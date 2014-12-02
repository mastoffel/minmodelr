#' Finds the minimal adequate model
#' 
#' Takes in a data frame and computes the minimal adequate model
#' 
#' @param yX A data frame with the dependent variable in the first column and the independent variables in the following columns
#' @param test "F" or "Chisq" for deletion testing and model comparison
#' @param family glm family
#' @return Returns a list consisting of
#'      \item{ModelData}{Data frame containing the variables from the minimal adequate model}
#'      \item{BestModel}{glm object, best model}
#' 
#' @export




MinMod <- function(yX, test = "F", family = gaussian) {

        stopdeleting <- FALSE
        # get df with indpendent vars
        dfBase <- subset(yX, select = 2:ncol(yX))
        
        ## get vector with dependent var
        depVar <- yX[, 1]
        names(depVar) <- names(dfBase)[1]
        
        ## which test is used
        if (test == "Chisq") {indat <- 5}
        if (test == "F") {indat <- 6}
        
        ## check case of just one independent variable and return glm if true
        if (ncol(dfBase)==2) {
                model <- glm(depVar ~., data = subset(dfBase, select = 1), family = family)
                print(summary(model))
                return(list(ModelData = df,
                            BestModel = model))
        }
        
        while (stopdeleting == FALSE) {
                
                # number of variables
                numVars <- ncol(dfBase)
                # model1
                model1 <- glm(depVar ~., data = dfBase, family = family)
                # create pvalue vector
                pvalues <- vector()
                ## delete every variable, make comparison and take the least significant out
                for (i in 1:ncol(dfBase)) {
                        
                        # Remove var
                        del <- i
                        dfTemp <- dfBase[-del]
                        # compute glm
                        model2 <- glm(depVar ~., data = dfTemp, family = family)
                        # ANOVA for model comparison
                        modelCompare <- anova(model1, model2, test = test)
                        # get p values in vector
                        pvalues[i] <- modelCompare[2, indat]
                        
                }
                
                remove <- which.max(pvalues)  ## +1 because dependent variable is in column one of df
                
                # stop if deleting variable with highest p value leads to a significant different model
                
                
                if (pvalues[remove] <= 0.05){
                        stopdeleting  <- TRUE
                        bestmodeldf <- dfBase
                        
                        # stop if just one variable left in dfTemp
                } else if ((pvalues[remove] > 0.05) & ((numVars)==2)){
                        stopdeleting <- TRUE
                        bestmodeldf <- subset(dfBase, select = -remove)
                        
                        # go on with removed variable
                } else {
                        dfBase <- dfBase[, -remove]
                }
                 
        }
        
        bestmodel <- glm(depVar ~., data = bestmodeldf, family = family)
        print(summary(bestmodel))
        
        bestmodeldf <- as.data.frame(cbind(depVar,bestmodeldf))
        
        output <- list(ModelData = bestmodeldf,
                       BestModel = bestmodel)
        output
}
