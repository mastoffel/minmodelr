#' Finds the minimal adequate model
#' 
#' Takes in a data frame and computes the minimal adequate model
#' 
#' @param df A data frame with the dependent variable in the first column and the independent variables in the following columns
#' @param test "F" or "Chisq" for deletion testing and model comparison
#' @param family glm family
#' @return Returns a list consisting of
#'      \item{ModelData}{Data frame containing the variables from the minimal adequate model}
#'      \item{BestModel}{glm object, best model}
#' 
#' @export




MinMod <- function(df, test = "F", family = gaussian) {
        ## GLM deletion testing function to get the most parsimonous model, and output results
        ## data frame format: dependent variable in the first column and independent variables in the other columns
        ## test is "Chisq" or "F"
        
        stopdeleting <- FALSE
        dfBase <- df
        
        if (test == "Chisq") {indat <- 5}
        if (test == "F") {indat <- 6}
        
        ## check case of just one independent variable and return glm if true
        if (ncol(dfBase)==2) {
                model <- glm(dfBase[, 1] ~., data = subset(dfBase, select = 2), family = family)
                print(summary(model))
                return(list(BestModelData = df,
                                BestModel = model))
        }
        
        while (stopdeleting == FALSE) {
                
                # number of variables
                numVars <- ncol(dfBase)
                
                # model1
                model1 <- glm(dfBase[, 1] ~., data = subset(dfBase, select = 2:numVars), family = family)
                
                # create pvalue vector
                pvalues <- vector()
                
                ## delete every variable, make comparison and take the least significant out
                for (i in 2:ncol(dfBase)) {
                        
                        # Remove var
                        del <- i
                        dftemp <- dfBase[, -del]
                        
                        # compute glm
                        model2 <- glm(dfBase[, 1] ~., data = subset(dftemp, select = 2:(numVars-1)), family = family)
                        
                        # ANOVA for model comparison
                        modelCompare <- anova(model1, model2, test = test)
                        
                        # get p values in vector
                        pvalues[i-1] <- modelCompare[2, indat]
                        
                }
                
                remove <- which.max(pvalues) + 1 ## +1 because dependent variable is in column one of df
                
                # stop if deleting variable with highest p value leads to a significant different model
                
        
                if (pvalues[remove-1] <= 0.05){
                        stopdeleting  <- TRUE
                        bestmodeldf <- dfBase
                        
                # stop if just one variable left
                } else if ((pvalues[remove-1] > 0.05) & ((numVars-1)==2)){
                        stopdeleting <- TRUE
                        bestmodeldf <- subset(dfBase, select = -remove)
                        
                # go on with removed variable
                } else {
                        dfBase <- dfBase[, -remove]
                }
                
                
                
        }
        
        bestmodel <- glm(bestmodeldf[, 1] ~., data = subset(bestmodeldf, select = 2:ncol(bestmodeldf)), family = family)
        
        print(summary(bestmodel))
        output <- list(ModelData = bestmodeldf,
                       BestModel = bestmodel)
        output
}
