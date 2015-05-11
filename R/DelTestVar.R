#' Single variable deletion testing
#' 
#' Tests for the significance of a single variable by deleting it from the model. The full model is then tested 
#' against the reduced model with F-tests and Chisquared-tests. Explained deviance per variable is computed by substracting 
#' the explained deviance from the reduced model (by ne variable) from the deviance explained by the full model.
#' 
#' @param yX A data frame with the dependent variable in the first column and the independent variables in the following columns
#' @param family Specifies glm type (e.g. "gaussian","binomial")
#' 
#' @return Data frame with estimates, explained deviance, F-value, p-value (F-test), p-value (Chisq-test) per variable. 
#' 
#' @export


DelTestVar <- function(yX, family = "gaussian") {
        
        
        # define Base data and Full Model
        dfBase <- yX
        FullModel <- glm(dfBase[, 1] ~., data = dfBase[2:ncol(dfBase)], family = family)
        devExplFull <- (FullModel$null.deviance - FullModel$deviance)/FullModel$null.deviance
        
        ## define data frame
        if (sum(sapply(yX, is.factor) > 0)) { # if factors in the data frame
                ValuesDf <- data.frame(matrix(data=NA, nrow = ncol(yX), ncol = 5))
                names(ValuesDf) <- c("estimate", "DevianceExplained",
                               "Fval", "Ftest_Pval", "Chitest_Pval")
                rownames(ValuesDf) <- names(yX)
                
        } else {
                ValuesDf <- data.frame("estimate" = FullModel$coefficients, "DevianceExplained" = NA,
                                       "Fval" = NA, "Ftest_Pval" = NA, "Chitest_Pval" = NA)    
        }
        
        
        for (i in 2:ncol(dfBase)) {
                
                # Remove var
                del <- i
                dftemp <- dfBase[-del]
                
                numVars <- ncol(dftemp)
                
                if (numVars == 1) {
                        modeltemp <- glm(dfBase[, 1] ~ 1, family = family)      
                } else {
                # compute glm
                modeltemp <- glm(dfBase[, 1] ~., data = subset(dftemp, select = 2:(numVars)), family = family)
                }
                # likelihood ratio test 
                modelCompareF <- anova(FullModel, modeltemp, test = "F")
                modelCompareChi <- anova(FullModel, modeltemp, test = "Chisq")
                
                # Get Difference in Deviance Explained
                devExpltemp <- (modeltemp$null.deviance - modeltemp$deviance)/modeltemp$null.deviance
                devExplVar <- devExplFull - devExpltemp
                
                # fill in values
                ValuesDf$DevianceExplained[i] <- devExplVar*100
                ValuesDf$Fval[i] <- modelCompareF[2, 5]
                ValuesDf$Ftest_Pval[i] <- modelCompareF[2, 6]
                ValuesDf$Chitest_Pval[i] <- modelCompareChi[2, 5]
        }
        
        names(ValuesDf) <- c("Estimate","Deviance Explained", "F", "P (F-test)", "P (Chisquared-test)")
        print(ValuesDf)
        Out <- ValuesDf
}