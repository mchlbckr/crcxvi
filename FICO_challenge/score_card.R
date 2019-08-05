x <- train[1:3,]

score2pd <- function(x, pdo = 20, scoreat = 500, atodds = 50, scmod = NULL, ...){
  
  #if(!is.vector(x)) stop("x must be a numeric vector of scores!")
  
  if(!is.numeric(x)) stop("x must be a numeric vector of scores!")
  
  
  
  if(is.null(scmod)) res <- 1/(1+atodds*2^((x-scoreat)/pdo))
  
  if(!is.null(scmod)){
    
    cat("Scaling from scmod is used!\n")
    
    if(class(scmod)!= "scorecard") stop("scmod must be of class scorecard if specified!")
    
    res <- 1/(1+exp((x-scmod$scaling[2])/scmod$scaling[1]))
    
  }
  
  return(res)
  
}

score_card <- function(m,x) {
  score <- 385 +
    c(-11,-2,5,14)[as.integer(cut(x$ExternalRiskEstimate, c(-Inf,67.1, 72.6, 81.3, Inf)))] +
    c(-13,0,8)[as.integer(cut(x$AverageMInFile, c(-Inf,59.3,80.4, Inf)))] +
    c(8,-3,-13)[as.integer(cut(x$NetFractionRevolvingBurden, c(-Inf,26.3,59.4, Inf)))] +
    c(-24,-12,-4,5)[as.integer(cut(x$PercentTradesNeverDelq, c(-Inf,58.4,83.2,95.4, Inf)))] +
    c(-3,12,21)[as.integer(cut(x$MSinceMostRecentInqexcl7days, c(-Inf,2.68,10.5, Inf)))] +
    c(-1,30)[as.integer(x$NoValid_MSinceMostRecentInqexcl7days)] +
    c(-8,3)[as.integer(cut(x$MSinceMostRecentDelq, c(-Inf,18.1, Inf)))] +
    c(-16,0,7)[as.integer(cut(x$NumSatisfactoryTrades, c(-Inf,11.2,23.1, Inf)))] +
    c(4,-2,-5)[as.integer(cut(x$NumBank2NatlTradesWHighUtilization, c(-Inf,0.383,2.4, Inf)))] +
    c(-9,-4,1,5)[as.integer(cut(x$MSinceOldestTradeOpen, c(-Inf,87.2,134,266, Inf)))] +
    c(4,1,-5,-8,-29)[as.integer(cut(x$PercentInstallTrades, c(-Inf,23.1,45.1,50.3,85.4, Inf)))] +
    c(4,-1,-9,-25,-9)[as.integer(cut(x$NumRevolvingTradesWBalance, c(-Inf,3.07,5.35,11.8,13.3, Inf)))] +
    c(2,-3,-13)[as.integer(cut(x$NumInqLast6M, c(-Inf,1.81,6.13, Inf)))] +
    c(-6,2)[as.integer(cut(x$MaxDelq2PublicRecLast12M, c(-Inf,5.26, Inf)))]
  
  score
}

score2pd(score_card(1,test))

mltools::auc_roc(score_card(1,test), test$RiskPerformance == "Good")
mltools::auc_roc(score_card(1,train), train$RiskPerformance == "Good")




expl_fico_score_card <- explain(1, test,
                                y = test$RiskPerformance == "Good",
                                predict_function = function(m,x) 1-score2pd(score_card(1,x)),
                                label = "Score Card")




modelDown(expl_fico_lmr_13_tuned, expl_fico_gbm_10000,expl_fico_score_card,
          modules = c("variable_response"),
          vr.vars = c("ExternalRiskEstimate", "NumInqLast6M"),
          output_folder = "summaryRMS")


library(ingredients)
samp <- ingredients::select_sample(test, 12)

cp_lmr_13_tuned <- ceteris_paribus(expl_fico_lmr_13_tuned, samp,
                                   variable_splits = list(ExternalRiskEstimate = 0:100))

cp_gbm_10000 <- ceteris_paribus(expl_fico_gbm_10000, samp,
                                   variable_splits = list(ExternalRiskEstimate = 0:100))

cp_score_card <- ceteris_paribus(expl_fico_score_card, samp,
                                   variable_splits = list(ExternalRiskEstimate = 0:100))

plot(cp_lmr_13_tuned, cp_gbm_10000, cp_score_card,
     color = "_label_") + theme(legend.position = "bottom")+
  facet_wrap(~`_ids_`) + ggtitle("Individual responses for 12 selected clients",
                                 "ExternalRiskEstimate")

cp_lmr_13_tuned <- partial_dependency(expl_fico_lmr_13_tuned,
                                   variable_splits = list(ExternalRiskEstimate = seq(50,100,0.1),
                                                          AverageMInFile = seq(0,200,0.2),
                                                          NumInqLast6M = seq(0,10,0.1),
                                                          NetFractionRevolvingBurden = seq(0,100,0.2),
                                                          PercentTradesNeverDelq = seq(50,100,0.1),
                                                          NumRevolvingTradesWBalance = seq(0,20,0.1)),
                                    N = 100)

cp_gbm_10000 <- partial_dependency(expl_fico_gbm_10000,
                                variable_splits = list(ExternalRiskEstimate = seq(50,100,0.1),
                                                       AverageMInFile = seq(0,200,0.2),
                                                       NumInqLast6M = seq(0,10,0.1),
                                                       NetFractionRevolvingBurden = seq(0,100,0.2),
                                                       PercentTradesNeverDelq = seq(50,100,0.1),
                                                       NumRevolvingTradesWBalance = seq(0,20,0.1)),
                                N = 100)

cp_score_card <- partial_dependency(expl_fico_score_card,
                                 variable_splits = list(ExternalRiskEstimate = seq(50,100,0.1),
                                                        AverageMInFile = seq(0,200,0.2),
                                                        NumInqLast6M = seq(0,10,0.1),
                                                        NetFractionRevolvingBurden = seq(0,100,0.2),
                                                        PercentTradesNeverDelq = seq(50,100,0.1),
                                                        NumRevolvingTradesWBalance = seq(0,20,0.1)),
                                 N = 100)

plot(cp_lmr_13_tuned, cp_gbm_10000, cp_score_card,
     color = "_label_") + theme(legend.position = "bottom")+
 ggtitle("Partial Dependency for FICO",
          "")


vp_lmer <- ingredients::feature_importance(expl_fico_lmr_13_tuned,
                                           loss_function = custom_loss_auc,
                                           n_sample = NULL)

vp_gbm <- ingredients::feature_importance(expl_fico_gbm_10000,
                                           loss_function = custom_loss_auc,
                                           n_sample = NULL)

vp_score <- ingredients::feature_importance(expl_fico_score_card,
                                           loss_function = custom_loss_auc,
                                           n_sample = NULL)
plot(vp_lmer) + ylab("1 - AUC")
plot(vp_score) + ylab("1 - AUC")
plot(vp_gbm) + ylab("1 - AUC")
plot(vp_lmer, vp_gbm, vp_score, bar_width = 2) + ylab("1 - AUC")


custom_loss_auc <- function(y, yhat) {
  1 - mltools::auc_roc(yhat, y)
}
