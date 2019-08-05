library(DALEX)
library(rms)

#
# LOAD DATA
#

load("FICO_challenge/01_GS_prepro_tv_imputation.Robj")

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





#
# GBM MODEL
#
library(gbm)
model_fico_gbm <- gbm((RiskPerformance == "Good") ~ ., train, distribution = "bernoulli", n.trees = 10000, interaction.depth = 3)

pred_fico_gbm <- predict(model_fico_gbm, test, type = "response", n.trees = 10000)
pred_fico_gbm2 <- predict(model_fico_gbm, train, type = "response", n.trees = 10000)

# How good is this model?
# 0.7878687
mltools::auc_roc(pred_fico_gbm, test$RiskPerformance == "Good")
# 0.8199987
mltools::auc_roc(pred_fico_gbm2, train$RiskPerformance == "Good")



#
# CREATE EXPLAINERS
#

# create explainer
expl_fico_lmr_13_tuned <- explain(model_fico_lmr, test,
                                  y = test$RiskPerformance == "Good",
                                  predict_function = function(m,x) predict(m,x,type = "fitted"),
                                  label = "RMS 13vars")

expl_fico_gbm_10000 <- explain(model_fico_gbm, test,
                               y = test$RiskPerformance == "Good",
                               predict_function = function(m,x) predict(m,x,n.trees = 10000, type="response"),
                               label = "GBM 10000")

expl_fico_score_card <- explain(1, test,
                                y = test$RiskPerformance == "Good",
                                predict_function = function(m,x) 1-score2pd(score_card(1,x)),
                                label = "Score Card")


#
# PDP
#

DALEX::single_variable(expl_fico_score_card,
                       variable = "ExternalRiskEstimate",
                       type = "pdp", trans = I) -> a
DALEX::single_variable(expl_fico_gbm_10000,
                       variable = "ExternalRiskEstimate",
                       type = "pdp", trans = I) -> b
DALEX::single_variable(expl_fico_lmr_13_tuned,
                       variable = "ExternalRiskEstimate",
                       type = "pdp", trans = I) -> d
plot(a,b,d)


#
#  MODEL DOWN
#

modelDown(expl_fico_lmr_13_tuned, expl_fico_gbm_10000,expl_fico_score_card,
          modules = c("variable_response"),
          vr.vars = c("ExternalRiskEstimate", "NumInqLast6M"),
          output_folder = "summaryRMS")

#
# INDIVIDUAL CETERIS PARIBUS
# FOR 12 SELECTED CLIENTS
#

score2pd(score_card(1,test[12,]))
(select_sample(test, 12))

library(ingredients)
samp <-test[1005,]

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

library(iBreakDown)
bd_lmr_13_tuned <- break_down(expl_fico_lmr_13_tuned, samp)
bd_gbm_10000 <- break_down(expl_fico_gbm_10000, samp)
bd_score_card <- break_down(expl_fico_score_card, samp)


save(bd_lmr_13_tuned, bd_gbm_10000, bd_score_card, file = "FICO_challenge/breakdown.rds")
load("FICO_challenge/breakdown.rds")
p1 <- plot(bd_lmr_13_tuned) + theme_light() + theme(legend.position = "none")
p2 <- plot(bd_gbm_10000) + theme_light() + theme(legend.position = "none")
p3 <- plot(bd_score_card) + theme_light() + theme(legend.position = "none")
grid.arrange(p1, p2, p3, nrow = 1)


sh_lmr_13_tuned <- shap(expl_fico_lmr_13_tuned, samp)
sh_gbm_10000 <- shap(expl_fico_gbm_10000, samp)
sh_score_card <- shap(expl_fico_score_card, samp)

save(sh_lmr_13_tuned, sh_gbm_10000, sh_score_card, file = "FICO_challenge/shapley.rds")
load("FICO_challenge/shapley.rds")
p1 <- plot(sh_lmr_13_tuned) + theme_light() + theme(legend.position = "none")
p2 <- plot(sh_gbm_10000) + theme_light() + theme(legend.position = "none")
p3 <- plot(sh_score_card) + theme_light() + theme(legend.position = "none")
grid.arrange(p1, p2, p3, nrow = 1)


library(ingredients)
library(lime)
li_lmr_13_tuned <- lime(expl_fico_lmr_13_tuned, samp)
li_gbm_10000 <- lime(expl_fico_gbm_10000, samp)
li_score_card <- lime(expl_fico_score_card, samp)
plot(li_lmr_13_tuned)


#
# FEATURE IMPORTANCE
# WITH 1-AUC LOSS
#

custom_loss_auc <- function(y, yhat) {
  1 - mltools::auc_roc(yhat, y)
}

vp_lmer <- ingredients::feature_importance(expl_fico_lmr_13_tuned,
                                           loss_function = custom_loss_auc,
                                           n_sample = NULL)

vp_gbm <- ingredients::feature_importance(expl_fico_gbm_10000,
                                          loss_function = custom_loss_auc,
                                          n_sample = NULL)

vp_score <- ingredients::feature_importance(expl_fico_score_card,
                                            loss_function = custom_loss_auc,
                                            n_sample = NULL)
plot(vp_lmer, bar_width = 4) + ylab("1 - AUC")
plot(vp_score, bar_width = 4) + ylab("1 - AUC")
plot(vp_gbm, bar_width = 4) + ylab("1 - AUC")

save(vp_score, vp_lmer, vp_gbm, file = "FICO_challenge/vp.rds")

load("FICO_challenge/vp.rds")
plot(vp_score, vp_lmer, vp_gbm, bar_width = 2) + ylab("1 - AUC")


#
# PDP ONCE MORE
#

cp_score_card <- partial_dependency(expl_fico_score_card,
                                    variable_splits = list(ExternalRiskEstimate = seq(50,100,0.1),
                                                           AverageMInFile = seq(0,200,0.2),
                                                           NumInqLast6M = seq(0,10,0.1),
                                                           NetFractionRevolvingBurden = seq(0,100,0.2),
                                                           PercentTradesNeverDelq = seq(50,100,0.1),
                                                           NumRevolvingTradesWBalance = seq(0,20,0.1)),
                                    N = 100)

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
                                    variable_splits = list(ExternalRiskEstimate = seq(50,100,0.1)),
                                    N = 100)

cp_lmr_13_tuned <- partial_dependency(expl_fico_lmr_13_tuned,
                                      variable_splits = list(ExternalRiskEstimate = seq(50,100,0.1)),
                                      N = 100)

cp_gbm_10000 <- partial_dependency(expl_fico_gbm_10000,
                                   variable_splits = list(ExternalRiskEstimate = seq(50,100,0.1)),
                                   N = 100)

save(cp_score_card, cp_lmr_13_tuned, cp_gbm_10000, file = "FICO_challenge/extrisk.rds")

plot(cp_score_card, cp_lmr_13_tuned, cp_gbm_10000, 
     color = "_label_") + 
  theme_light() + theme(legend.position = "bottom") +
  xlab("Average prediction") + 
  ggtitle("","")




ext_risk <- tibble(`Score Card Points` = c(-11,-11,-2,-2,5,5,14,14), 
                   ExternalRiskEstimate = c(50,67.1, 67.1, 72.6,72.6, 81.3,81.3, 100))

ggplot(data = ext_risk, aes(x = `ExternalRiskEstimate`, y = `Score Card Points`)) +
  geom_line()+ 
  theme_light() + 
  xlab("ExternalRiskEstimate") + 
  ylab("Score Card Points")








score_card_ind <- function(m,x) {
  score <- c(385,
    c(-11,-2,5,14)[as.integer(cut(x$ExternalRiskEstimate, c(-Inf,67.1, 72.6, 81.3, Inf)))],
    c(-13,0,8)[as.integer(cut(x$AverageMInFile, c(-Inf,59.3,80.4, Inf)))],
    c(8,-3,-13)[as.integer(cut(x$NetFractionRevolvingBurden, c(-Inf,26.3,59.4, Inf)))],
    c(-24,-12,-4,5)[as.integer(cut(x$PercentTradesNeverDelq, c(-Inf,58.4,83.2,95.4, Inf)))],
    c(-3,12,21)[as.integer(cut(x$MSinceMostRecentInqexcl7days, c(-Inf,2.68,10.5, Inf)))] ,
    c(-1,30)[as.integer(x$NoValid_MSinceMostRecentInqexcl7days)] ,
    c(-8,3)[as.integer(cut(x$MSinceMostRecentDelq, c(-Inf,18.1, Inf)))] ,
    c(-16,0,7)[as.integer(cut(x$NumSatisfactoryTrades, c(-Inf,11.2,23.1, Inf)))] ,
    c(4,-2,-5)[as.integer(cut(x$NumBank2NatlTradesWHighUtilization, c(-Inf,0.383,2.4, Inf)))] ,
    c(-9,-4,1,5)[as.integer(cut(x$MSinceOldestTradeOpen, c(-Inf,87.2,134,266, Inf)))] ,
    c(4,1,-5,-8,-29)[as.integer(cut(x$PercentInstallTrades, c(-Inf,23.1,45.1,50.3,85.4, Inf)))] ,
    c(4,-1,-9,-25,-9)[as.integer(cut(x$NumRevolvingTradesWBalance, c(-Inf,3.07,5.35,11.8,13.3, Inf)))] ,
    c(2,-3,-13)[as.integer(cut(x$NumInqLast6M, c(-Inf,1.81,6.13, Inf)))] ,
    c(-6,2)[as.integer(cut(x$MaxDelq2PublicRecLast12M, c(-Inf,5.26, Inf)))])
  
  score
}

loc_points <- tibble(Variable = c("ExternalRiskEstimate", "AverageMInFile", "NetFractionRevolvingBurden",
                                  "PercentTradesNeverDelq", "MSinceMostRecentInqexcl7days",
                                  "NoValid_MSinceMostRecentInqexcl7days", "MSinceMostRecentDelq",
                                  "NumSatisfactoryTrades", "NumBank2NatlTradesWHighUtilization",
                                  "MSinceOldestTradeOpen","PercentInstallTrades",
                                  "NumRevolvingTradesWBalance", "NumInqLast6M",
                                  "MaxDelq2PublicRecLast12M"), Points = score_card_ind(1,test[1005,])[-1])

save(loc_points, file = "FICO_challenge/loc_points.rds")


load(file = "FICO_challenge/loc_points.rds")
cols <- ifelse(loc_points$Points>=0,"#008000","#E41A1C")[order(loc_points$Variable)]

ggplot(loc_points, aes(y = Points, x = reorder(Variable, Points), fill = Variable)) + 
  geom_bar(stat="identity") + 
  theme_light() +
  scale_fill_manual(values=cols) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.75), legend.position = "none") + 
  xlab("Variables") + 
  ylab("Score Card points") + 
  coord_flip()
