library(tidyverse)
df = read_csv('../../../Data_Course/Data/GradSchool_Admissions.csv')
df


#### old code for logistic regression ####
fun.LogReg = function(data, group, vars, cut.off){
  require(tidyverse)
  # Splitting variables vector
  vars.plus = NULL
  for(i in 1:length(vars)){
    vars.plus = paste(vars.plus, vars[i], sep = '+')
  }
  vars.plus = substr(vars.plus, 2, nchar(vars.plus))
  
  # Creating formula variable
  formula = paste0(group, rawToChar(as.raw(126)), vars.plus)
  
  # Full summary of data
  full.mod = glm(formula, data = data, family = binomial(link = 'logit'))
  full.summ = summary(full.mod)
  conc = survival::concordance(full.mod)
  
  # Leave one out model
  pred = NULL
  results = NULL
  for(i in 1:nrow(data)){
    test = data[i,]
    train = data[-i,]
    train.mod = glm(formula, data = train, family = binomial(link = 'logit'))
    pred[i] = predict(train.mod, type = 'response', newdata = test)
  }
  results = ifelse(pred > cut.off, 1, 0)
  
  # Sensitivity and everything
  fication = table(pull(data, group), ifelse(pred > cut.off, 1, 0))
  rownames(fication) = c('No', 'Yes')
  colnames(fication) = c('Pred No', 'Pred Yes')
  
  acc = (fication[1] + fication[4]) / nrow(data)
  sens = fication[4] / (fication[2] + fication[4])
  specif = fication[1] / (fication[1] + fication[3])
  
  chisq.stat = full.summ$null.deviance - full.summ$deviance
  chisq.p = pchisq(chisq.stat, 1)
  aic.val = full.summ$aic
  conc.val = conc$concordance
  comb = c(Chi.Sq.Value = chisq.stat, Chi.Sq.P = chisq.p, AIC = aic.val, Concordance = conc.val, 
           Accuracy = acc, Sensitivity = sens, Specificity = specif)
  
  # ROC Curve
  test.cut = seq(0,1,.01)
  test.diag = matrix(NA, 101, 5)
  for(i in 1:101){
    test.tab = table(pull(data, group), ifelse(pred > test.cut[i], 1, 0))
    test.acc = (test.tab[1] + test.tab[4]) / nrow(data)
    test.sens = test.tab[4] / (test.tab[2] + test.tab[4])
    test.specif = test.tab[1] / (test.tab[1] + test.tab[3])
    test.add = test.sens + test.specif
    test.comb = c(test.cut[i], test.acc, test.sens, test.specif, test.add)
    test.diag[i,] = test.comb
  }
  test.diag = as.data.frame(test.diag)
  colnames(test.diag) = c('Cut_Off', 'Accuracy', 'Sensitivity', 'Specificity', 'Sens + Specif')
  
  line.seg = test.diag %>%
    group_by(Specificity)%>%
    summarize(y_val = min(Sensitivity))  %>%
    mutate(x_val = 1 - Specificity) %>%
    select(x_val, y_val) %>%
    arrange(x_val) %>%
    slice(-n())
  line.seg = cbind.data.frame(line.seg, line.seg$x_val, c(line.seg$y_val[2:nrow(line.seg)], 1), 
                              c(line.seg$x_val[2:nrow(line.seg)], 1))
  colnames(line.seg) = c('x_start', 'y_start', 'x_end', 'y_end', 'x_end_two')
  
  ROC = ggplot(test.diag, aes(1 - Specificity, Sensitivity)) +
    geom_point()
  for(i in 1:(nrow(line.seg) - 1)){
    ROC = ROC +
      geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end), data = line.seg) +
      geom_segment(aes(x = x_start, y = y_end, xend = x_end_two, yend = y_end), data = line.seg)
  }
  ROC = ROC +
    geom_abline(aes(intercept = 0, slope = 1, color = 'red')) +
    geom_segment(aes(x = 0, y = 0, xend = 0, yend = line.seg[1,2])) +
    labs(title = 'ROC Curve') +
    theme(plot.title = element_text(size = 20, face = 'bold', hjust = 0.5),
          legend.position = 'none')
  
  # return list
  Model.list = list()
  Model.list[['Model']] = full.mod
  Model.list[['Summary']] = full.summ
  Model.list[['Concordance']] = conc
  Model.list[['Probability']] = full.mod$fitted
  
  loo.list = list()
  loo.list[['Probability']] = pred
  loo.list[['Predicted_Group']] = results
  class.list = list()
  class.list[['Table']] = fication
  class.list[['Diagnostic']] = comb
  class.list[['Full_Diagnostic']] = test.diag
  loo.list[['Classification']] = class.list
  
  plots.list = list()
  plots.list[['ROC']] = ROC
  scatter.resub.list = list()
  scatter.loo.list = list()
  data = cbind.data.frame(data, Pred.resub = full.mod$fitted)
  for(i in vars){
    scatter.resub.list[[i]] = ggplot(data, aes_string(i, 'Pred.resub')) +
      geom_point()
  }
  data = cbind.data.frame(data, Pred.loo = pred)
  for(i in vars){
    scatter.loo.list[[i]] = ggplot(data, aes_string(i, 'Pred.loo')) +
      geom_point()
  }
  plots.list[['Scatter_Resub']] = scatter.resub.list
  plots.list[['Scatter_LOO']] = scatter.loo.list
  plots.list[['Data_for_Plots']] = data
  
  full.list = list()
  full.list[['Model']] = Model.list
  full.list[['LOO']] = loo.list
  full.list[['Plots']] = plots.list
  
  
  return(full.list)
}



#### exploratory plots ####
ggplot(df, aes(gpa, admit, color = factor(rank), size = gre)) +
  geom_jitter(width = 0, height = .1) +
  scale_size_continuous(range = c(1,3), guide = 'none') +
  labs(x = 'GPA', y = 'Admitted?', color = 'School Tier',
       title = 'Grad School Admission vs. GPA by School Tier')
ggplot(df, aes(gre, admit, color = factor(rank), size = gpa)) +
  geom_jitter(width = 0, height = .1) +
  scale_size_continuous(range = c(1,3), guide = 'none') +
  labs(x = 'GRE Score', y = 'Admitted?', color = 'School Tier',
       title = 'Grad School Admission vs. GRE Score by School Tier')
# tough to see in either plot if there is a clear relationship
# logistic regression might be able to work



#### logistic model ####
grad_mod = fun.LogReg(df, 'admit', c('gre', 'gpa', 'rank'), .5)
grad_mod$LOO$Classification$Full_Diagnostic
# the model looks the best with a cut off of .33
grad_mod$Plots$ROC
# shows about the same thing


#### re-done logistic model ####
grad_mod2 = fun.LogReg(df, 'admit', c('gre', 'gpa', 'rank'), .33)
grad_mod2$Model$Summary
grad_mod2$LOO$Classification$Table
best.odds = exp(grad_mod2$Model$Model$coefficients%*%c(1, 800, 4, 1))
worst.odds = exp(grad_mod2$Model$Model$coefficients%*%c(1, 220, 2.26, 4))
best.prob = best.odds / (1 + best.odds)
worst.prob = worst.odds / (1 + worst.odds)
