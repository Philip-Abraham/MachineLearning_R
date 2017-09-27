# To get a complete list of the models supported by caret
library(caret)
names(getModelInfo())

# As you can see, there are plenty of models available to satisfy most needs. 
# Some support either dual use, while others are either classification or 
# regression only. You can test for the type a model supports by using the same 
# getModelInfo function:
getModelInfo()$glm$type
getModelInfo()$rf$type
getModelInfo()$glmnet$type
getModelInfo()$gbm$type
getModelInfo()$lda$type
getModelInfo()$lasso$type
getModelInfo()$ridge$type

