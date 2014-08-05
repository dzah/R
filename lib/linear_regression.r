Чтобы при линейной регрессии использовать как координаты все столбцы:
http://stackoverflow.com/questions/5774813/short-formula-call-for-many-variables-when-building-a-model
You can use . as described in the help page for formula. The . stands for "all columns not otherwise in the formula".
lm(output ~ ., data = myData).
Alternatively, construct the formula manually with paste. This example is from the as.formula() help page:
xnam <- paste("x", 1:25, sep="")
(fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+"))))
You can then insert this object into regression function: lm(fmla, data = myData).

http://stackoverflow.com/questions/3384567/how-do-i-fit-a-model-without-specifying-the-number-of-variables
There are two special interpretations of ‘.’ in a formula. The usual one is in the context of a ‘data’ argument of model fitting functions and means ‘all columns not otherwise in the formula’: see ‘terms.formula’. In the context of ‘update.formula’, only, it means ‘what was previously in this part of the formula’.

