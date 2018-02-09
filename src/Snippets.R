
```{r,echo=FALSE, message=FALSE, warning=FALSE}

lm.summary <- summary(lm(summ.weffort ~proj.year , isbsg2))

panderOptions('table.alignment.default','left')
pander(lm.summary, caption="Summary of the Dataset", short= FALSE, split.tables=100)

```


##Productivity Problem (Exercise Problem)

Model the realtionship between Hours Required Per FP and team size.
Hours Required Per FP = norm.prod
Team size = team.size.gp


```{r,echo=FALSE, message=FALSE, warning=FALSE}

hours.lm = lm(norm.pdr ~team.size.gp , isbsg2)

lm.summary <- summary(hours.lm)

panderOptions('table.alignment.default','left')
pander(lm.summary, caption="Summary of the Dataset", short= FALSE, split.tables=100)

```

There is positive correlation between team size and hours required to develop unit size.  When
the team size increases, the effort also increases by a factor.

Model Equation:
  
  Hours required = 15.33 + 0.042 x team size (number of team members)

## Predict Hours required per FP for 140 member team


```{r,echo=TRUE, message=FALSE, warning=FALSE}
newdata = data.frame(team.size.gp = 140) 
predict(hours.lm, newdata)
```

##Productivity Problem (Modeling with Effort, and duration )

Model the realtionship between Hours Required Per FP and team size.
Hours Required Per FP = norm.prod
Team size = team.size.gp
Duration = proj.elap.tm


```{r,echo=FALSE, message=FALSE, warning=FALSE}

days.lm = lm(proj.elap.tm ~funct.size +team.size.gp  , isbsg2)

lm.summary <- summary(days.lm)

panderOptions('table.alignment.default','left')
pander(lm.summary, caption="Summary of the Dataset", short= FALSE, split.tables=100)

```


Model Equation:
  
  Duration in Months = 7.993 + (-0.006786 x team size) + 0.00147 x FP

## Predict Months  required for 2000  FP for 140 member team


```{r,echo=TRUE, message=FALSE, warning=FALSE}

newdata = data.frame(team.size.gp = 140,funct.size = 2000) 
predict(days.lm, newdata)

```
