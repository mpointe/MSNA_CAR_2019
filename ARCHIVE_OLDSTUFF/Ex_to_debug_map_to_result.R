repeat.var = 'admin_1'
independent.var = 'ig_8_statut'
independent.var.type = 'categorical'
dependent.var = 'ig_1_age'
dependent.var.type = 'numerical'
hypothesis.type = 'group_difference'
repeat.var.value = 'Sangha_Mbaere'


debug(map_to_result)

groupes <- response %>% group_split(admin_1)
lapply(groupes, function(x){map_to_result(data = x, 
                                          dependent.var = dependent.var, 
                                          independent.var = independent.var, 
                                          case = case, 
                                          weighting = weighting, 
                                          questionnaire = questionnaire)})

