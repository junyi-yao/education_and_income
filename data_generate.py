# -*- coding: utf-8 -*-
"""
Created on Mon Dec 13 02:01:47 2021

@author: Rubyxu
"""

import pandas as pd

data = pd.read_csv('temp.csv')
#data_f = data[data.Education]


data_new = data.copy()
data_new = data_new[data_new.Gender=='Male'].reset_index()
data_new['All Gender']=0
for year in range(2008,2015):
    data_year = data[data.Year==year]
    data_year['All Gender'] = 0
    data_year_m = data_year[data_year.Gender=='Male'].reset_index()
    data_year_f = data_year[data_year.Gender=='Female'].reset_index()

    for i in range(len(data_year_m)): 
        ind = (year-2008)*len(data_year_m)
        data_new['All Gender'][i+ind] = data_year_m['Population'][i]+data_year_f['Population'][i]
    
    data_new = data_new[['Year', 'Education', 'Income','All Gender']]

data_new['ratio'] = 0
Income = ['No Income','[5k,10k)','[10k,15k)','[15k,25k)','[25k,35k)','[35k,50k)','[50k,75k)','[75k,inf)']

for year in range(2008,2015):
    temp_year =  data_new[data_new['Year']==year]
    for income in Income:
        temp = temp_year[temp_year['Income']==income]
        ratio =  temp['All Gender']/sum(temp['All Gender'])
        for index in temp['All Gender'].index:
            data_new.iloc[index,-1] =  ratio[index]
            

output = []


for i in range(len(data_new)):
    data_one = data_new.iloc[i]
    output_one = {}
    for j in range(len(data_one)):
        key = data_one.index[j]
        value = data_one[j]
        output_one[key] = value
    output.append(output_one)
    
with open('data.txt','w') as f:
    f.write(str(output))









