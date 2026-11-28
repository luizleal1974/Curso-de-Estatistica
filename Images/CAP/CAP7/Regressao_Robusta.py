from os import chdir
from platform import system
from pandas import read_excel, DataFrame, concat
import numpy as np
from sklearn.linear_model import (
     LinearRegression, 
     HuberRegressor,
     RANSACRegressor,
     TheilSenRegressor
)
import plotly.graph_objects as go

# Dados
if system() == 'Windows': chdir('F:/Statistics/Apostila/eBook/Dados/Cap_07')
if system() == 'Linux': chdir('/media/luiz/KINGSTON/Statistics/Apostila/eBook/Dados/Cap_07')
dados = read_excel("04_Dados.xlsx")

# Variáveis
x = np.array(dados.x.to_list()).reshape((-1, 1))
y = np.array(dados.y.to_list())

# Modelos
LM = LinearRegression().fit(x, y)        # OLS
Huber = HuberRegressor().fit(x, y)       # Regressão robusta
RANSAC = RANSACRegressor().fit(x, y)     # Regressão robusta: Random Sample Consensus (RANSAC)
TheilSen = TheilSenRegressor().fit(x, y) # Regressão robusta

# Coeficientes estimados
cf = DataFrame({'LM': [LM.intercept_ , LM.coef_] ,
                'Huber': [Huber.intercept_ , Huber.coef_] ,
                'RANSAC': [RANSAC.estimator_.intercept_ , RANSAC.estimator_.coef_] ,
                'TheilSen': [TheilSen.intercept_ , TheilSen.coef_] ,
              })
cf.index = ['b_0', 'b_1']

# Mean Absolute Error (MAE)
MAE = DataFrame({'LM':       [sum(abs( y - LM.predict(x)       )    ) / len(y)],
                 'Huber':    [sum(abs( y - Huber.predict(x)    )    ) / len(y)],
                 'RANSAC':   [sum(abs( y - RANSAC.predict(x)   )    ) / len(y)],
                 'TheilSen': [sum(abs( y - TheilSen.predict(x) )    ) / len(y)]
               })
MAE.index = ['MAE']

# Mean Absolute Percentage Error (MAE)
MAPE = DataFrame({'LM':       [sum(abs( (y - LM.predict(x)       )  / y )    ) / len(y)],
                  'Huber':    [sum(abs( (y - Huber.predict(x)    )  / y )    ) / len(y)],
                  'RANSAC':   [sum(abs( (y - RANSAC.predict(x)   )  / y )    ) / len(y)],
                  'TheilSen': [sum(abs( (y - TheilSen.predict(x) )  / y )    ) / len(y)]
                })
MAPE.index = ['MAPE']

# Métricas
concat([MAE, MAPE], axis = 0).transpose()

# Scatter plot
fig = go.Figure()

fig.add_trace(go.Scatter(x = dados.x,
                         y = dados.y,
                         name = '',
                         mode = "markers",
                         marker_color = '#000000',
                         marker_size = 15,
                         showlegend = False)
                         )
                         
fig.add_trace(go.Scatter(x = dados.x,
                         y = LM.predict(x),
                         name = 'LM',
                         mode = "lines",
                         line = dict(color = '#ff3266', width = 5)
                         ))

fig.add_trace(go.Scatter(x = dados.x,
                         y = Huber.predict(x),
                         name = 'Huber',
                         mode = "lines",
                         line = dict(color = '#77a9d7', width = 5)
                         ))

fig.add_trace(go.Scatter(x = dados.x,
                         y = RANSAC.predict(x),
                         name = 'RANSAC',
                         mode = "lines",
                         line = dict(color = '#a7dd9c', width = 5)
                         ))

fig.add_trace(go.Scatter(x = dados.x,
                         y = TheilSen.predict(x),
                         name = 'TheilSen',
                         mode = "lines",
                         line = dict(color = '#ccad51', width = 5)
                         ))
                         
fig.update_layout(title = dict(text = 'Scatter plot', x = 0.5, font = dict(size = 44, color = '#000000')),
                  legend = dict(title = dict(text = '<b>Modelo</b>', font = dict(size = 38, color = '#000000')), font = dict(size = 38, color = '#000000')),
                  showlegend = True
                  )
                  
fig.update_xaxes(title_text = 'x', title_font = dict(size = 44, color = '#000000'), tickfont = dict(size = 44, color = '#000000'))
fig.update_yaxes(title_text = 'y', title_font = dict(size = 44, color = '#000000'), tickfont = dict(size = 44, color = '#000000'))
fig.show()



