#%%
import sys
!{sys.executable} -m pip install jupyter pandas altair altair_saver numpy plotnine scikit-learn

import sys
!{sys.executable} -m pip install altair_saver

import sys
!{sys.executable} -m pip install pandas
import sys
!{sys.executable} -m pip install numpy pandas scikit-learn plotnine altair

#%%
import sys
!{sys.executable} -m pip install --upgrade pip

import sys
!{sys.executable}- m pip install radian

import pandas as pd
import numpy as np
import altair as alt
from plotnine import *
from plotnine.data import *
from plotnine import ggplot, geom_point, aes, stat_smooth, facet_wrap, geom_line

import pyarrow.feather as feather
import pandas as pd
pd.plotting.register_matplotlib_converters()
import matplotlib.pyplot as plt
%matplotlib inline
import numpy as np
#%%
import pandas as pd

gap = pd.read_csv (r'/Users/Zack/Downloads/gapminder.csv').assign(
    pop100k = lambda x : x['pop'] / 100000
)
print(gap)

#%%
p1_1_chart = (alt.Chart(gap)
  .encode(
    alt.X('lifeExp', title = "Life Expectancy", scale = alt.Scale(zero=False)),
    alt.Y('gdpPercap', title = "GDP per capita", scale = alt.Y(type='sqrt')),
    alt.Color('continent'),
    size = alt.Size("pop100k", title = 'Population (100k)'))
  .mark_circle()
  .properties(width = 125)
  .facet(column = 'year'
  )
)

chart = alt.chart(gap).<chart_methods>
chart.save('p1_1_chart.png')

#%%
  #Beginning 2nd Chart
def weighted_avg(gap, values, weights):
    d = gap[values]
    w = gap[weights]
    return (d * w).sum() / w.sum()

weighted = gap.groupby(['continent', 'year'],as_index = False)['pop'].sum()
weighted2 = gap.groupby(['continent', 'year'],as_index = False).apply(weighted_avg, 'gdpPercap', 'pop')
weighted2.columns = ['continent', 'year', 'gdpPercap']
weighted3 = weighted.merge(weighted2, how = 'inner')

# 2nd Graph
#%%
p1_2_chart = (ggplot(gap, aes(x = 'year', y = 'gdpPercap', color = 'continent', group = 'country')) 
+ geom_point() 
+ facet_grid('~continent') 
+ xlab("Year") 
+ ylab("GDP per capita") 
+ geom_line() 
+ theme_bw() 
+labs(size = "Population (100k)", color = "Continent"))
#%%
p1_2_chart2 = (p1_2_chart + geom_point(weighted3, aes(x = 'year', y = 'weighted_avg', 
            size = 'pop/100000', group = 'continent'), color = 'black') 
            + geom_line(weighted3, aes(x = 'year', y = 'weighted_avg', 
            group = 'continent'), color = "black"))
