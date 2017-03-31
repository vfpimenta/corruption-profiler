import matplotlib.pyplot as plt
import numpy as np
import itertools

import handler

months = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
years = ['2009','2010','2011','2012','2013','2014','2015','2016']
parties = ['DEM','PCdoB','PDT','PEN','PHS','PMB','PMDB','PMN','PP','PPS','PR','PRB','PROS','PRP','PRTB','PSB','PSC','PSD','PSDB','PSDC','PSL','PSOL','PT','PTB','PTC','PTN','PTdoB','PV','REDE','S.PART.','SD']
states = ['SP','PR','CE','BA','SC','RS','MG','RJ','RO','PB','MS','SE','MA','DF','PI','AL','PE','PA','RR','TO','AC','AP','GO','AM','ES','RN','MT']
markers = itertools.cycle(('o', 'v', '^', '<', '>', '8', 's', 'p', '*', 'h', 'H', 'D', 'd', 'P', 'X'))

x_ticks = []
for year in years:
	for month in months:
		x_ticks.append(year+" "+month)

x = list(range(1,97))
ys = []
plt.xticks(x,x_ticks)

for state in states:
	y = handler.fetch_expenses('state',state)
	ys.append(y)
	plt.plot(x,y,label=state,marker=next(markers))
	print("Ploted graph for "+state+"...")

arr = np.array(ys)
avg = np.mean(arr, axis=0)

plt.plot(x,avg,label="avg",linewidth=3,linestyle="--",c="red")

plt.show()
