import matplotlib.pyplot as plt
import numpy as np
import itertools

import handler

class Profiler:

	def __init__(self):
		self.months = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
		self.years = ['2009','2010','2011','2012','2013','2014','2015','2016']
		self.parties = ['DEM','PCdoB','PDT','PEN','PHS','PMB','PMDB','PMN','PP','PPS','PR','PRB','PROS','PRP','PRTB','PSB','PSC','PSD','PSDB','PSDC','PSL','PSOL','PT','PTB','PTC','PTN','PTdoB','PV','REDE','S.PART.','SD']
		self.states = ['SP','PR','CE','BA','SC','RS','MG','RJ','RO','PB','MS','SE','MA','DF','PI','AL','PE','PA','RR','TO','AC','AP','GO','AM','ES','RN','MT']
		self.subquota_description = ['Maintenance of office supporting parliamentary activity','Fuels and lubricants','Consultancy, research and technical work','Publicity of parliamentary activity','Security service provided by specialized company','Flight tickets','Telecommunication','Postal services','Lodging, except for congressperson from Distrito Federal','Automotive vehicle renting or watercraft charter','Flight ticket issue','Locomotion, meal and lodging','Purchase of office supplies','Congressperson meal','Publication subscriptions','Automotive vehicle renting or charter','Taxi, toll and parking','Software purchase or renting; Postal services; Subscriptions','Aircraft renting or charter of aircraft','Terrestrial, maritime and fluvial tickets','Watercraft renting or charter','Participation in course, talk or similar event']
		self.markers = itertools.cycle(('o', 'v', '^', '<', '>', '8', 's', 'p', '*', 'h', 'H', 'D', 'd', 'P', 'X'))

	def plot_general(self, plot_type):
		if plot_type.lower() == 'state'.lower():
			collection = self.states
		elif plot_type.lower() == 'party'.lower():
			collection = self.parties
		elif plot_type.lower() == 'quota'.lower():
			collection = self.subquota_description
		else:
			raise PlotTypeError('The plot type {} is not supported!'.format(plot_type))

		x_ticks = []
		for year in self.years:
			for month in self.months:
				x_ticks.append(year+" "+month)

		x = list(range(1,97))
		ys = []
		plt.xticks(x,x_ticks)

		for item in collection:
			y = handler.fetch_expenses(plot_type,item)
			ys.append(y)
			plt.plot(x,y,label=item,marker=next(self.markers))
			print("Ploted graph for "+item+"...")

		arr = np.array(ys)
		avg = np.mean(arr, axis=0)

		plt.plot(x,avg,label="avg",linewidth=3,linestyle="--",c="red")
		plt.show()

	def plot_sum(self, trim=True):
		y = handler.fetch_sum(trim)
		x = list(range(0,len(y)))
		print(x)
		print(y)
		
		plt.plot(x,y)
		plt.show()
