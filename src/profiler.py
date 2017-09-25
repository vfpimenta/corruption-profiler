from collections import OrderedDict
from iohandler import SQLiteHandler, CSVHandler
import matplotlib.pyplot as plt
import numpy as np
import os.path
import itertools
import util
import json

class Profiler:

    def __init__(self):
        self.months = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
        self.years = ['2009','2010','2011','2012','2013','2014','2015','2016']
        self.parties = ['DEM','PCdoB','PDT','PEN','PHS','PMB','PMDB','PMN','PP','PPS','PR','PRB','PROS','PRP','PRTB','PSB','PSC','PSD','PSDB','PSDC','PSL','PSOL','PT','PTB','PTC','PTN','PTdoB','PV','REDE','S.PART.','SD']
        self.states = ['SP','PR','CE','BA','SC','RS','MG','RJ','RO','PB','MS','SE','MA','DF','PI','AL','PE','PA','RR','TO','AC','AP','GO','AM','ES','RN','MT']
        self.subquota_description = ['Maintenance of office supporting parliamentary activity','Fuels and lubricants','Consultancy, research and technical work','Publicity of parliamentary activity','Security service provided by specialized company','Flight tickets','Telecommunication','Postal services','Lodging, except for congressperson from Distrito Federal','Automotive vehicle renting or watercraft charter','Flight ticket issue','Locomotion, meal and lodging','Purchase of office supplies','Congressperson meal','Publication subscriptions','Automotive vehicle renting or charter','Taxi, toll and parking','Software purchase or renting; Postal services; Subscriptions','Aircraft renting or charter of aircraft','Terrestrial, maritime and fluvial tickets','Watercraft renting or charter','Participation in course, talk or similar event']
        self.markers = itertools.cycle(('o', 'v', '^', '<', '>', '8', 's', 'p', '*', 'h', 'H', 'D', 'd', 'P', 'X'))

        self.sqlh = SQLiteHandler()
        self.csvh = CSVHandler()

    def get_general(self, opt, trim=True, plot=False):
        if opt.lower() == 'state'.lower():
            collection = self.states
        elif opt.lower() == 'party'.lower():
            collection = self.parties
        elif opt.lower() == 'quota'.lower():
            collection = self.subquota_description
        else:
            raise PlotTypeError('The plot option {} is not supported!'.format(opt))

        x_ticks = []
        for year in self.years:
            for month in self.months:
                x_ticks.append(year+" "+month)

        x = list(range(1,97))
        ys = []
        y_ts = {}
        plt.xticks(x,x_ticks)

        for item in collection:
            y = self.sqlh.fetch_expenses(opt,item,trim)
            ys.append(y)
            y_ts[item] = y
            if plot:
                plt.plot(x,y,label=item,marker=next(self.markers))
                print("Ploted graph for "+item+"...")

        arr = np.array(ys)
        avg = np.mean(arr, axis=0)

        if plot:
            plt.plot(x,avg,label="avg",linewidth=3,linestyle="--",c="red")
            plt.show()

        return y_ts

    def get_sum(self, trim=True, plot=False):
        y = self.sqlh.fetch_sum(trim)
        x = list(range(0,len(y)))

        if plot:
            print(x)
            print(y)
            
            plt.plot(x,y)
            plt.show()

        return y

    def get_congressman_ts(self, trim=True):
        congressman_ts = OrderedDict()
        print('Reading csv info...')
        congressman = self.csvh.get_congressman_info()
        idx = 0
        for _id in congressman:
            idx += 1
            congressman_ts[_id] = self.sqlh.fetch_expenses('congressperson_id',_id, trim)
            util.printProgressBar(idx, len(congressman), prefix='Fetching data', suffix='Complete')
        return congressman_ts

    def build_congressman_json(self):
        data = self.get_congressman_ts()
        idx = 0
        for key in data.keys():
            idx += 1
            info = self.sqlh.fetch_congressman_info(key)
            data[key] = (info[0],info[1],data[key])
            util.printProgressBar(idx, len(data.keys()), prefix='Fetching info', suffix='Complete')
        
        print('Dumping data to json...')
        with open('../data/congressman_ts.json','w') as jsonfile:
            json.dump(data, jsonfile)

    def read_congressman_json(self):
        if not os.path.exists('../data/congressman_ts.json'):
            self.build_congressman_json()

        with open('../data/congressman_ts.json') as jsonfile:    
            data = json.load(jsonfile)

        return data