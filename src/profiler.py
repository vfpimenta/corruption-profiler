from collections import OrderedDict
from io_handler import SQLiteHandler, CSVHandler, JsonHandler
import matplotlib.pyplot as plt
import matplotlib.lines as mlines
import numpy as np
import os.path
import itertools
import calendar
import util

class Profiler:

    def __init__(self):
        self.sql_handler = SQLiteHandler()
        self.csv_handler = CSVHandler()
        self.json_handler = JsonHandler()

        self.months = map(lambda month: month[0:3],calendar.month_name[1:])
        self.years = self.sql_handler.get_field('year')
        self.parties = self.sql_handler.get_field('party')
        self.states = self.sql_handler.get_field('state')
        self.subquota_description = self.sql_handler.get_field('subquota_description')
        self.markers = itertools.cycle(tuple(mlines.Line2D.markers.keys()))

    def get_general(self, opt, trim=True, plot=False):
        if opt.lower() == 'state'.lower():
            collection = self.states
        elif opt.lower() == 'party'.lower():
            collection = self.parties
        elif opt.lower() == 'quota'.lower():
            opt = 'subquota_description'
            collection = self.subquota_description
        else:
            raise Exception('The plot option {} is not supported!'.format(opt))

        # x_ticks = []
        # for year in str(self.years):
        #     for month in self.months:
        #         x_ticks.append(year+" "+month)

        # x = list(range(1,97))
        ys = []
        y_ts = {}
        # plt.xticks(x,x_ticks)

        for item in collection:
            y = self.sql_handler.fetch_expenses(opt,item,trim)
            ys.append(y)
            y_ts[item] = y
            if plot:
                print("Plotting graph for "+item+"...",end='\r')
                x = list(range(len(y)))
                plt.plot(x,y,label=item,marker=next(self.markers))
                print("Plotting graph for "+item+"... Done")

        arr = np.array(ys)
        avg = np.mean(arr, axis=0)

        if plot:
            plt.legend(collection)
            plt.plot(x,avg,label="avg",linewidth=3,linestyle="--",c="red")
            plt.show()

        return y_ts

    def get_sum(self, trim=True, plot=False):
        y = self.sql_handler.fetch_sum(trim)
        x = list(range(0,len(y)))

        if plot:
            print(x)
            print(y)
            
            plt.plot(x,y)
            plt.show()

        return y

    def get_congressman_ts(self, trim=True):
        congressman_ts = OrderedDict()
        print('Reading csv info...',end='\r')
        congressman = self.csv_handler.get_congressman_info()
        print('Reading csv info... Done')

        idx = 0
        for _id in congressman:
            idx += 1
            congressman_ts[_id] = self.sql_handler.fetch_expenses('congressperson_id',_id, trim)
            util.printProgressBar(idx, len(congressman), prefix='Fetching data', suffix='Complete')
        return congressman_ts

    def build_congressman_json(self):
        data = self.get_congressman_ts()
        idx = 0
        for key in data.keys():
            idx += 1
            info = self.sql_handler.fetch_congressman_info(key)
            data[key] = (
                info[0], # Name
                info[1], # State
                info[2], # party
                (info[3],info[4],info[5]), # Legislature (53,54,55)
                data[key] # Time series
            )
            util.printProgressBar(idx, len(data.keys()), prefix='Fetching info', suffix='Complete')

        self.json_handler.dump(data) 

    def read_congressman_json(self, legislature):
        if not os.path.exists('../data/congressman_ts.json'):
            print('Json cache not found! Building cache...')
            self.build_congressman_json()
        else:
            print('Json cache found.')

        data = self.json_handler.load()
        
        filtered_data = {}
        for _id in data.keys():
            if(data[_id][3][legislature-53]):
                filtered_data[_id] = data[_id]
                begin, end = __get_idx__(legislature)
                filtered_data[_id][4] = filtered_data[_id][4][begin:end]

        return filtered_data

def __get_idx__(legislature):
    if legislature == 53:
        return 0, 22
    elif legislature == 54:
        return 22, 70
    elif legislature == 55:
        return 70, 89