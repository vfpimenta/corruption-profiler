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
            collection = self.subquota_description
        else:
            raise PlotTypeError('The plot option {} is not supported!'.format(opt))

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
                x = list(range(len(y)))
                plt.plot(x,y,label=item,marker=next(self.markers))
                print("Ploted graph for "+item+"...")

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
        print('Reading csv info...')
        congressman = self.csv_handler.get_congressman_info()
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
            data[key] = (info[0],info[1],data[key])
            util.printProgressBar(idx, len(data.keys()), prefix='Fetching info', suffix='Complete')

        self.json_handler.dump(data) 

    def read_congressman_json(self):
        if not os.path.exists('../data/congressman_ts.json'):
            self.build_congressman_json()  

        return self.json_handler.load()