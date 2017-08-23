import sqlite3
import time

def timing(f):
    def wrap(*args):
        time1 = time.time()
        ret = f(*args)
        time2 = time.time()
        print('{} function took {} ms'.format(f.__name__, (time2-time1)*1000.0))
        return ret
    return wrap

#@timing
def fetch_expenses(field, value):
	con = sqlite3.Connection('../data/local.db')
	cur = con.cursor()
	tuples = []

	for val in cur.execute('SELECT c.year, c.month, COALESCE(py.net,0) FROM calendar c LEFT JOIN (SELECT year,month,SUM(net_value) as net FROM previous_years WHERE {} = ? GROUP BY year, month) py ON py.year = c.year AND py.month = c.month'.format(field),[value]):
		tuples.append(val)

	values = [i[2] for i in tuples]

	cur.close()
	con.close()
	return values

def fetch_sum(trim):
	con = sqlite3.Connection('../data/local.db')
	cur = con.cursor()
	tuples = []

	sql = 'SELECT c.year, c.month, COALESCE(py.net,0) FROM calendar c LEFT JOIN (SELECT year,month,SUM(net_value) as net FROM previous_years GROUP BY year, month) py ON py.year = c.year AND py.month = c.month'
	if trim:
		# jul 2009 ~ ago 2016
		sql += ' WHERE (c.year = ? AND c.month >= ?) OR (c.year = ? AND c.month <= ?) OR (c.year >= ? AND c.year <= ?)'
		for val in cur.execute(sql,[2009,7,2016,8,2010,2015]):
			tuples.append(val)
	else:
		for val in cur.execute(sql):
			tuples.append(val)

	values = [i[2] for i in tuples]

	cur.close()
	con.close()
	return values