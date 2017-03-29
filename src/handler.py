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

@timing
def fetch_congressman_expenses(congressperson_id):
	con = sqlite3.Connection('local.db')
	cur = con.cursor()
	values = []

	for val in cur.execute('SELECT SUM(net_value) FROM previous_years WHERE congressperson_id = ? GROUP BY year, month',[congressperson_id]):
		tuples.append(val)
	
	values = [i[0] for i in tuples]
	values.append(0)

	cur.close()
	con.close()
	return values

@timing
def fetch_party_expenses(party):
	con = sqlite3.Connection('local.db')
	cur = con.cursor()
	values = []

	for val in cur.execute('SELECT SUM(net_value) FROM previous_years WHERE party = ? GROUP BY year, month',[party]):
		tuples.append(val)
	
	values = [i[0] for i in tuples]
	values.append(0)

	cur.close()
	con.close()
	return values

@timing
def fetch_state_expenses(state):
	con = sqlite3.Connection('local.db')
	cur = con.cursor()
	tuples = []

	for val in cur.execute('SELECT SUM(net_value) FROM previous_years WHERE state = ? GROUP BY year, month',[state]):
		tuples.append(val)
	
	values = [i[0] for i in tuples]
	values.append(0)

	cur.close()
	con.close()
	return values

# =================
#     OLD MODEL
# =================

@timing
def old_fetch_state_expenses(state):
	con = sqlite3.Connection('local.db')
	cur = con.cursor()
	values = []

	for year in range (2009,2017):
		for month in range(1,13):
			cur.execute('SELECT SUM(net_value) FROM previous_years WHERE state = ? AND month = ? AND year = ?',[state,month,year])
			value = cur.fetchall()[0][0]
			if value is None:
				value = 0
			values.append(value)

	cur.close()
	con.close()
	return values
