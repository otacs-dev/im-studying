import plotly.express as px
from die import Die

die_1 = Die()
die_2 = Die(10)

results = []
for roll_num in range(50_000):
    result = die_1.roll() + die_2.roll()
    results.append(result)

title = "Results of Rolling a D6 and a D10 50,000 times"
labels = {'x': 'Result', 'y': 'Frequency of Result'}
