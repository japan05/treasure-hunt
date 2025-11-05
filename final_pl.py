from pyswip import Prolog

prolog = Prolog()
prolog.consult("final_w.pl")

result = list(prolog.query("run(Step,Score)"))
if result:
    path = result[0]['Step']
    print("Step:", path)
    path = result[0]['Score']
    print("Score:", path)

