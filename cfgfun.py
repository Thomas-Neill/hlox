import random
loxcfg ='''expression:literal|unary|binary|grouping
literal:"NUMBER"|"STRING"|"true"|"false"|"nil"
grouping:"("+expression+")"
unary:modifier+expression
modifier:"-"|"!"
binary:expression+operator+expression
operator:"=="|"!="|"<"|"<="|">"|">="|"+"|"-"|"*"|"/"'''
loxcfg = loxcfg.split('\n')
loxcfg = [i.split(':') for i in loxcfg]
loxcfg = [tuple([i[0],i[1].split('|')]) for i in loxcfg]
loxcfg = dict(loxcfg)
def evaluate(group):
#    print(group)
    try:
        strLit = eval(group)
        if(strLit == 'NUMBER'):
            return random.randint(1,10)
        elif(strLit == 'STRING'):
            return '"'+''.join([chr(random.randint(65,90)) for i in range(random.randint(1,10))])+'"'
        else:
            return strLit
    except:
        try:
            option = random.choice(loxcfg[group])
            return(evaluate(option))
        except:
            split = group.split('+')
            return ''.join(map(evaluate,split))
print(evaluate('expression'))        
