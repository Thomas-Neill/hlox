defns = '''(:LEFT_PAREN
):RIGHT_PAREN
{:LEFT_BRACE
}:RIGHT_BRACE
,:COMMA
.:DOT
-:MINUS
+:PLUS
;:SEMICOLON
!=:BANG_EQUAL
!:BANG
==:EQUAL_EQUAL
=:EQUAL
>=:GREATER_EQUAL
<=:LESS_EQUAL
'''
def haskFormat(token):
    if(len(token) == 1):
        return "'" + token + "'"
    else:
        return "'" + "':'".join(list(token)) + "'"
def link(symbol,token):
    print('tokenize ({}:xs) = tokenize xs >>= append {}'.format(haskFormat(symbol),token))
symbolTokenList = [i.split(':') for i in defns.split('\n')][:-1:]
list(map(lambda pair:link(pair[0],pair[1]),symbolTokenList))
for i in ['and','class','else','false','fun','for','if','nil','print','return','super','this','true','var','while']:
    print("getKeyword (IDENTIFIER \"{}\") = {}".format(i,i.upper()))
