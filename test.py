import pandas as pd
import re
import collections

# Token specification
INT = r'(?P<INT>\d+)'
COMMA = r'(?P<COMMA>\,)'
RBCLOSE = r'(?P<RBCLOSE>\})'
EQ = r'(?P<EQ>\=)'
LBCLOSE = r'(?P<LBCLOSE>\{)'
LPAREN = r'(?P<LPAREN>\[)'
RPAREN = r'(?P<RPAREN>\])'
WS = r'(?P<WS>\s+)'

master_pat = re.compile('|'.join([INT, RBCLOSE, COMMA,LBCLOSE, LPAREN, RPAREN, EQ,WS]))
# Tokenizer
Token = collections.namedtuple('Token', ['type', 'value'])


def generate_tokens(text):
    scanner = master_pat.scanner(text)
    for m in iter(scanner.match, None):
        tok = Token(m.lastgroup, m.group())
        if tok.type != 'WS':
            yield tok


# Parser


# def getlen(termval):
#     user = re.findall(r'\d+|\{[0-9]+|\,[0-9]+|\,\[\d+\]\=\d+', termval)
#     print(len(user))
#     return (len(user))


class ExpressionEvaluator:

    def parse(self, text):
       i=problem_test(text)
       if i==1:
        self.tokens = generate_tokens(text)
        self.tok = None  # Last symbol consumed
        self.nexttok = None  # Next symbol tokenized
        self._advance()  # Load first lookahead token
        return self.expr()

    def _advance(self):
        'Advance one token ahead'
        self.tok, self.nexttok = self.nexttok, next(self.tokens, None)

    def _accept(self, toktype):
        'Test and consume the next token if it matches toktype'
        if self.nexttok and self.nexttok.type == toktype:
            self._advance()
            return True
        else:
            return False

    def _expect(self, toktype):
        'Consume next token if it matches toktype or raise SyntaxError'
        if not self._accept(toktype):
            raise SyntaxError('Expected ' + toktype)
    def getlen(termval):
        user=re.findall(r'\d+|\{[0-9]+|\,[0-9]+|\,\[\d+\]\=\d+',termval)
        print(len(user))
        return (len(user))

    def expr(self):
        "expression ::= term { ('+'|'-') term }*"
        exprval = self.term()
        while self._accept('COMMA'):
            op = self.tok.type
            right = self.term()
            if op == 'COMMA':
                exprval =str(exprval)+','+str(right)

        return exprval

    def term(self, self_=None):
        "term ::= factor { ('*'|'/') factor }*"
        termval = self.factor()
        while self._accept('LPAREN'):
            op = self.factor()
            if self._accept('RPAREN') :
              N=self.getlen(termval)
              print(N)
              right = self.factor()
              termval=str(termval)+','+str(right)
            elif op == 'TIMES':
                termval *= right
        return termval

    def factor(self):
        "factor ::= NUM | ( expr )"
        if self._accept('INT'):
            return int(self.tok.value)
        elif self._accept('LBCLOSE'):
            exprval = self.expr()
            self._expect('RBCLOSE')
            return ('['+str(exprval)+']')
        else:
            raise SyntaxError('Expected NUMBER or LPAREN')
def problem_test(text):
    if re.findall(r'\}\{',text):
        raise SyntaxError("comma lost err")

    elif re.findall(r'\d\s\d',text):
        raise SyntaxError("comma lost err")

    elif re.findall(r'([A-Z]|[a-z])',text):
        raise SyntaxError("not a num err")

    elif re.findall(r'({.*,}})',text):
        raise SyntaxError("	too-many-braces err")

    else:
        return 1
def descent_parser():
    e = ExpressionEvaluator()
    i=input("input please")
    print(e.parse(i))



if __name__ == '__main__':
    descent_parser()
