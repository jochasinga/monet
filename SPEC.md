# Spec

The spec is in EBNF. This is rapidly evolving, so read it with that in mind.

```ebnf
op     ::=
        | 'Deposit' | 'Pay' | 'DealProposalCreated' | 'DealPublished'
        | 'DealActivated' | 'DealTerminated' | ...

event  ::= dealProposalCreated | dealPublished | dealActivated |
           dealTerminated

dealProposalCreated ::= 'DealProposalCreated'

keyword ::= 'When' | 'Close'
term    ::= value | keyword

binop ::= '*' | '/' | '+' | '-' | '++' | '%' | '||' | '&&'
uop ::= '!'
string-op ::= '++'


expr ::= '(' expr binop expr ')'
      | '(' uop expr ')'
	    | '(' term ')'
      | term

term ::= ['('] fixnum | decimal | string | bool [')']


string-expr ::= string-term '++' string-expr
string-term ::= string

fixnum   ::= (hexdigit|'_')+
          |  [('-'|'+')](digit|'_')+

decimal  ::= [('-'|'+')](digit|'_')+'.'(digit|'_')*
string   ::= '"' char* '"'
boolean  ::= ':t'|':f'
digit    ::= 0 | 1 | 2 | 3 | ... | 9
hexdigit ::= digit | a | b | c | d | e | f

expr ::= prim
      | token | constant-param | constant | address | mulvalue


param. ::= '(' keyword [value] ')'

address        ::= '(' 'Address' string ')'
token          ::= '(' 'Token' string string ')'
constant-param ::= '(' 'ConstantParam' string ')'
constant       ::= '(' 'Constant' (fixnum|decimal|string) ')'
mulvalue       ::= '(' 'MulValue' constant constant-param ')'


contract  ::= 'Close'
           | pay
           | if
           | when
           | let
           | assert

pay    ::= '(' 'Pay' payer payee token val contract ')'
if     ::= '(' 'If' event contract contract ')'
when   ::= '(' 'When' case+ [timeout] contract ')'
let    ::= '(' 'Let' id expr contract ')'
assert ::= '(' 'Assert' event contract ')'

cond         ::= 'When' case
case         ::= '[' 'Case' contract+ ']'

action  ::=
              | '(' op (param|action)* ['Close'] ')'
              | '(' cond ')'
```

