%
% Genealogical problem from Nerode and Shore
%
input_clause(mother_daughter,axiom,
    [--daughter(Y,X),
     --female(X),
     ++mother(X,Y)]).

input_clause(mother_son,axiom,
    [--son(Y,X),
     --female(X),
     ++mother(X,Y)]).

input_clause(daughter_mother,axiom,
    [--mother(Y,X),
     --female(X),
     ++daughter(X,Y)]).

input_clause(son_mother,axiom,
    [--mother(Y,X),
     --male(X),
     ++son(X,Y)]).

input_clause(father_daughter,axiom,
    [--daughter(Y,X),
     --male(X),
     ++father(X,Y)]).

input_clause(father_son,axiom,
    [--son(Y,X),
     --male(X),
     ++father(X,Y)]).

input_clause(daughter_father,axiom,
    [--father(Y,X),
     --female(X),
     ++daughter(X,Y)]).

input_clause(son_father,axiom,
    [--father(Y,X),
     --male(X),
     ++son(X,Y)]).

input_clause(male_jim,hypothesis,
    [++male(jim)]).

input_clause(male_tim,hypothesis,
    [++male(tim)]).

input_clause(female_jane,hypothesis,
    [++female(jane)]).

input_clause(female_pam,hypothesis,
    [++female(pam)]).

input_clause(father_jim_tim,hypothesis,
    [++father(jim,tim)]).

input_clause(father_jim_pam,hypothesis,
    [++father(jim,pam)]).

input_clause(mother_jane_tim,hypothesis,
    [++mother(jane,tim)]).

input_clause(mother_jane_pam,hypothesis,
    [++mother(jane,pam)]).

input_clause(son_tim_jim,conjecture,
    [++son(tim,jim)]).