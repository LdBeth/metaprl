%--------------------------------------------------------------------------
% File     : BOO008-3 : TPTP v2.1.0. Released v1.0.0.
% Domain   : Boolean Algebra
% Problem  : Sum is associative ( (X + Y) + Z = X + (Y + Z) )
% Version  : [MOW76] axioms : Reduced > Incomplete.
% English  : 

% Refs     : 
% Source   : [OTTER]
% Names    : bool_ass.in [OTTER]
%          : bool.in [OTTER]

% Status   : satisfiable
% Rating   : 0.33 v2.1.0, 0.00 v2.0.0
% Syntax   : Number of clauses    :   35 (   0 non-Horn;  16 unit;  20 RR)
%            Number of literals   :   73 (  23 equality)
%            Maximal clause size  :    5 (   2 average)
%            Number of predicates :    3 (   0 propositional; 2-3 arity)
%            Number of functors   :   12 (   9 constant; 0-2 arity)
%            Number of variables  :   90 (   0 singleton)
%            Maximal term depth   :    2 (   1 average)

% Comments : 
%--------------------------------------------------------------------------
%----Include Reflexivity, symmetry and transitivity, of equality 
include('Axioms/EQU001-0.ax').
%----Omit the Boolean algebra axioms, add the used ones manually 
% include('axioms/BOO002-0.ax').
%----Include boolean algebra equality axioms 
%include('Axioms/BOO002-0.eq').
%--------------------------------------------------------------------------
input_clause(sum_substitution1,axiom,
    [--equal(X,Y),
     --sum(X,W,Z),
     ++sum(Y,W,Z)]).

input_clause(sum_substitution2,axiom,
    [--equal(X,Y),
     --sum(W,X,Z),
     ++sum(W,Y,Z)]).

input_clause(sum_substitution3,axiom,
    [--equal(X,Y),
     --sum(W,Z,X),
     ++sum(W,Z,Y)]).

input_clause(addition_is_well_defined,axiom,
    [--sum(X,Y,U),
     --sum(X,Y,V),
     ++equal(U,V)]).

%input_clause(multiplication_is_well_defined,axiom,
%    [--product(X,Y,U),
%     --product(X,Y,V),
%     ++equal(U,V)]).

% input_clause(product_substitution1,axiom,
%     [--equal(X,Y),
%      --product(X,W,Z),
%      ++product(Y,W,Z)]).
% 
% input_clause(product_substitution2,axiom,
%     [--equal(X,Y),
%      --product(W,X,Z),
%      ++product(W,Y,Z)]).
% 
% input_clause(product_substitution3,axiom,
%     [--equal(X,Y),
%      --product(W,Z,X),
%      ++product(W,Z,Y)]).
% 
% input_clause(add_substitution1,axiom,
%     [--equal(X,Y),
%      ++equal(add(X,W),add(Y,W))]).
% 
% input_clause(add_substitution2,axiom,
%     [--equal(X,Y),
%      ++equal(add(W,X),add(W,Y))]).
% 
% input_clause(multiply_substitution1,axiom,
%     [--equal(X,Y),
%      ++equal(multiply(X,W),multiply(Y,W))]).
% 
% input_clause(multiply_substitution2,axiom,
%     [--equal(X,Y),
%      ++equal(multiply(W,X),multiply(W,Y))]).
% 
% input_clause(inverse_substitution,axiom,
%     [--equal(X,Y),
%      ++equal(inverse(X),inverse(Y))]).

%input_clause(closure_of_addition,axiom,
%    [++sum(X,Y,add(X,Y))]).

%input_clause(closure_of_multiplication,axiom,
%    [++product(X,Y,multiply(X,Y))]).

input_clause(commutativity_of_addition,axiom,
    [--sum(X,Y,Z),
     ++sum(Y,X,Z)]).

% input_clause(commutativity_of_multiplication,axiom,
%     [--product(X,Y,Z),
%      ++product(Y,X,Z)]).
% 
% input_clause(additive_identity1,axiom,
%     [++sum(additive_identity,X,X)]).
% 
% input_clause(additive_identity2,axiom,
%     [++sum(X,additive_identity,X)]).
% 
% input_clause(multiplicative_identity1,axiom,
%     [++sum(multiplicative_identity,X,X)]).
% 
% input_clause(multiplicative_identity2,axiom,
%     [++sum(X,multiplicative_identity,X)]).
% 
% input_clause(distributivity1,axiom,
%     [--product(X,Y,V1),
%      --product(X,Z,V2),
%      --sum(Y,Z,V3),
%      --product(X,V3,V4),
%      ++sum(V1,V2,V4)]).
% 
% input_clause(distributivity2,axiom,
%     [--product(X,Y,V1),
%      --product(X,Z,V2),
%      --sum(Y,Z,V3),
%      --sum(V1,V2,V4),
%      ++product(X,V3,V4)]).
% 
% % input_clause(distributivity3,axiom,
% %     [--product(Y,X,V1),
% %      --product(Z,X,V2),
% %      --sum(Y,Z,V3),
% %      --product(V3,X,V4),
% %      ++sum(V1,V2,V4)]).
% 
% % input_clause(distributivity4,axiom,
% %     [--product(Y,X,V1),
% %      --product(Z,X,V2),
% %      --sum(Y,Z,V3),
% %      --sum(V1,V2,V4),
% %      ++product(V3,X,V4)]).
% 
% input_clause(distributivity5,axiom,
%     [--sum(X,Y,V1),
%      --sum(X,Z,V2),
%      --product(Y,Z,V3),
%      --sum(X,V3,V4),
%      ++product(V1,V2,V4)]).
% 
% input_clause(distributivity6,axiom,
%     [--sum(X,Y,V1),
%      --sum(X,Z,V2),
%      --product(Y,Z,V3),
%      --product(V1,V2,V4),
%      ++sum(X,V3,V4)]).
% 
% % input_clause(distributivity7,axiom,
% %     [--sum(Y,X,V1),
% %      --sum(Z,X,V2),
% %      --product(Y,Z,V3),
% %      --sum(V3,X,V4),
% %      ++product(V1,V2,V4)]).
% 
% % input_clause(distributivity8,axiom,
% %     [--sum(Y,X,V1),
% %      --sum(Z,X,V2),
% %      --product(Y,Z,V3),
% %      --product(V1,V2,V4),
% %      ++sum(V3,X,V4)]).
% 
% input_clause(additive_inverse1,axiom,
%     [++sum(inverse(X),X,multiplicative_identity)]).
% 
% input_clause(additive_inverse2,axiom,
%     [++sum(X,inverse(X),multiplicative_identity)]).
% 
% input_clause(multiplicative_inverse1,axiom,
%     [++product(inverse(X),X,additive_identity)]).
% 
% input_clause(multiplicative_inverse2,axiom,
%     [++product(X,inverse(X),additive_identity)]).

input_clause(y_plus_z,hypothesis,
    [++sum(y,z,y_plus_z)]).

input_clause(x_plus__y_plus_z,hypothesis,
    [++sum(x,y_plus_z,x__plus_y_plus_z)]).

input_clause(x_plus_y,hypothesis,
    [++sum(x,y,x_plus_y)]).

input_clause(x_plus_y__plus_z,hypothesis,
    [++sum(x_plus_y,z,x_plus_y__plus_z)]).

input_clause(prove_equality,conjecture,
    [++equal(x__plus_y_plus_z,x_plus_y__plus_z)]).
%--------------------------------------------------------------------------
