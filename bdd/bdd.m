%------------------------------------------------------------------------------%
%
% Module: bdd
% Copyright (c) Mission Critical IT
% MIT License
% Author: Ludovic Langevine <llg@missioncriticalit.com>
%
% Simple representation and implementation of binary decision diagrams (BDD)
%
% This module exports an abstract bdd(V) type representing a binary decision
% diagram with boolean variable of type V.
%
% It implements:
%   - building of a BDD from a boolean expression, using recursive Shannon expansion.
%   - composition of two BDDs using AND/OR/XOR
%   - application of the negation operator NOT
%   - evaluation of a BDD given a valuation of the variabls (a function V -> bool)
%
% The unification can be used to check that 2 BDDs are equivalent.
%
% A BDD is a directed acyclic graph where:
%   - there are at most 2 leafs, "true" and "false"
%   - each internal node is labeled by a boolean variable and has 2 outgoing
%     arcs ("high" and "low"). Internal nodes are called "if then else" nodes
%
% Here the BDDs are ordered (there is a total order on variables, which is the
% default Mercury ordering) and reduced (an internal nodes has two distinct
% sub-graphs). They are called "ROBDDs".
%
% Some nice properties about ROBDDs:
%   - two equivalent boolean expressions have the same ROBDDs
%   - the BDD is reduced to a leaf when the boolean expression is (un)satisfiable.
%   - the boolean operators not/and/or/xor on BDDs are efficient
%
%------------------------------------------------------------------------------%

:- module bdd.

:- interface.

:- import_module bool.
:- import_module set.

:- import_module boolean_expression.

:- type bdd(BooleanVar).

:- func bdd(boolean_expression(BooleanVar)) = bdd(BooleanVar).

:- func bdd(BooleanVar) `and` bdd(BooleanVar) = bdd(BooleanVar).
:- func bdd(BooleanVar) `or` bdd(BooleanVar) = bdd(BooleanVar).
:- func bdd(BooleanVar) `xor` bdd(BooleanVar) = bdd(BooleanVar).
:- func not(bdd(BooleanVar)) = (bdd(BooleanVar)).

:- pred (bdd(BooleanVar)::in) `entails` (bdd(BooleanVar)::in) is semidet.

:- pred is_satisfiable(bdd(BooleanVar)::in) is semidet.

:- pred is_unsatisfiable(bdd(BooleanVar)::in) is semidet.

    %
    % From a set of boolean variables {V1, ..., Vn}, build the BDD equivalent to:
    % \/_i=1^n (
    %           V_i /\ not(\/j=1^n,j\=i V_j)
    %          )
    % Informally: exactly one of the V_i is true.
    %
:- func exactly_one(set(Var)) = bdd(Var).

:- func all_false(set(Var)) = bdd(Var).

:- func apply_and(bdd(BooleanVar), bdd(BooleanVar)) = bdd(BooleanVar).
:- func apply_or(bdd(BooleanVar), bdd(BooleanVar)) = bdd(BooleanVar).
:- func apply_xor(bdd(BooleanVar), bdd(BooleanVar)) = bdd(BooleanVar).

:- func evaluate(valuation(BooleanVar), bdd(BooleanVar)) = bool.
:- pred evaluate(valuation_pred(BooleanVar), bdd(BooleanVar)).
:- mode evaluate(in(valuation_pred), in) is semidet.

:- func bdd_from_positive_and_negative_atoms(set(Var), set(Var)) = bdd(Var).

:- func get_vars(bdd(Var)) = set(Var).

:- func bdd_to_dnf(bdd(Var)::in) = (boolean_expression(Var)::out(boolean_expression.dnf)) is det.

    %
    % Given a BDD, returns:
    %  - Entailed, the set of entailed variables
    %  - DisEntailed, the set of disentailed variables
    %  - Possibly, the set of variables which may be either true or false across the models
:- pred decompose_bdd(bdd(Var)::in, set(Var)::out, set(Var)::out, set(Var)::out) is det.

:- func partial_evaluation(bool, Var, bdd(Var)) = bdd(Var).

:- func bdd_to_str(func(Var) = string, bdd(Var)) = string.

:- implementation.

:- import_module list.
:- import_module map.
:- import_module require.
:- import_module solutions.
:- import_module string.

exactly_one(Vars) = exactly_one_aux(to_sorted_list(Vars)).

    %
    % Assume that the list of variables is sorted!
    % Once a true variable occurs, all the subsequent variables have to be valued to false
    % If no more variables are left, then the evaluation fails.
    %
:- func exactly_one_aux(list(V)) = bdd(V).

exactly_one_aux([]) = false_leaf.
exactly_one_aux([V|Vs]) = if_then_else(V, all_false_aux(Vs), exactly_one_aux(Vs)).

all_false(Vars) = all_false_aux(to_sorted_list(Vars)).

:- func all_false_aux(list(Var)) = bdd(Var).

all_false_aux([]) = true_leaf.
all_false_aux([V|Vs]) = if_then_else(V, false_leaf, all_false_aux(Vs)).

apply_and(BDD1, BDD2) = BDD1 `and` BDD2.
apply_or(BDD1, BDD2) = BDD1 `or` BDD2.
apply_xor(BDD1, BDD2) = BDD1 `xor` BDD2.

:- type bdd(BooleanVar)
    ---> if_then_else(
             b_var  :: BooleanVar,
             b_high :: bdd(BooleanVar),
             b_low  :: bdd(BooleanVar)
            )
    ;    true_leaf
    ;    false_leaf
    .

bdd(BooleanExpression) = BDD :-
    ( BooleanExpression = false ->
        BDD = false_leaf
    ; BooleanExpression = true ->
        BDD = true_leaf
    ;
        %
        % The expression is complex: use Shannon expansion
        %
        simplify_expression_and_get_vars(BooleanExpression, SimplifiedExpression, OccuringVariables),
        BDD = recursive_shannon_expansion(set.to_sorted_list(OccuringVariables), SimplifiedExpression)
    ).

    %
    % Optmized 'apply' for AND
    %
true_leaf `and` BDD2 = BDD2.
false_leaf `and` _ = false_leaf.
if_then_else(_, _, _) @ BDD1 `and` true_leaf = BDD1.
if_then_else(_, _, _) `and` false_leaf = false_leaf.
if_then_else(_, _, _) @ BDD1 `and` if_then_else(_, _, _) @ BDD2 = BDD1_And_BDD2 :-
    compare(CompareRes, BDD1 ^ b_var, BDD2 ^ b_var),
    ( CompareRes = (=),
        Var = BDD1 ^ b_var,
        High = BDD1 ^ b_high `and` BDD2 ^ b_high,
        Low = BDD1 ^ b_low `and` BDD2 ^ b_low
    ; CompareRes = (<),
        Var = BDD1 ^ b_var,
        High = BDD1 ^ b_high `and` BDD2,
        Low = BDD1 ^ b_low `and` BDD2
    ; CompareRes = (>),
        Var = BDD2 ^ b_var,
        High = BDD2 ^ b_high `and` BDD1,
        Low = BDD2 ^ b_low `and` BDD1
    ),

    ( High = Low ->
        %
        % The variable has no effect on the result: ignore it
        %
        BDD1_And_BDD2 = High
    ;
        BDD1_And_BDD2 = if_then_else(Var, High, Low)
    ).

    %
    % Optmized 'apply' for OR
    %
true_leaf `or` _ = true_leaf.
false_leaf `or` BDD2 = BDD2.
if_then_else(_, _, _) `or` true_leaf = true_leaf.
if_then_else(_, _, _) @ BDD1 `or` false_leaf = BDD1.
if_then_else(_, _, _) @ BDD1 `or` if_then_else(_, _, _) @ BDD2 = BDD1_Or_BDD2 :-
    compare(CompareRes, BDD1 ^ b_var, BDD2 ^ b_var),
    ( CompareRes = (=),
        Var = BDD1 ^ b_var,
        High = BDD1 ^ b_high `or` BDD2 ^ b_high,
        Low = BDD1 ^ b_low `or` BDD2 ^ b_low
    ; CompareRes = (<),
        Var = BDD1 ^ b_var,
        High = BDD1 ^ b_high `or` BDD2,
        Low = BDD1 ^ b_low `or` BDD2
    ; CompareRes = (>),
        Var = BDD2 ^ b_var,
        High = BDD2 ^ b_high `or` BDD1,
        Low = BDD2 ^ b_low `or` BDD1
    ),

    ( High = Low ->
        %
        % The variable has no effect on the result: ignore it
        %
        BDD1_Or_BDD2 = High
    ;
        BDD1_Or_BDD2 = if_then_else(Var, High, Low)
    ).

    %
    % Optmized 'apply' for XOR
    %
true_leaf `xor` BDD2 = not(BDD2).
false_leaf `xor` BDD2 = BDD2.
if_then_else(_, _, _) @ BDD1 `xor` true_leaf = not(BDD1).
if_then_else(_, _, _) @ BDD1 `xor` false_leaf = BDD1.
if_then_else(_, _, _) @ BDD1 `xor` if_then_else(_, _, _) @ BDD2 = BDD1_Xor_BDD2 :-
    compare(CompareRes, BDD1 ^ b_var, BDD2 ^ b_var),
    ( CompareRes = (=),
        Var = BDD1 ^ b_var,
        High = BDD1 ^ b_high `xor` BDD2 ^ b_high,
        Low = BDD1 ^ b_low `xor` BDD2 ^ b_low
    ; CompareRes = (<),
        Var = BDD1 ^ b_var,
        High = BDD1 ^ b_high `xor` BDD2,
        Low = BDD1 ^ b_low `xor` BDD2
    ; CompareRes = (>),
        Var = BDD2 ^ b_var,
        High = BDD2 ^ b_high `xor` BDD1,
        Low = BDD2 ^ b_low `xor` BDD1
    ),

    ( High = Low ->
        %
        % The variable has no effect on the result: ignore it
        %
        BDD1_Xor_BDD2 = High
    ;
        BDD1_Xor_BDD2 = if_then_else(Var, High, Low)
    ).

not(true_leaf) = false_leaf.
not(false_leaf) = true_leaf.
not(if_then_else(Var, High, Low)) = if_then_else(Var, not(High), not(Low)).

BDD1 `entails` BDD2 :-
    entailment_test(BDD1, BDD2, yes).

:- pred entailment_test(bdd(Atom)::in, bdd(Atom)::in, bool::out) is det.

entailment_test(false_leaf, _, yes).
entailment_test(true_leaf, HasToBeTrue, (if HasToBeTrue = true_leaf then yes else no)).
entailment_test(if_then_else(_, _, _), true_leaf, yes).
entailment_test(if_then_else(_, _, _), false_leaf, no).
entailment_test(if_then_else(Var1, High1, Low1) @ BDD1, if_then_else(Var2, High2, Low2) @ BDD2, Res) :-
    ( BDD1 = BDD2 ->
        Res = yes
    ;
        compare(CompareRes, Var1, Var2),
        ( CompareRes = (=),
            entailment_test(High1, High2, HighRes),
            ( HighRes = yes,
                entailment_test(Low1, Low2, Res)
            ; HighRes = no,
                Res = no
            )
        ; CompareRes = (>),
            Res = no
        ; CompareRes = (<),
            entailment_test(High1, BDD2, ResHigh),
            ( ResHigh = yes,
                entailment_test(Low1, BDD2, Res)
            ; ResHigh = no,
                Res = no
            )
        )
    ).

is_satisfiable(true_leaf).
is_satisfiable(if_then_else(_, _, _)).

is_unsatisfiable(false_leaf).

:- func recursive_shannon_expansion(list(Var), boolean_expression(Var)) = bdd(Var).
:- mode recursive_shannon_expansion(in, in(simplified_expression)) = out is det.

recursive_shannon_expansion([], Expr) = Leaf :-
    ( Expr = true ->
        Leaf = true_leaf
    ; Expr = false ->
        Leaf = false_leaf
    ;
        error($module ++ "." ++ $pred ++ ": unexpected expression " ++ string(Expr)
            ++ " for empty variable list. Should contain: " ++ string(get_vars(Expr)))
    ).
recursive_shannon_expansion([FirstVar|OtherVars] @ Vars, Expr) = BDD :-
    ( Expr = var(V) ->
        ( list.member(V, Vars) ->
            BDD = if_then_else(V, true_leaf, false_leaf)
        ;
            error($module ++ "." ++ $pred ++ ": unexpected variable " ++ string(V)
            ++ " for variable list " ++ string(Vars))
        )
    ;
        substitute(FirstVar, yes, Expr, ExprHigh),
        substitute(FirstVar, no, Expr, ExprLow),

        HighBranch = recursive_shannon_expansion(OtherVars, ExprHigh),
        LowBranch = recursive_shannon_expansion(OtherVars, ExprLow),
            %
            % When the two sub-BDDs are equal, the variable under examination can be ignored
            %
       ( HighBranch = LowBranch ->
           BDD = recursive_shannon_expansion(OtherVars, ExprHigh)
       ;
           BDD = if_then_else(FirstVar, HighBranch, LowBranch)
       )
    ).

evaluate(_, true_leaf) = yes.
evaluate(_, false_leaf) = no.
evaluate(Valuation, if_then_else(Variable, High, Low)) = evaluate(Valuation, High_or_Low) :-
    map.lookup(Valuation, Variable, Value),
    ( Value = yes,
        High_or_Low = High
    ; Value = no,
        High_or_Low = Low
    ).

evaluate(_, true_leaf).
evaluate(_, false_leaf) :-
    fail.
evaluate(ValuationPred, if_then_else(Variable, High, Low)) :-
    ( ValuationPred(Variable) ->
        evaluate(ValuationPred, High)
    ;
        evaluate(ValuationPred, Low)
    ).

bdd_from_positive_and_negative_atoms(PositiveAtoms, NegativeAtoms) =
    bdd_from_positive_and_negative_atoms_aux(to_sorted_list(PositiveAtoms), to_sorted_list(NegativeAtoms)).

:- func bdd_from_positive_and_negative_atoms_aux(list(Var), list(Var)) = bdd(Var).

bdd_from_positive_and_negative_atoms_aux([], []) = true_leaf.
bdd_from_positive_and_negative_atoms_aux([T|Ts], []) = if_then_else(T, High, false_leaf) :-
    High = bdd_from_positive_and_negative_atoms_aux(Ts, []).
bdd_from_positive_and_negative_atoms_aux([], [F|Fs]) = if_then_else(F, false_leaf, Low) :-
    Low = bdd_from_positive_and_negative_atoms_aux([], Fs).
bdd_from_positive_and_negative_atoms_aux([T|Ts], [F|Fs]) = if_then_else(Var, High, Low) :-
    compare(CompareRes, T, F),
    ( CompareRes = (=),
        error($pred ++ ": variable occurs in both positive and negative atoms " ++ string(T))
    ; CompareRes = (<),
        Var = T,
        High = bdd_from_positive_and_negative_atoms_aux(Ts, [F|Fs]),
        Low = false_leaf
    ; CompareRes = (>),
        Var = F,
        High = false_leaf,
        Low = bdd_from_positive_and_negative_atoms_aux([T|Ts], Fs)
    ).

get_vars(true_leaf) = set.init.
get_vars(false_leaf) = set.init.
get_vars(if_then_else(Var, High, Low)) = set.insert(set.union(get_vars(High), get_vars(Low)), Var).

bdd_to_dnf(BDD) = DNF :-
    bdd_to_dnf_aux(BDD) = AtomsLists,
    Conjunctions = list.map(atoms_list_to_conjunction, AtomsLists),
    DNF = conjunctions_to_disjunction(Conjunctions).

:- func bdd_to_dnf_aux(bdd(Var)) = list(list(boolean_expression(Var))).
bdd_to_dnf_aux(true_leaf) = [[]].
bdd_to_dnf_aux(false_leaf) = [].
bdd_to_dnf_aux(if_then_else(Var, High, Low)) = Var_And_A ++ NotVar_And_B :-
    Var_And_A = list.map(func(C) = [var(Var) | C], bdd_to_dnf_aux(High)),
    NotVar_And_B = list.map(func(C) = [not(var(Var)) | C], bdd_to_dnf_aux(Low)).

:- func atoms_list_to_conjunction(list(boolean_expression(Var))) = boolean_expression(Var).

atoms_list_to_conjunction([]) = true.
atoms_list_to_conjunction([A]) = A.
atoms_list_to_conjunction([A, B | As]) = and(A, atoms_list_to_conjunction([B | As])).

:- func conjunctions_to_disjunction(list(boolean_expression(Var))) = boolean_expression(Var).

conjunctions_to_disjunction([]) = false.
conjunctions_to_disjunction([A]) = A.
conjunctions_to_disjunction([A, B | As]) = or(A, conjunctions_to_disjunction([B | As])).

decompose_bdd(BDD, Entailed, Disentailed, Possibly) :-
    Vars = get_vars(BDD),
    Entailed = set.filter(
        (pred(V::in) is semidet :-
            solutions(pred(Low::out) is nondet :- part_of_bdd_with_var(V, BDD, _, Low), Lows),
            list.all_true(pred(false_leaf::in) is semidet, Lows)
        ), Vars),
    Disentailed = set.filter(
        (pred(V::in) is semidet :-
            solutions(pred(High::out) is nondet :- part_of_bdd_with_var(V, BDD, High, _), Highs),
            list.all_true(pred(false_leaf::in) is semidet, Highs)
        ), Vars),
    set.difference(Vars, set.union(Entailed, Disentailed), Possibly).

partial_evaluation(_, _, true_leaf) = true_leaf.
partial_evaluation(_, _, false_leaf) = false_leaf.
partial_evaluation(KnownValue, KnownVar, if_then_else(CurrentVar, High, Low) @ BDD) = ResultingBDD :-
    compare(CompareRes, KnownVar, CurrentVar),
    ( CompareRes = (=),
        ResultingBDD = (if KnownValue = yes then High else Low)
    ; CompareRes = (>),
        ResultingBDD = if_then_else(CurrentVar, partial_evaluation(KnownValue, KnownVar, High), partial_evaluation(KnownValue, KnownVar, Low))
    ; CompareRes = (<),
        ResultingBDD = BDD
    ).

:- pred part_of_bdd_with_var(Atom::in, bdd(Atom)::in, bdd(Atom)::out, bdd(Atom)::out) is nondet.
part_of_bdd_with_var(Var, if_then_else(Var, High, Low), High, Low).
part_of_bdd_with_var(Var, if_then_else(_, High1, Low1), High, Low) :-
    ( part_of_bdd_with_var(Var, High1, High, Low)
    ; part_of_bdd_with_var(Var, Low1, High, Low)
    ).

bdd_to_str(VarFormat, BDD) = BDDStr :-
    BDDStr = bdd_to_str_aux(VarFormat, BDD).

:- func bdd_to_str_aux(func(Var) = string, bdd(Var)) = string.

bdd_to_str_aux(_, true_leaf) = "true".
bdd_to_str_aux(_, false_leaf) = "false".
bdd_to_str_aux(VarFormat, if_then_else(_, _, _) @ BDD) = BDDStr :-
    decompose_bdd(BDD, Entailed, Disentailed, Possibly),
    PossiblyBDD1 = set.fold(partial_evaluation(yes), Entailed, BDD),
    PossiblyBDD = set.fold(partial_evaluation(no), Disentailed, PossiblyBDD1),
    EntailedStrs = list.map(VarFormat, to_sorted_list(Entailed)),
    DisentailedStrs = list.map(func(V) = "!" ++ VarFormat(V), to_sorted_list(Disentailed)),
    SomeVarsStr = join_list(" /\\ ", EntailedStrs ++ DisentailedStrs),
    ( set.remove_least(OneVar, Possibly, _) ->
        BDDStr = string.format("%s /\\ (%s /\\ (%s) \\/ !%s /\\ (%s))",
            [s(SomeVarsStr), s(VarFormat(OneVar)), s(bdd_to_str_aux(VarFormat, partial_evaluation(yes, OneVar, PossiblyBDD))),
            s(VarFormat(OneVar)), s(bdd_to_str_aux(VarFormat, partial_evaluation(no, OneVar, PossiblyBDD)))])
    ;
        BDDStr = SomeVarsStr
    ).

:- end_module bdd.
