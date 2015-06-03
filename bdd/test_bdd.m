%------------------------------------------------------------------------------%
%
% Module: test_bdd
% Copyright (c) Mission Critical IT
% MIT License
% Author: Ludovic Langevine <llg@missioncriticalit.com>
%
% Random and systematic testing of BDD implementation.
%
% This test module generate a set of boolean expressions of a given depth and:
%   - check that te BDD construction succeeds
%   - check that all the valuations of this BDD are the same as the naive valuations
%     of the boolean expression
%   - check that, for any composition of 2 boolean expressions:
%     - the BDD constructed from the composition of the 2 boolean expressions is
%       equal to the composition of the two BDDs.
%       (in other words, check the optimized version of "Apply")
%
% TODO: performance assessment.
%
%------------------------------------------------------------------------------%

:- module test_bdd.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bdd.
:- import_module boolean_expression.

:- import_module assoc_list.
:- import_module bool.
:- import_module exception.
:- import_module getopt.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module pprint.
:- import_module random.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.

:- type distribution(V) == assoc_list(V, int).

:- func leafs_distribution = distribution(boolean_expression(string)).

leafs_distribution = [true - 2, false - 2, var("A") - 5, var("B") - 5, var("C") - 4, var("D") - 3, var("E") - 2, var("F") - 2, var("G") - 1, var("H") - 1].

:- type bool_op
    ---> et
    ;    ou
    ;    xou
    .

:- func binary_operators_distribution = distribution(bool_op).

binary_operators_distribution = [et - 7, ou - 4, xou - 1].

:- pred one_test_expression_from_tree(list(string)::in, binary_tree::in, boolean_expression(string)::out) is nondet.

%one_test_expression_from_tree(_, leaf, true).
%one_test_expression_from_tree(_, leaf, false).

one_test_expression_from_tree([V | _Vars], leaf, var(V)).
one_test_expression_from_tree(Vars, node1(Tree), not(Expr)) :-
    one_test_expression_from_tree(Vars, Tree, Expr).
one_test_expression_from_tree(Vars, node2(T1, T2), and(E1, E2)) :-
    one_test_expression_from_tree(Vars, T1, E1),
    one_test_expression_from_tree(Vars, T2, E2),
    compare((>), E1, E2).
one_test_expression_from_tree(Vars, node2(T1, T2), or(E1, E2)) :-
    one_test_expression_from_tree(Vars, T1, E1),
    one_test_expression_from_tree(Vars, T2, E2),
    compare((>), E1, E2).

:- pred one_random_test_expression_from_tree(int::in, binary_tree::in, boolean_expression(string)::out, random.supply::in, random.supply::out) is det.

one_random_test_expression_from_tree(TargetDepth, Tree, RandomExpression, !Random) :-
    analyze_tree(Tree, BinaryNb, LeafsNb),
    get_n_values_from_distrib(binary_operators_distribution, BinaryNb, RandomOperators, !Random),
    get_n_values_from_distrib(leafs_distribution, LeafsNb, RandomLeafs, !Random),
    convert_binary_tree_to_boolean_expression(Tree, RandomOperators, _, RandomLeafs, _, RandomExpression1),
    ( depth(RandomExpression1) = TargetDepth + 1 ->
        RandomExpression = RandomExpression1
    ;
        one_random_test_expression_from_tree(TargetDepth, Tree, RandomExpression, !Random)
    ).

:- pred convert_binary_tree_to_boolean_expression(binary_tree::in, list(bool_op)::in, list(bool_op)::out,
    list(boolean_expression(Var))::in, list(boolean_expression(Var))::out, boolean_expression(Var)::out) is det.

convert_binary_tree_to_boolean_expression(leaf, !Ops, Leafs, det_tail(Leafs), det_head(Leafs)).
convert_binary_tree_to_boolean_expression(node1(SubTree), !Ops, !Leafs, not(SubExpr)) :-
    convert_binary_tree_to_boolean_expression(SubTree, !Ops, !Leafs, SubExpr).
convert_binary_tree_to_boolean_expression(node2(Tree1, Tree2), !Ops, !Leafs, Expression) :-
    Op = det_head(!.Ops),
    !:Ops = det_tail(!.Ops),
    convert_binary_tree_to_boolean_expression(Tree1, !Ops, !Leafs, SubExpr1),
    convert_binary_tree_to_boolean_expression(Tree2, !Ops, !Leafs, SubExpr2),
    ( Op = et,
        Expression = and(SubExpr1, SubExpr2)
    ; Op = ou,
        Expression = or(SubExpr1, SubExpr2)
    ; Op = xou,
        Expression = xor(SubExpr1, SubExpr2)
    ).

:- pred analyze_tree(binary_tree::in, int::out, int::out) is det.

analyze_tree(leaf, 0, 1).
analyze_tree(node1(SubTree), BinaryNodes, Leafs) :-
    analyze_tree(SubTree, BinaryNodes, Leafs).
analyze_tree(node2(Left, Right), BinaryNodes1 + BinaryNodes2 + 1, Leafs1 + Leafs2) :-
    analyze_tree(Left, BinaryNodes1, Leafs1),
    analyze_tree(Right, BinaryNodes2, Leafs2).


main(!IO) :-
    int.fold_up(perform_random_test_for_depth, 0, 6, !IO).
%    int.fold_up(perform_systematic_test_for_depth, 0, 3, !IO).

:- pred get_n_values_from_distrib(assoc_list(X, int)::in, int::in, list(X)::out, random.supply::in, random.supply::out) is det.

get_n_values_from_distrib(Distribution, NeededLeafsNb, Values, !Random) :-
    MaxPos = list.foldl(int.plus, values(Distribution), 0),
    int.fold_up2(
        (pred(_::in, PreviousValues::in, [V|PreviousValues]::out, !.R::in, !:R::out) is det :-
            random.random(1, MaxPos, PositionV, !R),
            get_value_at_position(Distribution, PositionV, V)
        ), 1, NeededLeafsNb, [], Values, !Random).

:- pred get_value_at_position(assoc_list(V, int)::in, int::in, V::out) is det.

get_value_at_position([], _, _) :-
    error($pred ++ ": empty list").
get_value_at_position([V-I | R], Pos, Value) :-
    ( Pos =< I ->
        Value = V
    ;
        get_value_at_position(R, Pos - I, Value)
    ).

:- pred perform_random_test_for_depth(int::in, io::di, io::uo) is det.

perform_random_test_for_depth(Depth, !IO) :-
    io.format("Processing random test for depth=%d\n", [i(Depth)], !IO),
    solutions.solutions(binary_tree(Depth), Trees),
    io.format("\t%d binary trees will be used\n", [i(length(Trees))], !IO),
    
    io.write_string(to_string(120, to_doc(Trees)) ++ ".\n", !IO),
    random.init(17, RandomSupplier1),
    list.map_foldl(one_random_test_expression_from_tree(Depth), Trees, Expressions, RandomSupplier1, RandomSupplier2),
    list.foldl2(
        (pred(Expr::in, !.R::in, !:R::out, !.IO1::di, !:IO1::uo) is det :-
            Vars = to_sorted_list(get_vars(Expr)),
            get_n_valuations_for_variables(1 << length(Vars) - 1, Vars, Valuations, !R),
            test_with_single_expression(sort_and_remove_dups(Valuations), Expr, !IO1)
        ), Expressions, RandomSupplier2, RandomSupplier3, !IO),
    random.permutation(Expressions, PermutatedExpressions, RandomSupplier3, RandomSupplier4),
    get_n_values_from_distrib(binary_operators_distribution, length(Expressions) - 1, RandomOperators, RandomSupplier4, _),
    test_aggregations_of_expressions(PermutatedExpressions, RandomOperators, !IO).

:- pred test_aggregations_of_expressions(list(boolean_expression(Var))::in, list(bool_op)::in, io::di, io::uo) is det.

test_aggregations_of_expressions([], _, !IO).
test_aggregations_of_expressions([FirstExpr | OtherExprs], Operators, !IO) :-
    test_aggregations_of_expressions_aux(OtherExprs, FirstExpr, bdd(FirstExpr), Operators, !IO).

:- pred test_aggregations_of_expressions_aux(list(boolean_expression(Var))::in, boolean_expression(Var)::in, bdd(Var)::in, list(bool_op)::in, io::di, io::uo) is det.

test_aggregations_of_expressions_aux([], _, _, _, !IO).
test_aggregations_of_expressions_aux([NewExpr | OtherExprs], !.CurrentExpr @ OldExpr, !.CurrentBDD @ OldBDD, Operators, !IO) :-
    Op = det_head(Operators),
    ( Op = et,
        !:CurrentExpr = and(!.CurrentExpr, NewExpr),
        !:CurrentBDD = !.CurrentBDD `and` bdd(NewExpr)
    ; Op = ou,
        !:CurrentExpr = or(!.CurrentExpr, NewExpr),
        !:CurrentBDD = !.CurrentBDD `or` bdd(NewExpr)
    ; Op = xou,
        !:CurrentExpr = xor(!.CurrentExpr, NewExpr),
        !:CurrentBDD = !.CurrentBDD `xor` bdd(NewExpr)
    ),
    ( !.CurrentBDD = bdd(!.CurrentExpr) ->
        io.format("Apply %s: ok\n", [s(string(Op))], !IO)
    ;
        io.format("Error when applying %s:\nFirst expression:\n\t%s\nBDD:\n\t%s\nAdding:\n\t%s\nResulting BDD:\n\t%s\nBDD built from scrach:\n\t%s\n",
            [s(string(Op)), s(bool_expr_to_str(OldExpr)), s(string(OldBDD)), s(bool_expr_to_str(NewExpr)), s(string(!.CurrentBDD)), s(string(bdd(!.CurrentExpr)))], !IO),
        error($pred ++ " failed")
    ),
    test_aggregations_of_expressions_aux(OtherExprs, !.CurrentExpr, !.CurrentBDD, det_tail(Operators), !IO).

:- pred perform_systematic_test_for_depth(int::in, io::di, io::uo) is det.

perform_systematic_test_for_depth(Depth, !IO) :-
    io.format("Processing systematic test for depth=%d\n", [i(Depth)], !IO),
    solutions.solutions(binary_tree(Depth), Trees),
    io.format("\t%d binary trees will be used\n", [i(length(Trees))], !IO),

    list.foldl(systematic_test_for_binary_tree(Depth), Trees, !IO).

:- pred systematic_test_for_binary_tree(int::in, binary_tree::in, io::di, io::uo) is det.

systematic_test_for_binary_tree(Depth, Tree, !IO) :-
    io.write_string("Systematic test for binary tree:\n" ++ to_string(120, to_doc(Tree)) ++ ".\n", !IO),
    analyze_tree(Tree, _, LeafsNb),
    solutions.solutions(one_test_expression_from_tree(take_upto(LeafsNb, ["A", "B", "C", "D", "E", "F"]), Tree), Expressions),
    list.filter_map(
        (pred(E::in, Es::out) is semidet :-
            simplify_expression_and_get_vars(E, Es, Vars),
            \+ set.empty(Vars),
            depth(Es) = Depth - 1
        ), Expressions, SimplifiedExpressions),
    UniqueExpressions = list.sort_and_remove_dups(SimplifiedExpressions),
    io.format("%d boolean expressions will be used.\n", [i(length(UniqueExpressions))], !IO),
    random.init(17, RandomSupplier),
    list.foldl2(
        (pred(Expr::in, !.R::in, !:R::out, !.IO1::di, !:IO1::uo) is det :-
            Vars = to_sorted_list(get_vars(Expr)),
            get_n_valuations_for_variables(1 << length(Vars) - 1, Vars, Valuations, !R),
            test_with_single_expression(sort_and_remove_dups(Valuations), Expr, !IO1)
        ), Expressions, RandomSupplier, _, !IO).

/*:- pred generate_relevant_expressions(int::in, io::di, io::uo) is det.

generate_relevant_expressions(Depth, !IO) :-
    analyze_tree(Tree, _, LeafsNb),
    solutions.solutions(one_test_expression_from_tree(take_upto(LeafsNb, ["A", "B", "C", "D", "E", "F"]), Tree), Expressions),
    list.filter_map(
        (pred(E::in, Es::out) is semidet :-
            simplify_expression_and_get_vars(E, Es, Vars),
            \+ set.empty(Vars),
            depth(Es) = Depth - 1
        ), Expressions, SimplifiedExpressions),
    UniqueExpressions = list.sort_and_remove_dups(SimplifiedExpressions).*/

:- pred get_n_valuations_for_variables(int::in, list(V)::in,  list(valuation(V))::out, random.supply::in, random.supply::out) is det.

get_n_valuations_for_variables(_, [], [map.init],  !Random).
get_n_valuations_for_variables(Nb, [Var | Vars], Valuations,  !Random) :-
    get_n_valuations_for_variables(Nb >> 1, Vars, Valuations1,  !Random),
    ( Nb mod 2 = 0 ->
        random(0, 2, V, !Random),
        VarValues = [(if V = 0 then no else yes)]
    ; Nb mod 2 = 1 ->
        VarValues = [yes, no]
    ;
        error($pred ++ " unexpected modulo result.")
    ),
    Valuations = condense(list.map(
        (func(Value) = list.map(func(Valuation) = map.det_insert(Valuation, Var, Value), Valuations1)), VarValues)).

:- pred test_with_single_expression(list(valuation(Var))::in, boolean_expression(Var)::in, io::di, io::uo) is det.

test_with_single_expression(ValuationsToTest, BooleanExpression, !IO) :-
    promise_equivalent_solutions [BDDBuilingResult] (
        try(pred(BDD_res::out) is det :- BDD_res = bdd(BooleanExpression), BDDBuilingResult)
    ),
    ( BDDBuilingResult = succeeded(BDD)
    ; BDDBuilingResult = exception(_),
        simplify_expression_and_get_vars(BooleanExpression, SimplifiedExpression1, Vars),
        io.format("Unable to construct BDD for expression: %s involving %s\n", [s(bool_expr_to_str(SimplifiedExpression1)), s(string(to_sorted_list(Vars) : list(_)))], !IO),
        rethrow(BDDBuilingResult)
    ),
    list.filter(evaluation_not_the_same(BooleanExpression, BDD), ValuationsToTest, FailedValuations),
    io.format("Try %d valuations of %s:\t ", [i(length(ValuationsToTest)), s(bool_expr_to_str(BooleanExpression))], !IO),
    ( FailedValuations = [],
        io.write_string("ok.\n", !IO)
    ; FailedValuations = [_|_],
        simplify_expression_and_get_vars(BooleanExpression, SimplifiedExpression, _),
        io.format("failed for %d valuations. The BDD was:\n%s\nBDD's answers:\nSimplified boolean expression: %s\n", [i(length(FailedValuations)),
            s(to_string(120, to_doc(BDD))), s(bool_expr_to_str(SimplifiedExpression))], !IO),
        list.foldl(
            (pred(Valuation::in, !.IO1::di, !:IO1::uo) is det :-
                RepBDD = evaluate(BDD, Valuation),
                Expected = evaluate(BooleanExpression, Valuation),
                io.format("%s\t instead of\t%s for valuation:\n\t\t%s\n", [s(string(RepBDD)), s(string(Expected)),
                s(to_string(120, to_doc(to_assoc_list(Valuation) : assoc_list(_, bool))))], !IO1)
            ), FailedValuations, !IO)
    ).

:- pred evaluation_not_the_same(boolean_expression(Var)::in, bdd(Var)::in, valuation(Var)::in) is semidet.

evaluation_not_the_same(Expression, BDD, Valuation) :-
    evaluate(Expression, Valuation) \= evaluate(BDD, Valuation).

:- type binary_tree
    ---> node2(
               left_subtree  :: binary_tree,
               right_subtree :: binary_tree
              )
    ;     node1(
                subtree       :: binary_tree
               )
    ;    leaf
    .

%
% Supplier of binary trees of given depth (up to 8).
%
:- pred binary_tree(int::in, binary_tree::out) is nondet.

binary_tree(0, leaf).

binary_tree(1, node1(leaf)).
binary_tree(1, node2(leaf, leaf)).
binary_tree(2, node2(SubTree1, SubTree2)) :-
    binary_tree(1, SubTree1),
    binary_tree(1, SubTree2).
binary_tree(2, node1(SubTree)) :-
    binary_tree(1, SubTree).
binary_tree(3, node1(SubTree)) :-
    binary_tree(2, SubTree).
binary_tree(3, node2(SubTree1, SubTree2)) :-
    binary_tree(2, SubTree1),
    binary_tree(2, SubTree2).
binary_tree(3, node2(SubTree1, SubTree2)) :-
    binary_tree(2, SubTree1),
    binary_tree(1, SubTree2).
binary_tree(N, node2(SubTree1, SubTree2)) :-
    list.member(N, [4, 5, 6, 7, 8]),
    list.member(N2, [0, 1, 2, 3]),
    N2 < N / 2,
    binary_tree(N - 1, SubTree1),
    binary_tree(N2, SubTree2).
binary_tree(N, node1(SubTree)) :-
    list.member(N, [4, 5, 6, 7, 8]),
    binary_tree(N - 1, SubTree).

:- end_module test_bdd.
