%------------------------------------------------------------------------------%
%
% Module: boolean_expression
% Author: Ludovic Langevine <llg@missioncriticalit.com>
%
% Simple representation and implementation of boolean expressions
%
% This module exports a type to represent a boolean expression with variables.
%
% It implements:
%   - naive evaluation of such an expression give a valuation (function V -> bool)
%   - basic simplification algorithm
%   - computation of depth and set of occuring variables
%   - serialization to string
%   - substitution of a variable
%
% Those algorithms and the underlying representation are not optimized. They are
% aimed at supporting BDD implementation.
%
%------------------------------------------------------------------------------%

:- module boolean_expression.

:- interface.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module set.

:- type boolean_expression(BooleanVar)
    ---> var(BooleanVar)
    ;    not(boolean_expression(BooleanVar))
    ;    and(boolean_expression(BooleanVar), boolean_expression(BooleanVar))
    ;    or(boolean_expression(BooleanVar), boolean_expression(BooleanVar))
    ;    xor(boolean_expression(BooleanVar), boolean_expression(BooleanVar))
    ;    true
    ;    false
    .

:- pred is_non_trivial(boolean_expression(Atom), boolean_expression(Atom)).
:- mode is_non_trivial(in(non_trivial_dnf), out(non_trivial_dnf)) is det.
:- mode is_non_trivial(in(dnf), out(non_trivial_dnf)) is semidet.

:- inst dnf
    ---> true
    ;    false
    ;    or(non_trivial_dnf, non_trivial_dnf)
    ;    and(conjunction, conjunction)
    ;    not(ground_var)
    ;    var(ground)
    .

:- inst non_trivial_dnf
    ---> or(non_trivial_dnf, non_trivial_dnf)
    ;    and(conjunction, conjunction)
    ;    not(ground_var)
    ;    var(ground)
    .

:- inst conjunction
    ---> and(ground_var, conjunction)
    ;    var(ground)
    ;    not(ground_var)
    .

:- inst ground_var
    ---> var(ground).

:- inst simplified_expression
    ---> var(ground)
    ;    not(simplified_non_constant_boolean_expression)
    ;    and(simplified_non_constant_boolean_expression, simplified_non_constant_boolean_expression)
    ;    or(simplified_non_constant_boolean_expression, simplified_non_constant_boolean_expression)
    ;    xor(simplified_non_constant_boolean_expression, simplified_non_constant_boolean_expression)
    ;    true
    ;    false
    .

:- inst simplified_non_constant_boolean_expression
    ---> var(ground)
    ;    not(simplified_non_constant_boolean_expression)
    ;    and(simplified_non_constant_boolean_expression, simplified_non_constant_boolean_expression)
    ;    or(simplified_non_constant_boolean_expression, simplified_non_constant_boolean_expression)
    ;    xor(simplified_non_constant_boolean_expression, simplified_non_constant_boolean_expression)
    .

:- func get_vars(boolean_expression(BooleanVar)) = set(BooleanVar).

:- func evaluate(boolean_expression(BooleanVar), valuation(BooleanVar)) = bool.

:- pred substitute(BooleanVar::in, bool::in, boolean_expression(BooleanVar)::in(simplified_expression),
    boolean_expression(BooleanVar)::out(simplified_expression)) is det.

:- func bool_expr_to_str(boolean_expression(Var)) = string.

:- type valuation(BooleanVar) == map(BooleanVar, bool).
:- type valuation_pred(BooleanVar) == pred(BooleanVar).

:- inst valuation_pred == (pred(in) is semidet).

:- pred simplify_expression_and_get_vars(boolean_expression(Var)::in, boolean_expression(Var)::out(simplified_expression), set(Var)::out) is det.

:- func depth(boolean_expression(_)) = int.

:- func boolean_expression_map(func(V1) = boolean_expression(V2), boolean_expression(V1)) = boolean_expression(V2).

:- func and_list(list(boolean_expression(V))) = boolean_expression(V).

:- func or_list(list(boolean_expression(V))) = boolean_expression(V).

:- implementation.

:- import_module string.

:- import_module bdd.

is_non_trivial(or(_, _) @ Or, Or).
is_non_trivial(and(_, _) @ And, And).
is_non_trivial(var(A), var(A)).
is_non_trivial(not(var(_)) @ Not, Not).

simplify_expression_and_get_vars(true, true, set.init).
simplify_expression_and_get_vars(false, false, set.init).
simplify_expression_and_get_vars(var(Var), var(Var), set.make_singleton_set(Var)).
simplify_expression_and_get_vars(not(SubExpression), SimplifiedExpression, Vars) :-
    simplify_expression_and_get_vars(SubExpression, SimplifiedSubExpression, Vars),
    ( SimplifiedSubExpression = not(SimplifiedExpression1),
        SimplifiedExpression = SimplifiedExpression1
    ; SimplifiedSubExpression = true,
        SimplifiedExpression = false
    ; SimplifiedSubExpression = false,
        SimplifiedExpression = true
    ; ( SimplifiedSubExpression = var(_)
      ; SimplifiedSubExpression = and(_,_)
      ; SimplifiedSubExpression = or(_,_)
      ; SimplifiedSubExpression = xor(_,_)
      ),
        SimplifiedExpression = not(SimplifiedSubExpression)
    ).
simplify_expression_and_get_vars(and(A, B), SimplifiedExpression, set.union(VarsA, VarsB)) :-
    simplify_expression_and_get_vars(A, Ap, VarsA),
    simplify_expression_and_get_vars(B, Bp, VarsB),
    ( Ap = true,
        SimplifiedExpression = Bp
    ; Ap = false,
        SimplifiedExpression = false
    ; ( Ap = var(_) ; Ap = not(_) ; Ap = and(_,_) ; Ap = or(_,_) ; Ap = xor(_,_) ),
        ( Bp = true,
            SimplifiedExpression = Ap
        ; Bp = false,
            SimplifiedExpression = false
        ; ( Bp = var(_) ; Bp = not(_) ; Bp = and(_,_) ; Bp = or(_,_) ; Bp = xor(_,_) ),
            SimplifiedExpression = and(Ap, Bp)
        )
    ).
simplify_expression_and_get_vars(or(A, B), SimplifiedExpression, Vars) :-
    simplify_expression_and_get_vars(A, Ap, VarsA),
    simplify_expression_and_get_vars(B, Bp, VarsB),
    ( Ap = true,
        SimplifiedExpression = true,
        Vars = set.init
    ; Ap = false,
        SimplifiedExpression = Bp,
        Vars = VarsB
    ; ( Ap = var(_) ; Ap = not(_) ; Ap = and(_,_) ; Ap = or(_,_) ; Ap = xor(_,_) ),
        ( Bp = true,
            SimplifiedExpression = true,
            Vars = set.init
        ; Bp = false,
            SimplifiedExpression = Ap,
            Vars = VarsA
        ; ( Bp = var(_) ; Bp = not(_) ; Bp = and(_,_) ; Bp = or(_,_) ; Bp = xor(_,_) ),
            SimplifiedExpression = or(Ap, Bp),
            Vars = set.union(VarsA, VarsB)
        )
    ).
simplify_expression_and_get_vars(xor(A, B), SimplifiedExpression, Vars) :-
    simplify_expression_and_get_vars(A, Ap, VarsA),
    simplify_expression_and_get_vars(B, Bp, VarsB),
    ( Ap = true,
        ( Bp = true,
            SimplifiedExpression = false,
            Vars = set.init
        ; Bp = false,
            SimplifiedExpression = true,
            Vars = set.init
        ; Bp = not(NegatedBp),
            SimplifiedExpression = NegatedBp,
            Vars = VarsB
        ; ( Bp = var(_) ; Bp = and(_,_) ; Bp = or(_,_) ; Bp = xor(_,_) ),
            SimplifiedExpression = not(Bp),
            Vars = VarsB
        )
    ; Ap = false,
        SimplifiedExpression = Bp,
        Vars = VarsB
    ; ( Ap = var(_) ; Ap = not(_) ; Ap = and(_,_) ; Ap = or(_,_) ; Ap = xor(_,_) ),
        ( Bp = true,
            ( Ap = not(NegatedAp),
                SimplifiedExpression = NegatedAp
            ; ( Ap = var(_) ; Ap = and(_,_) ; Ap = or(_,_) ; Ap = xor(_,_) ),
                SimplifiedExpression = not(Ap)
            ),
            Vars = VarsA
        ; Bp = false,
            SimplifiedExpression = Ap,
            Vars = VarsA
        ; ( Bp = var(_) ; Bp = not(_) ; Bp = and(_,_) ; Bp = or(_,_) ; Bp = xor(_,_)
          ),
            SimplifiedExpression = xor(Ap, Bp),
            Vars = set.union(VarsA, VarsB)
        )
    ).

substitute(_, _, true, true).
substitute(_, _, false, false).
substitute(Var, Value, var(V), Expr) :-
    ( Var = V ->
        ( Value = yes,
            Expr = true
        ; Value = no,
            Expr = false
        )
    ;
        Expr = var(V)
    ).
substitute(Var, Value, not(A), Expr) :-
    substitute(Var, Value, A, Ap),
    ( Ap = true,
        Expr = false
    ; Ap = false,
        Expr = true
    ; Ap = var(_),
        Expr = not(Ap)
    ; Ap = not(Bp),
        Expr = Bp
    ; ( Ap = and(_,_) ; Ap = or(_,_) ; Ap = xor(_,_)),
        Expr = not(Ap)
    ).
substitute(Var, Value, and(A, B), Expr) :-
    substitute(Var, Value, A, Ap),
    ( Ap = true,
        substitute(Var, Value, B, Expr)
    ; Ap = false,
        Expr = false
    ; (Ap = and(_,_) ; Ap = or(_,_) ; Ap = xor(_,_) ; Ap = not(_) ; Ap = var(_)),
        substitute(Var, Value, B, Bp),
        ( Bp = true,
            Expr = Ap
        ; Bp = false,
            Expr = false
        ; (Bp = var(_) ; Bp = not(_) ; Bp = and(_,_) ; Bp = or(_,_) ; Bp = xor(_,_)),
            Expr = and(Ap, Bp)
        )
    ).
substitute(Var, Value, or(A, B), Expr) :-
    substitute(Var, Value, A, Ap),
    ( Ap = true,
        Expr = true
    ; Ap = false,
        substitute(Var, Value, B, Expr)
    ; ( Ap = not(_) ; Ap = var(_) ; Ap = and(_,_) ; Ap = or(_,_) ; Ap = xor(_,_)),
        substitute(Var, Value, B, Bp),
        ( Bp = true,
            Expr = true
        ; Bp = false,
            Expr = Ap
        ; (Bp = var(_) ; Bp = not(_) ; Bp = and(_,_) ; Bp = or(_,_) ; Bp = xor(_,_)),
            Expr = or(Ap, Bp)
        )
    ).
substitute(Var, Value, xor(A, B), Expr) :-
    substitute(Var, Value, A, Ap),
    ( Ap = true,
        substitute(Var, Value, not(B), Expr)
    ; Ap = false,
        substitute(Var, Value, B, Expr)
    ; ( Ap = not(_) ; Ap = var(_) ; Ap = and(_,_) ; Ap = or(_,_) ; Ap = xor(_,_)),
        substitute(Var, Value, B, Bp),
        ( Bp = true,
            Expr = not(Ap)
        ; Bp = false,
            Expr = Ap
        ; (Bp = var(_) ; Bp = not(_) ; Bp = and(_,_) ; Bp = or(_,_) ; Bp = xor(_,_)),
            Expr = xor(Ap, Bp)
        )
    ).

get_vars(Expr) = Vars :-
    ( Expr = var(Var),
        Vars = set.make_singleton_set(Var)
    ; ( Expr = true
      ; Expr = false
      ),
        Vars = set.init
    ; ( Expr = and(A, B)
      ; Expr = or(A, B)
      ; Expr = xor(A, B)
      ),
      Vars = set.union(get_vars(A), get_vars(B))
    ; Expr = not(A),
        Vars = get_vars(A)
    ).

depth(Expr) = Depth :-
    ( ( Expr = true ; Expr = false ; Expr = var(_) ),
        Depth = 1
    ; ( Expr = and(A, B) ; Expr = or(A, B) ; Expr = xor(A, B) ),
        Depth = 1 + int.max(depth(A), depth(B))
    ; Expr = not(A),
        Depth = 1 + depth(A)
    ).

evaluate(true, _) = yes.
evaluate(false, _) = no.
evaluate(var(Var), Valuation) = map.lookup(Valuation, Var).
evaluate(not(Expr), Valuation) = bool.not(evaluate(Expr, Valuation)).
evaluate(and(ExprA, ExprB), Valuation) = bool.and(Va, Vb) :-
    Va = evaluate(ExprA, Valuation),
    Vb = evaluate(ExprB, Valuation).
evaluate(or(ExprA, ExprB), Valuation) = bool.or(Va, Vb) :-
    Va = evaluate(ExprA, Valuation),
    Vb = evaluate(ExprB, Valuation).
evaluate(xor(ExprA, ExprB), Valuation) = bool.xor(Va, Vb) :-
    Va = evaluate(ExprA, Valuation),
    Vb = evaluate(ExprB, Valuation).

bool_expr_to_str(true) = "true".
bool_expr_to_str(false) = "false".
bool_expr_to_str(var(V)) = "?" ++ string(V).
bool_expr_to_str(not(E)) = "not(" ++ bool_expr_to_str(E) ++ ")".
bool_expr_to_str(and(A, B)) = "(" ++ bool_expr_to_str(A) ++ ") and (" ++ bool_expr_to_str(B) ++ ")".
bool_expr_to_str(or(A, B)) = "(" ++ bool_expr_to_str(A) ++ ") or (" ++ bool_expr_to_str(B) ++ ")".
bool_expr_to_str(xor(A, B)) = "(" ++ bool_expr_to_str(A) ++ ") xor (" ++ bool_expr_to_str(B) ++ ")".



boolean_expression_map(F, var(V)) = F(V).
boolean_expression_map(F, not(E)) = not(boolean_expression_map(F, E)).
boolean_expression_map(F, and(E1, E2)) = and(boolean_expression_map(F, E1), boolean_expression_map(F, E2)).
boolean_expression_map(F, or(E1, E2)) = or(boolean_expression_map(F, E1), boolean_expression_map(F, E2)).
boolean_expression_map(F, xor(E1, E2)) = xor(boolean_expression_map(F, E1), boolean_expression_map(F, E2)).
boolean_expression_map(_, true) = true.
boolean_expression_map(_, false) = false.


and_list([]) = true.
and_list([E | Es]) = and(E, and_list(Es)).

or_list([]) = false.
or_list([E | Es]) = or(E, or_list(Es)).

:- end_module boolean_expression.
