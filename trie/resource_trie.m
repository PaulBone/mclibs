%-----------------------------------------------------------------------------%
:- module resource_trie.
% A trie with an rdf.resource key.
%-----------------------------------------------------------------------------%

:- interface.

:- import_module rdf.

:- import_module bool.
:- import_module list.

%-----------------------------------------------------------------------------%
% The functions and predicates below have the same meanings as those in
% trie.m with the same name.
%

:- type resource_trie(V).

:- func init = resource_trie(V).

:- pred search(resource_trie(V)::in, rdf.resource::in, V::out) is semidet.

:- pred set(rdf.resource::in, V::in,
    resource_trie(V)::in, resource_trie(V)::out) is det.

:- pred delete(rdf.resource::in,
    resource_trie(V)::in, resource_trie(V)::out) is det.

:- func count(resource_trie(V)) = int.

:- func keys(resource_trie(V)) = list(rdf.resource).

:- func values(resource_trie(V)) = list(V).

:- pred is_empty(resource_trie(V)::in) is semidet.

:- pred foldl(pred(rdf.resource, V, A, A), resource_trie(V), A, A).
:- mode foldl(in(pred(in, in, in, out) is det), in, in, out) is det.
:- mode foldl(in(pred(in, in, in, out) is semidet), in, in, out) is semidet.
:- mode foldl(in(pred(in, in, di, uo) is det), in, di, uo) is det.
:- mode foldl(in(pred(in, in, in, out) is cc_multi), in, in, out)
    is cc_multi.
:- mode foldl(in(pred(in, in, di, uo) is cc_multi), in, di, uo) is cc_multi.

:- pred foldl2(pred(rdf.resource, V, A, A, B, B), resource_trie(V),
    A, A, B, B).
:- mode foldl2(in(pred(in, in, in, out, in, out) is det), in, in, out,
    in, out) is det.
:- mode foldl2(in(pred(in, in, in, out, in, out) is semidet), in, in, out,
    in, out) is semidet.
:- mode foldl2(in(pred(in, in, in, out, di, uo) is det), in, in, out,
    di, uo) is det.
:- mode foldl2(in(pred(in, in, in, out, in, out) is cc_multi), in, in, out,
    in, out) is cc_multi.
:- mode foldl2(in(pred(in, in, in, out, di, uo) is cc_multi), in, in, out,
    di, uo) is cc_multi.

:- pred foldl3(pred(rdf.resource, V, A, A, B, B, C, C), resource_trie(V),
    A, A, B, B, C, C).
:- mode foldl3(in(pred(in, in, in, out, in, out, in, out) is det), in,
    in, out, in, out, in, out) is det.
:- mode foldl3(in(pred(in, in, in, out, in, out, in, out) is semidet),
    in, in, out, in, out, in, out) is semidet.
:- mode foldl3(in(pred(in, in, in, out, in, out, di, uo) is det), in,
    in, out, in, out, di, uo) is det.
:- mode foldl3(in(pred(in, in, in, out, in, out, in, out) is cc_multi),
    in, in, out, in, out, in, out) is cc_multi.
:- mode foldl3(in(pred(in, in, in, out, in, out, di, uo) is cc_multi),
    in, in, out, in, out, di, uo) is cc_multi.

:- pred foldl_while(pred(rdf.resource, V, bool, A, A), resource_trie(V), bool,
    A, A).
:- mode foldl_while(in(pred(in, in, out, in, out) is det), in, out, in, out)
    is det.
:- mode foldl_while(in(pred(in, in, out, di, uo) is det), in, out, di, uo)
    is det.

:- pred foldl2_while(pred(rdf.resource, V, bool, A, A, B, B), resource_trie(V), 
    bool, A, A, B, B).
:- mode foldl2_while(in(pred(in, in, out, in, out, in, out) is det), in,
    out, in, out, in, out) is det.
:- mode foldl2_while(in(pred(in, in, out, in, out, di, uo) is det), in,
    out, in, out, di, uo) is det.

:- pred foldl3_while(pred(rdf.resource, V, bool, A, A, B, B, C, C),
    resource_trie(V), bool,
    A, A, B, B, C, C).
:- mode foldl3_while(in(pred(in, in, out, in, out, in, out, in, out) is det),
    in, out, in, out, in, out, in, out) is det.
:- mode foldl3_while(in(pred(in, in, out, in, out, in, out, di, uo) is det), in,
    out, in, out, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module trie.
:- import_module uri.

:- import_module int.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type resource_trie(V)
    --->    resource_trie(
                uri_trie    :: trie(V),
                anon_trie   :: trie(V)
            ).

init = resource_trie(trie.init, trie.init).

search(RTrie, Key, Val) :-
    det_search(RTrie, Key, yes(Val)).

:- pred det_search(resource_trie(V)::in, rdf.resource::in, maybe(V)::out)
    is det.

det_search(resource_trie(URITrie, _), uri(URI), MaybeVal) :-
    ( trie.search(URITrie, URI ^ str, Val) ->
        MaybeVal = yes(Val)
    ;
        MaybeVal = no
    ).
det_search(resource_trie(_, AnonTrie), anon(Id), MaybeVal) :-
    ( trie.search(AnonTrie, Id, Val) ->
        MaybeVal = yes(Val)
    ;
        MaybeVal = no
    ).

set(uri(URI), Val, resource_trie(UT0, AT), resource_trie(UT, AT)) :-
    trie.set(URI ^ str, Val, UT0, UT).
set(anon(Id), Val, resource_trie(UT, AT0), resource_trie(UT, AT)) :-
    trie.set(Id, Val, AT0, AT).

delete(uri(URI), resource_trie(UT0, AT), resource_trie(UT, AT)) :-
    trie.delete(URI ^ str, UT0, UT).
delete(anon(Id), resource_trie(UT, AT0), resource_trie(UT, AT)) :-
    trie.delete(Id, AT0, AT).

count(resource_trie(UT, AT)) = trie.count(UT) + trie.count(AT).

keys(resource_trie(UT, AT))
    = list.map(func(A) = anon(A), keys(AT))
    % XXX this is what is going to be expensive?
    % Maybe it's cheaper to get the keys and then lookup 
    % the values to get the actual values
   ++ list.map(func(U) = uri(U ^ uri), keys(UT)).

values(resource_trie(UT, AT))
    = values(AT) ++ values(UT).

is_empty(resource_trie(UT, AT)) :-
    trie.is_empty(UT),
    trie.is_empty(AT).

:- pragma promise_equivalent_clauses(foldl/4).

foldl(Pred::in(pred(in, in, in, out) is det), resource_trie(UT, AT)::in,
        !.A::in, !:A::out) :-
    trie.foldl(
        ( pred(K::in, V::in, A0::in, A1::out) is det :-
            Pred(uri(K ^ uri), V, A0, A1)
        ), UT, !A),
    trie.foldl(
        ( pred(K::in, V::in, A0::in, A1::out) is det :-
            Pred(anon(K), V, A0, A1)
        ), AT, !A).
foldl(Pred::in(pred(in, in, in, out) is semidet), resource_trie(UT, AT)::in,
        !.A::in, !:A::out) :-
    trie.foldl(
        ( pred(K::in, V::in, A0::in, A1::out) is semidet :-
            Pred(uri(K ^ uri), V, A0, A1)
        ), UT, !A),
    trie.foldl(
        ( pred(K::in, V::in, A0::in, A1::out) is semidet :-
            Pred(anon(K), V, A0, A1)
        ), AT, !A).
foldl(Pred::in(pred(in, in, di, uo) is det), resource_trie(UT, AT)::in,
        !.A::di, !:A::uo) :-
    trie.foldl(
        ( pred(K::in, V::in, A0::di, A1::uo) is det :-
            Pred(uri(K ^ uri), V, A0, A1)
        ), UT, !A),
    trie.foldl(
        ( pred(K::in, V::in, A0::di, A1::uo) is det :-
            Pred(anon(K), V, A0, A1)
        ), AT, !A).
foldl(Pred::in(pred(in, in, in, out) is cc_multi), resource_trie(UT, AT)::in,
        !.A::in, !:A::out) :-
    trie.foldl(
        ( pred(K::in, V::in, A0::in, A1::out) is cc_multi :-
            Pred(uri(K ^ uri), V, A0, A1)
        ), UT, !A),
    trie.foldl(
        ( pred(K::in, V::in, A0::in, A1::out) is cc_multi :-
            Pred(anon(K), V, A0, A1)
        ), AT, !A).
foldl(Pred::in(pred(in, in, di, uo) is cc_multi), resource_trie(UT, AT)::in,
        !.A::di, !:A::uo) :-
    trie.foldl(
        ( pred(K::in, V::in, A0::di, A1::uo) is cc_multi :-
            Pred(uri(K ^ uri), V, A0, A1)
        ), UT, !A),
    trie.foldl(
        ( pred(K::in, V::in, A0::di, A1::uo) is cc_multi :-
            Pred(anon(K), V, A0, A1)
        ), AT, !A).

:- pragma promise_equivalent_clauses(foldl2/6).

foldl2(Pred::in(pred(in, in, in, out, in, out) is det),
        resource_trie(UT, AT)::in,
        !.A::in, !:A::out, !.B::in, !:B::out) :-
    trie.foldl2(
        ( pred(K::in, V::in, A0::in, A1::out, B0::in, B1::out) is det :-
            Pred(uri(K ^ uri), V, A0, A1, B0, B1)
        ), UT, !A, !B),
    trie.foldl2(
        ( pred(K::in, V::in, A0::in, A1::out, B0::in, B1::out) is det :-
            Pred(anon(K), V, A0, A1, B0, B1)
        ), AT, !A, !B).
foldl2(Pred::in(pred(in, in, in, out, in, out) is semidet),
        resource_trie(UT, AT)::in,
        !.A::in, !:A::out, !.B::in, !:B::out) :-
    trie.foldl2(
        ( pred(K::in, V::in, A0::in, A1::out, B0::in, B1::out) is semidet :-
            Pred(uri(K ^ uri), V, A0, A1, B0, B1)
        ), UT, !A, !B),
    trie.foldl2(
        ( pred(K::in, V::in, A0::in, A1::out, B0::in, B1::out) is semidet :-
            Pred(anon(K), V, A0, A1, B0, B1)
        ), AT, !A, !B).
foldl2(Pred::in(pred(in, in, in, out, in, out) is cc_multi),
        resource_trie(UT, AT)::in,
        !.A::in, !:A::out, !.B::in, !:B::out) :-
    trie.foldl2(
        ( pred(K::in, V::in, A0::in, A1::out, B0::in, B1::out) is cc_multi :-
            Pred(uri(K ^ uri), V, A0, A1, B0, B1)
        ), UT, !A, !B),
    trie.foldl2(
        ( pred(K::in, V::in, A0::in, A1::out, B0::in, B1::out) is cc_multi :-
            Pred(anon(K), V, A0, A1, B0, B1)
        ), AT, !A, !B).
foldl2(Pred::in(pred(in, in, in, out, di, uo) is det),
        resource_trie(UT, AT)::in,
        !.A::in, !:A::out, !.B::di, !:B::uo) :-
    trie.foldl2(
        ( pred(K::in, V::in, A0::in, A1::out, B0::di, B1::uo) is det :-
            Pred(uri(K ^ uri), V, A0, A1, B0, B1)
        ), UT, !A, !B),
    trie.foldl2(
        ( pred(K::in, V::in, A0::in, A1::out, B0::di, B1::uo) is det :-
            Pred(anon(K), V, A0, A1, B0, B1)
        ), AT, !A, !B).
foldl2(Pred::in(pred(in, in, in, out, di, uo) is cc_multi),
        resource_trie(UT, AT)::in,
        !.A::in, !:A::out, !.B::di, !:B::uo) :-
    trie.foldl2(
        ( pred(K::in, V::in, A0::in, A1::out, B0::di, B1::uo) is cc_multi :-
            Pred(uri(K ^ uri), V, A0, A1, B0, B1)
        ), UT, !A, !B),
    trie.foldl2(
        ( pred(K::in, V::in, A0::in, A1::out, B0::di, B1::uo) is cc_multi :-
            Pred(anon(K), V, A0, A1, B0, B1)
        ), AT, !A, !B).

:- pragma promise_equivalent_clauses(foldl3/8).

foldl3(Pred::in(pred(in, in, in, out, in, out, in, out) is det),
        resource_trie(UT, AT)::in,
        !.A::in, !:A::out, !.B::in, !:B::out, !.C::in, !:C::out) :-
    trie.foldl3(
        ( pred(K::in, V::in, A0::in, A1::out, B0::in, B1::out, C0::in, C1::out)
                is det :-
            Pred(uri(K ^ uri), V, A0, A1, B0, B1, C0, C1)
        ), UT, !A, !B, !C),
    trie.foldl3(
        ( pred(K::in, V::in, A0::in, A1::out, B0::in, B1::out, C0::in, C1::out)
                is det :-
            Pred(anon(K), V, A0, A1, B0, B1, C0, C1)
        ), AT, !A, !B, !C).
foldl3(Pred::in(pred(in, in, in, out, in, out, in, out) is semidet),
        resource_trie(UT, AT)::in,
        !.A::in, !:A::out, !.B::in, !:B::out, !.C::in, !:C::out) :-
    trie.foldl3(
        ( pred(K::in, V::in, A0::in, A1::out, B0::in, B1::out, C0::in, C1::out)
                is semidet :-
            Pred(uri(K ^ uri), V, A0, A1, B0, B1, C0, C1)
        ), UT, !A, !B, !C),
    trie.foldl3(
        ( pred(K::in, V::in, A0::in, A1::out, B0::in, B1::out, C0::in, C1::out)
                is semidet :-
            Pred(anon(K), V, A0, A1, B0, B1, C0, C1)
        ), AT, !A, !B, !C).
foldl3(Pred::in(pred(in, in, in, out, in, out, in, out) is cc_multi),
        resource_trie(UT, AT)::in,
        !.A::in, !:A::out, !.B::in, !:B::out, !.C::in, !:C::out) :-
    trie.foldl3(
        ( pred(K::in, V::in, A0::in, A1::out, B0::in, B1::out, C0::in, C1::out)
                is cc_multi :-
            Pred(uri(K ^ uri), V, A0, A1, B0, B1, C0, C1)
        ), UT, !A, !B, !C),
    trie.foldl3(
        ( pred(K::in, V::in, A0::in, A1::out, B0::in, B1::out, C0::in, C1::out)
                is cc_multi :-
            Pred(anon(K), V, A0, A1, B0, B1, C0, C1)
        ), AT, !A, !B, !C).
foldl3(Pred::in(pred(in, in, in, out, in, out, di, uo) is det),
        resource_trie(UT, AT)::in,
        !.A::in, !:A::out, !.B::in, !:B::out, !.C::di, !:C::uo) :-
    trie.foldl3(
        ( pred(K::in, V::in, A0::in, A1::out, B0::in, B1::out, C0::di, C1::uo)
                is det :-
            Pred(uri(K ^ uri), V, A0, A1, B0, B1, C0, C1)
        ), UT, !A, !B, !C),
    trie.foldl3(
        ( pred(K::in, V::in, A0::in, A1::out, B0::in, B1::out, C0::di, C1::uo)
                is det :-
            Pred(anon(K), V, A0, A1, B0, B1, C0, C1)
        ), AT, !A, !B, !C).
foldl3(Pred::in(pred(in, in, in, out, in, out, di, uo) is cc_multi),
        resource_trie(UT, AT)::in,
        !.A::in, !:A::out, !.B::in, !:B::out, !.C::di, !:C::uo) :-
    trie.foldl3(
        ( pred(K::in, V::in, A0::in, A1::out, B0::in, B1::out, C0::di, C1::uo)
                is cc_multi :-
            Pred(uri(K ^ uri), V, A0, A1, B0, B1, C0, C1)
        ), UT, !A, !B, !C),
    trie.foldl3(
        ( pred(K::in, V::in, A0::in, A1::out, B0::in, B1::out, C0::di, C1::uo)
                is cc_multi :-
            Pred(anon(K), V, A0, A1, B0, B1, C0, C1)
        ), AT, !A, !B, !C).

:- pragma promise_equivalent_clauses(foldl_while/5).

foldl_while(Pred::in(pred(in, in, out, in, out) is det),
        resource_trie(UT, AT)::in, Complete::out, !.A::in, !:A::out) :-
    trie.foldl_while(
        ( pred(K::in, V::in, Continue0::out, A0::in, A1::out) is det :-
            Pred(uri(K ^ uri), V, Continue0, A0, A1)
        ), UT, Continue, !A),
    ( Continue = yes,
        trie.foldl_while(
            ( pred(K::in, V::in, Continue0::out, A0::in, A1::out) is det :-
                Pred(anon(K), V, Continue0, A0, A1)
            ), AT, Complete, !A)
    ; Continue = no,
        Complete = no
    ).
foldl_while(Pred::in(pred(in, in, out, di, uo) is det),
        resource_trie(UT, AT)::in, Complete::out, !.A::di, !:A::uo) :-
    trie.foldl_while(
        ( pred(K::in, V::in, Continue0::out, A0::di, A1::uo) is det :-
            Pred(uri(K ^ uri), V, Continue0, A0, A1)
        ), UT, Continue, !A),
    ( Continue = yes,
        trie.foldl_while(
            ( pred(K::in, V::in, Continue0::out, A0::di, A1::uo) is det :-
                Pred(anon(K), V, Continue0, A0, A1)
            ), AT, Complete, !A)
    ; Continue = no,
        Complete = no
    ).

:- pragma promise_equivalent_clauses(foldl2_while/7).

foldl2_while(Pred::in(pred(in, in, out, in, out, in, out) is det),
        resource_trie(UT, AT)::in, Complete::out,
        !.A::in, !:A::out, !.B::in, !:B::out) :-
    trie.foldl2_while(
        ( pred(K::in, V::in, Continue0::out, A0::in, A1::out, B0::in, B1::out) 
                is det :-
            Pred(uri(K ^ uri), V, Continue0, A0, A1, B0, B1)
        ), UT, Continue, !A, !B),
    ( Continue = yes,
        trie.foldl2_while(
            ( pred(K::in, V::in, Continue0::out, A0::in, A1::out,
                    B0::in, B1::out) is det :-
                Pred(anon(K), V, Continue0, A0, A1, B0, B1)
            ), AT, Complete, !A, !B)
    ; Continue = no,
        Complete = no
    ).
foldl2_while(Pred::in(pred(in, in, out, in, out, di, uo) is det),
        resource_trie(UT, AT)::in, Complete::out,
        !.A::in, !:A::out, !.B::di, !:B::uo) :-
    trie.foldl2_while(
        ( pred(K::in, V::in, Continue0::out, A0::in, A1::out, B0::di, B1::uo) 
                is det :-
            Pred(uri(K ^ uri), V, Continue0, A0, A1, B0, B1)
        ), UT, Continue, !A, !B),
    ( Continue = yes,
        trie.foldl2_while(
            ( pred(K::in, V::in, Continue0::out, A0::in, A1::out,
                    B0::di, B1::uo) is det :-
                Pred(anon(K), V, Continue0, A0, A1, B0, B1)
            ), AT, Complete, !A, !B)
    ; Continue = no,
        Complete = no
    ).

:- pragma promise_equivalent_clauses(foldl3_while/9).

foldl3_while(Pred::in(pred(in, in, out, in, out, in, out, in, out) is det),
        resource_trie(UT, AT)::in, Complete::out,
        !.A::in, !:A::out, !.B::in, !:B::out, !.C::in, !:C::out) :-
    trie.foldl3_while(
        ( pred(K::in, V::in, Continue0::out, A0::in, A1::out, B0::in, B1::out,
                C0::in, C1::out) is det :-
            Pred(uri(K ^ uri), V, Continue0, A0, A1, B0, B1, C0, C1)
        ), UT, Continue, !A, !B, !C),
    ( Continue = yes,
        trie.foldl3_while(
            ( pred(K::in, V::in, Continue0::out, A0::in, A1::out,
                    B0::in, B1::out, C0::in, C1::out) is det :-
                Pred(anon(K), V, Continue0, A0, A1, B0, B1, C0, C1)
            ), AT, Complete, !A, !B, !C)
    ; Continue = no,
        Complete = no
    ).
foldl3_while(Pred::in(pred(in, in, out, in, out, in, out, di, uo) is det),
        resource_trie(UT, AT)::in, Complete::out,
        !.A::in, !:A::out, !.B::in, !:B::out, !.C::di, !:C::uo) :-
    trie.foldl3_while(
        ( pred(K::in, V::in, Continue0::out, A0::in, A1::out, B0::in, B1::out,
                C0::di, C1::uo) is det :-
            Pred(uri(K ^ uri), V, Continue0, A0, A1, B0, B1, C0, C1)
        ), UT, Continue, !A, !B, !C),
    ( Continue = yes,
        trie.foldl3_while(
            ( pred(K::in, V::in, Continue0::out, A0::in, A1::out,
                    B0::in, B1::out, C0::di, C1::uo) is det :-
                Pred(anon(K), V, Continue0, A0, A1, B0, B1, C0, C1)
            ), AT, Complete, !A, !B, !C)
    ; Continue = no,
        Complete = no
    ).

%-----------------------------------------------------------------------------%
:- end_module resource_trie.
%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et fileencoding=utf8
% -*- coding:utf8; -*-
