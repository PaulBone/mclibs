% An alternative implementation of BDDs
% Author: llg
%
% Each BDD node is registered by an integer. The correspondance between nodes and integer
% is stored in a bimap (see bdds type). The bimap is stored in a hidden mutable.
%
% It is NOT SOUND to reset this mutable, to serialize/unserialize BDDs outside the scope of
% an execution. To do so, use the other implementation of BDDs (see bdd module).
%
% - Two boolean expressions are equivalent if and only if their BDDs are the same.
% - Nodes are shared accross all the BDDs of an execution.
%
% Atoms are also registered as integers. Correspondance between the atom of a given type and
% integers is stored in a bimap (see atom_register type)
%
% if Atom1 < Atom2 then Atom1 is _before_ Atom2 in the BDD.
%
% The following operations are natively supported:
%   - conjunction
%   - disjunction
%   - exclusive disjunction
%   - negation
%   - entailment test
%
:- module bdd_alt.

:- interface.

:- import_module bool.
:- import_module io.
:- import_module set.

:- import_module boolean_expression.

:- type bdd.

:- func false_bdd = bdd.
:- func true_bdd = bdd.

:- pred is_satisfiable(bdd::in) is semidet.

:- func bdd(boolean_expression(Atom)) = bdd.

:- func bdd_to_dnf(bdd::in) = (boolean_expression(Atom)::out(dnf)) is det.

:- func bdd `and` bdd = bdd.
:- func bdd `or` bdd = bdd.
:- func bdd `xor` bdd = bdd.
:- func not(bdd) = bdd.

:- func evaluate(bdd, valuation(Atom)) = bool.

:- pred (bdd::in) `entails` (bdd::in) is semidet.

:- func get_vars(bdd) = set(Atom).

:- pred print_bdd_stats(io::di, io::uo) is det.

:- implementation.

:- import_module array.
:- import_module bimap.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module hash_table.
:- import_module pair.
:- import_module require.
:- import_module type_desc.
:- import_module univ.

:- import_module cache.

:- type bdds
    ---> bdds(
              bdds_reg    :: hash_table(bdd_node, bdd_id),
              bdd_pool    :: array(bdd_node),
              registers   :: map(type_desc, univ),
              next_id     :: bdd_id,
                  %
                  % Query caching (one cache per type of operation we cache)
                  %
              cache_and   :: hash_table(pair(int, int), bdd), %map(pair(bdd_id, bdd_id), bdd),
              cache_or    :: map(pair(bdd_id, bdd_id), bdd),
              cache_xor   :: map(pair(bdd_id, bdd_id), bdd),
              cache_ent   :: map(pair(bdd_id, bdd_id), bool)
             ).

print_bdd_stats(!IO) :-
    run_query_with_cache(pred(BDD::out, BDD::in, BDD::out) is det :- true, BDDStruct),
    print_bdd_stats(BDDStruct, !IO).

:- pred print_bdd_stats(bdds::in, io::di, io::uo) is det.

print_bdd_stats(BDDs, !IO) :-
    io.write_string("Statistics about BDD implementation " ++ $module ++ "\n", !IO),
    io.format("Number of BDD nodes:\t%d\t\n", [i(BDDs ^ next_id ^ bdd_id)], !IO),
    hash_table_inst(BDDs ^ cache_and, AndCache),
    io.format("Number of AND results cached:\t%d\t\n", [i(hash_table.num_occupants(AndCache))], !IO),
    io.format("Number of Entailment results cached:\t%d\t\n", [i(map.count(BDDs ^ cache_ent))], !IO),
    io.format("Number of OR results cached:\t%d\t\n", [i(map.count(BDDs ^ cache_or))], !IO),
    io.format("Number of XOR results cached:\t%d\t\n", [i(map.count(BDDs ^ cache_xor))], !IO).

BDD1 `and` BDD2 = BDD3 :-
    run_query_with_cache(apply_and(BDD1, BDD2), BDD3).

BDD1 `or` BDD2 = BDD3 :-
    run_query_with_cache(apply_or(BDD1, BDD2), BDD3).

BDD1 `xor` BDD2 = BDD3 :-
    run_query_with_cache(apply_xor(BDD1, BDD2), BDD3).

not(BDD) = NotBDD :-
    run_query_with_cache(apply_not(BDD), NotBDD).

BDD1 `entails` BDD2 :-
    run_query_with_cache(
        (pred(Res::out, !.BDDs::in, !:BDDs::out) is det :-
            entailment_test(BDD1, BDD2, Res, !BDDs)
        ), yes).

bdd(BooleanExpression) = BDD :-
    cache.run_query_with_cache(bdd(BooleanExpression), BDD).

evaluate(BDD, Valuation) = BooleanResult :-
    run_query_with_cache(
        (pred(Res::out, BDDs::in, BDDs::out) is det :-
            evaluate(BDD, Valuation, get_atom_register(BDDs), BDDs, Res)
        ), BooleanResult).

get_vars(BDD) = Atoms :-
    run_query_with_cache(
        (pred(As::out, BDDs::in, BDDs::out) is det :-
            AtomRegister : atom_register(Atom) = get_atom_register(BDDs),
            As = get_vars(BDD, AtomRegister, BDDs)
        ), Atoms : set(Atom)).

:- func get_vars(bdd, atom_register(Atom), bdds) = set(Atom).

get_vars(true, _, _) = set.init.
get_vars(false, _, _) = set.init.
get_vars(bddite(BDDNode), Register, BDDs) = set.insert(OtherAtoms, bimap.lookup(Register ^ atom_map, BDD ^ atom)) :-
    BDD = get_bdd_node(BDDs, BDDNode),
    OtherAtoms = set.union(get_vars(BDD ^ high, Register, BDDs), get_vars(BDD ^ low, Register, BDDs)).

:- pred evaluate(bdd::in, valuation(Atom)::in, atom_register(Atom)::in, bdds::in, bool::out) is det.

evaluate(true, _, _, _, yes).
evaluate(false, _, _, _, no).
evaluate(bddite(BDDid), Valuation, Register, BDDs, Res) :-
    BDDNode = get_bdd_node(BDDs, BDDid),
    Atom = bimap.lookup(Register ^ atom_map, BDDNode ^ atom),
    AtomValue =  Valuation ^ det_elem(Atom),
    Branch = (if AtomValue = yes then BDDNode ^ high else BDDNode ^ low),
    evaluate(Branch, Valuation, Register, BDDs, Res).

:- pred bdd(boolean_expression(Var)::in, bdd::out, bdds::in, bdds::out) is det.

bdd(BooleanExpression, BDD, !BDDs) :-
    AtomRegister0 = get_atom_register(!.BDDs),
    list.foldl(register_atom, set.to_sorted_list(get_vars(BooleanExpression)), AtomRegister0, AtomRegister),
    set_atom_register(AtomRegister, !BDDs),
    build_bdd_from_boolean_expression(AtomRegister, BooleanExpression, BDD, !BDDs).

:- pred build_bdd_from_boolean_expression(atom_register(Atom)::in, boolean_expression(Atom)::in, bdd::out, bdds::in, bdds::out) is det.

build_bdd_from_boolean_expression(AtomRegister, var(Atom), BDD, !BDDs) :-
    AtomId = bimap.reverse_lookup(AtomRegister ^ atom_map, Atom),
    register_bdd_node(AtomId, true, false, BDD, !BDDs).
build_bdd_from_boolean_expression(AtomRegister, not(A), BDD, !BDDs) :-
    build_bdd_from_boolean_expression(AtomRegister, A, BDD_A, !BDDs),
    apply_not(BDD_A, BDD, !BDDs).
build_bdd_from_boolean_expression(AtomRegister, and(A, B), BDD, !BDDs) :-
    build_bdd_from_boolean_expression(AtomRegister, A, BDD_A, !BDDs),
    build_bdd_from_boolean_expression(AtomRegister, B, BDD_B, !BDDs),
    apply_and(BDD_A, BDD_B, BDD, !BDDs).
build_bdd_from_boolean_expression(AtomRegister, or(A, B), BDD, !BDDs) :-
    build_bdd_from_boolean_expression(AtomRegister, A, BDD_A, !BDDs),
    build_bdd_from_boolean_expression(AtomRegister, B, BDD_B, !BDDs),
    apply_or(BDD_A, BDD_B, BDD, !BDDs).
build_bdd_from_boolean_expression(AtomRegister, xor(A, B), BDD, !BDDs) :-
    build_bdd_from_boolean_expression(AtomRegister, A, BDD_A, !BDDs),
    build_bdd_from_boolean_expression(AtomRegister, B, BDD_B, !BDDs),
    apply_xor(BDD_A, BDD_B, BDD, !BDDs).
build_bdd_from_boolean_expression(_AtomRegister, true, true, !BDDs).
build_bdd_from_boolean_expression(_AtomRegister, false, false, !BDDs).

:- type atom_register(Atom)
    ---> atom_register(atom_map :: bimap(var_id, Atom)).

:- instance cache.cache(atom_register(Atom)) where [
        init = atom_register(bimap.init)
    ].

:- func bdd_pool_initial_size = int.

bdd_pool_initial_size = 10000000.

:- func dummy_bdd_node = bdd_node.

dummy_bdd_node = bdd_node(var_id(-1), true, true).

:- instance cache.cache(bdds) where [
        init = bdds(hash_table.new(bdd_node_hash, 16, 0.7), array.init(bdd_pool_initial_size, dummy_bdd_node), init, bdd_id(0), bdd_cache_init, map.init, map.init, map.init)
    ].

:- pred register_atom(Atom::in, atom_register(Atom)::in, atom_register(Atom)::out) is det.

register_atom(Atom, !AtomRegister) :-
    ( bimap.reverse_search(!.AtomRegister ^ atom_map, _, Atom) ->
        true
    ;
        bimap.det_insert(!.AtomRegister ^ atom_map, var_id(map.count(bimap.forward_map(!.AtomRegister ^ atom_map))), Atom, !:AtomRegister ^ atom_map)
    ).

:- type bdd_id
    ---> bdd_id(bdd_id :: int).

:- type var_id
    ---> var_id(var_id :: int).

:- type bdd
    ---> true
    ;    false
    ;    bddite(bdd_id)
    .

false_bdd = bdd_alt.false.
true_bdd = bdd_alt.true.

is_satisfiable(true).
is_satisfiable(bddite(_)).

:- type bdd_node
    ---> bdd_node(
                  atom :: var_id,
                  high :: bdd,
                  low  :: bdd
                 ).

:- import_module int.

:- pred bdd_node_hash(bdd_node::in, int::out) is det.

bdd_node_hash(bdd_node(VarId, BDD1, BDD2), (VarIdHash * 0x49249249)  `xor` (ID1 * 0x92492492) `xor` (ID2 * 0x24924924) ) :-
    int_hash(VarId ^ var_id, VarIdHash),
    ( BDD1 = true,
        ID1 = -1
    ; BDD1 = false,
        ID1 = -2
    ; BDD1 = bddite(bdd_id(ID1))
    ),
    ( BDD2 = true,
        ID2 = -1
    ; BDD2 = false,
        ID2 = -2
    ; BDD2 = bddite(bdd_id(ID2))
    ).

:- pred apply_and(bdd::in, bdd::in, bdd::out, bdds::in, bdds::out) is det.

apply_and(false, _, false, !BDDs).
apply_and(true, BDD, BDD, !BDDs).
apply_and(bddite(_) @ BDD, true, BDD, !BDDs).
apply_and(bddite(_), false, false, !BDDs).
apply_and(bddite(BDDid_a), bddite(BDDid_b), ResultingBDD, !BDDs) :-
    normalized_bddid_order(BDDid_a, BDDid_b, BDDid1, BDDid2),
    ( BDDid1 = BDDid2 ->
        ResultingBDD = bddite(BDDid1)
    ;
      search_in_bdd_cache(!.BDDs ^ cache_and, BDDid1 ^ bdd_id, BDDid2 ^ bdd_id, CachedResult) ->
        ResultingBDD = CachedResult
    ;
        BDD1 = get_bdd_node(!.BDDs, BDDid1),
        BDD2 = get_bdd_node(!.BDDs, BDDid2),
        compare(CompareRes, BDD1 ^ atom, BDD2 ^ atom),
        ( CompareRes = (=),
            Var = BDD1 ^ atom,
            apply_and(BDD1 ^ high, BDD2 ^ high, High, !BDDs),
            apply_and(BDD1 ^ low, BDD2 ^ low, Low, !BDDs)
        ; CompareRes = (<),
            Var = BDD1 ^ atom,
            apply_and(BDD1 ^ high, bddite(BDDid2), High, !BDDs),
            apply_and(BDD1 ^ low, bddite(BDDid2), Low, !BDDs)
        ; CompareRes = (>),
            Var = BDD2 ^ atom,
            apply_and(bddite(BDDid1), BDD2 ^ high, High, !BDDs),
            apply_and(bddite(BDDid1), BDD2 ^ low, Low, !BDDs)
        ),
        register_bdd_node(Var, High, Low, ResultingBDD, !BDDs),
        insert_to_bdd_cache(!.BDDs ^ cache_and, BDDid1 ^ bdd_id, BDDid2 ^ bdd_id, ResultingBDD, UpdatedCache),
        !BDDs ^ cache_and := UpdatedCache
    ).

:- pred apply_or(bdd::in, bdd::in, bdd::out, bdds::in, bdds::out) is det.

apply_or(false, BDD, BDD, !BDDs).
apply_or(true, _, true, !BDDs).
apply_or(bddite(_), true, true, !BDDs).
apply_or(bddite(_) @ BDD, false, BDD, !BDDs).
apply_or(bddite(BDDid_a), bddite(BDDid_b), ResultingBDD, !BDDs) :-
    normalized_bddid_order(BDDid_a, BDDid_b, BDDid1, BDDid2),
    ( map.search(!.BDDs ^ cache_or, BDDid1 - BDDid2, CachedResult) ->
        ResultingBDD = CachedResult
    ;
      BDDid1 = BDDid2 ->
        ResultingBDD = bddite(BDDid1)
    ;
        BDD1 = get_bdd_node(!.BDDs, BDDid1),
        BDD2 = get_bdd_node(!.BDDs, BDDid2),
        compare(CompareRes, BDD1 ^ atom, BDD2 ^ atom),
        ( CompareRes = (=),
            Var = BDD1 ^ atom,
            apply_or(BDD1 ^ high, BDD2 ^ high, High, !BDDs),
            apply_or(BDD1 ^ low, BDD2 ^ low, Low, !BDDs)
        ; CompareRes = (<),
            Var = BDD1 ^ atom,
            apply_or(BDD1 ^ high, bddite(BDDid2), High, !BDDs),
            apply_or(BDD1 ^ low, bddite(BDDid2), Low, !BDDs)
        ; CompareRes = (>),
            Var = BDD2 ^ atom,
            apply_or(bddite(BDDid1), BDD2 ^ high, High, !BDDs),
            apply_or(bddite(BDDid1), BDD2 ^ low, Low, !BDDs)
        ),
        register_bdd_node(Var, High, Low, ResultingBDD, !BDDs),
        map.det_insert(!.BDDs ^ cache_or, BDDid1 - BDDid2, ResultingBDD, UpdatedCache),
        !BDDs ^ cache_or := UpdatedCache
    ).
    

:- pred apply_xor(bdd::in, bdd::in, bdd::out, bdds::in, bdds::out) is det.

apply_xor(false, BDD, BDD, !BDDs).
apply_xor(true, BDD, NotBDD, !BDDs) :-
    apply_not(BDD, NotBDD, !BDDs).
apply_xor(bddite(_) @ BDD, true, NotBDD, !BDDs) :-
    apply_not(BDD, NotBDD, !BDDs).
apply_xor(bddite(_) @ BDD, false, BDD, !BDDs).
apply_xor(bddite(BDDid_a), bddite(BDDid_b), ResultingBDD, !BDDs) :-
    normalized_bddid_order(BDDid_a, BDDid_b, BDDid1, BDDid2),
    ( map.search(!.BDDs ^ cache_xor, BDDid1 - BDDid2, CachedResult) ->
        ResultingBDD = CachedResult
    ;
      BDDid1 = BDDid2 ->
        ResultingBDD = false
    ;
        BDD1 = get_bdd_node(!.BDDs, BDDid1),
        BDD2 = get_bdd_node(!.BDDs, BDDid2),
        compare(CompareRes, BDD1 ^ atom, BDD2 ^ atom),
        ( CompareRes = (=),
            Var = BDD1 ^ atom,
            apply_xor(BDD1 ^ high, BDD2 ^ high, High, !BDDs),
            apply_xor(BDD1 ^ low, BDD2 ^ low, Low, !BDDs)
        ; CompareRes = (<),
            Var = BDD1 ^ atom,
            apply_xor(BDD1 ^ high, bddite(BDDid2), High, !BDDs),
            apply_xor(BDD1 ^ low, bddite(BDDid2), Low, !BDDs)
        ; CompareRes = (>),
            Var = BDD2 ^ atom,
            apply_xor(bddite(BDDid1), BDD2 ^ high, High, !BDDs),
            apply_xor(bddite(BDDid1), BDD2 ^ low, Low, !BDDs)
        ),
        register_bdd_node(Var, High, Low, ResultingBDD, !BDDs),
        map.det_insert(!.BDDs ^ cache_xor, BDDid1 - BDDid2, ResultingBDD, UpdatedCache),
        !BDDs ^ cache_xor := UpdatedCache
    ).

    %
    % Res = yes <=> ( BDD1 and not(BDD2) is unsatisfiable )
    %
:- pred entailment_test(bdd::in, bdd::in, bool::out, bdds::in, bdds::out) is det.
:- import_module io, string.
entailment_test(BDD1, BDD2, Res, !BDDs) :-
    entailment_test_aux(BDD1, BDD2, Res, !BDDs).

:- pred entailment_test_aux(bdd::in, bdd::in, bool::out, bdds::in, bdds::out) is det.

entailment_test_aux(false,             _, yes, !BDDs).
entailment_test_aux(true, HasToBeTrue, (if HasToBeTrue = true then yes else no), !BDDs).
entailment_test_aux(bddite(_),      true, yes, !BDDs).
entailment_test_aux(bddite(_),     false, no, !BDDs).
entailment_test_aux(BDD1 @ bddite(BDDid1), BDD2 @ bddite(BDDid2), Res, !BDDs) :-
    ( BDD1 = BDD2 ->
        Res = yes
    ;
      map.search(!.BDDs ^ cache_ent, BDDid1 - BDDid2, Res1) ->
        Res = Res1
    ;
        BDDNode1 = get_bdd_node(!.BDDs, BDDid1),
        BDDNode2 = get_bdd_node(!.BDDs, BDDid2),
        compare(AtomComparison, BDDNode1 ^ atom, BDDNode2 ^ atom),
        ( AtomComparison = (=),
            entailment_test_aux(BDDNode1 ^ high, BDDNode2 ^ high, ResHigh, !BDDs),
            ( ResHigh = yes,
                entailment_test_aux(BDDNode1 ^ high, BDDNode2 ^ high, Res, !BDDs)
            ; ResHigh = no,
                Res = no
            )
        ; AtomComparison = (>),
            Res = no
        ; AtomComparison = (<),
            entailment_test_aux(BDDNode1 ^ high, BDD2, ResHigh, !BDDs),
            ( ResHigh = yes,
                entailment_test_aux(BDDNode1 ^ low, BDD2, Res, !BDDs)
            ; ResHigh = no,
                Res = no
            )
        ),
        map.det_insert(!.BDDs ^ cache_ent, BDDid1 - BDDid2, Res, UpdatedCache),
        !BDDs ^ cache_ent := UpdatedCache
    ).

:- pred apply_not(bdd::in, bdd::out, bdds::in, bdds::out) is det.

apply_not(true, false, !BDDs).
apply_not(false, true, !BDDs).
apply_not(bddite(BDDid), NotBDD, !BDDs) :-
    BDD = get_bdd_node(!.BDDs, BDDid),
    apply_not(BDD ^ high, NotHigh, !BDDs),
    apply_not(BDD ^ low, NotLow, !BDDs),
    register_bdd_node(BDD ^ atom, NotHigh, NotLow, NotBDD, !BDDs).

:- pred normalized_bddid_order(bdd_id::in, bdd_id::in, bdd_id::out, bdd_id::out) is det.

normalized_bddid_order(BDDa, BDDb, BDD1, BDD2) :-
    ( BDDb ^ bdd_id < BDDa ^ bdd_id ->
        BDD1 = BDDb,
        BDD2 = BDDa
    ;
        BDD1 = BDDa,
        BDD2 = BDDb
    ).

:- func get_bdd_node(bdds, bdd_id) = bdd_node.

get_bdd_node(BDDs, bdd_id(BDDid)) = Node :-
    Node = array.lookup(BDDs ^ bdd_pool, BDDid).

:- pred register_bdd_node(var_id::in, bdd::in, bdd::in, bdd::out, bdds::in, bdds::out) is det.

register_bdd_node(Var, High, Low, BDD, !BDDs) :-
    ( High = Low ->
        BDD = High
    ;
        BDDNode = bdd_node(Var, High, Low),
        hash_table_inst(!.BDDs ^ bdds_reg, HashTable),
        ( hash_table.search(/*!.BDDs ^ bdds_reg*/ HashTable, BDDNode, ExistingBDDId) ->
            BDDNodeId = ExistingBDDId
        ;
            BDDNodeId = !.BDDs ^ next_id,
            !BDDs ^ next_id := bdd_id((BDDNodeId ^ bdd_id) + 1),
            array.set(!.BDDs ^ bdd_pool, BDDNodeId ^ bdd_id, BDDNode, UpdatedBDDPool),
            hash_table.det_insert(BDDNode, BDDNodeId, HashTable, UpdatedBDDMap),
            !BDDs ^ bdd_pool := UpdatedBDDPool,
            ground_hash_table_inst(UpdatedBDDMap, GroundUpdatedBDDMap),
            !BDDs ^ bdds_reg := GroundUpdatedBDDMap
        ),
        BDD = bddite(BDDNodeId)
    ).

:- pred search_in_bdd_cache(hash_table(pair(int, int), V)::in, int::in, int::in, V::out) is semidet.

search_in_bdd_cache(GroundHashTable, Id1, Id2, V) :-
    hash_table_inst(GroundHashTable, HashTable),
    hash_table.search(HashTable, Id1 - Id2, V).

:- pred insert_to_bdd_cache(hash_table(pair(int, int), V)::in, int::in, int::in, V::in, hash_table(pair(int, int), V)::out) is det.

insert_to_bdd_cache(!.GroundHashTable, Id1, Id2, V, !:GroundHashTable) :-
    hash_table_inst(!.GroundHashTable, HashTable0),
    hash_table.det_insert(Id1 - Id2, V, HashTable0, HashTable),
    ground_hash_table_inst(HashTable, !:GroundHashTable).

:- func bdd_cache_init = hash_table(pair(int, int), V).

bdd_cache_init = GroundHashTable :-
    ground_hash_table_inst(hash_table.new((pred(I1 - I2::in, ((I1 * 0x92492492) `xor` (I2 * 0x24924924))::out) is det), 16, 0.7), GroundHashTable).

:- pred hash_table_inst(hash_table(K, V)::in, hash_table(K, V)::hash_table_uo) is det.

:- pragma foreign_proc("C",
    hash_table_inst(A::in, B::hash_table_uo),
    [promise_pure, will_not_call_mercury, tabled_for_io, may_not_duplicate],
"
    B = A;
").

:- pred ground_hash_table_inst(hash_table(K, V)::hash_table_ui, hash_table(K, V)::out) is det.

:- pragma foreign_proc("C",
    ground_hash_table_inst(A::hash_table_ui, B::out),
    [promise_pure, will_not_call_mercury, tabled_for_io, may_not_duplicate],
"
    B = A;
").

:- func get_atom_register(bdds) = atom_register(Atom).

get_atom_register(BDDs) = AtomRegister:AR :-
    Id = type_of(_:AR),
    ( map.search(BDDs ^ registers, Id, Univ) ->
        det_univ_to_type(Univ, AtomRegister:AR)
    ;
        AtomRegister = cache.init
    ).

:- pred set_atom_register(atom_register(Atom)::in, bdds::in, bdds::out) is det.

set_atom_register(Register, !BDDs) :-
    Id = type_of(Register),
    map.set(!.BDDs ^ registers, Id, univ(Register), UpdatedRegisters),
    !BDDs ^ registers := UpdatedRegisters.

bdd_to_dnf(BDD) = DNF : boolean_expression(Atom) :-
    run_query_with_cache(
        (pred(F::out, BDDs::in, BDDs::out) is det :-
            F : boolean_expression(Atom) = bdd_to_dnf(BDDs, BDD)
        ), DNF).

:- func bdd_to_dnf(bdds, bdd) = boolean_expression(Atom).

bdd_to_dnf(BDDs, BDD) = DNF : boolean_expression(Atom) :-
    bdd_to_dnf_aux(BDDs, BDD) = AtomsLists : list(list(boolean_expression(Atom))),
    Conjunctions : list(boolean_expression(Atom)) = list.map(atoms_list_to_conjunction, AtomsLists),
    DNF = conjunctions_to_disjunction(Conjunctions).

:- func bdd_to_dnf_aux(bdds, bdd) = list(list(boolean_expression(Atom))).
bdd_to_dnf_aux(_, true) = [[]].
bdd_to_dnf_aux(_, false) = [].
bdd_to_dnf_aux(BDDs, bddite(NodeId)) = (Var_And_A ++ NotVar_And_B) : list(list(boolean_expression(AtomType))) :-
    Node = get_bdd_node(BDDs, NodeId),
    Atom : AtomType = lookup(get_atom_register(BDDs) ^ atom_map, Node ^ atom),
    Var_And_A = list.map(func(C) = [var(Atom) | C], bdd_to_dnf_aux(BDDs, Node ^ high)),
    NotVar_And_B = list.map(func(C) = [not(var(Atom)) | C], bdd_to_dnf_aux(BDDs, Node ^ low)).

:- func atoms_list_to_conjunction(list(boolean_expression(Atom))) = boolean_expression(Atom).

atoms_list_to_conjunction([]) = true.
atoms_list_to_conjunction([A]) = A.
atoms_list_to_conjunction([A, B | As]) = and(A, atoms_list_to_conjunction([B | As])).

:- func conjunctions_to_disjunction(list(boolean_expression(Atom))) = boolean_expression(Atom).

conjunctions_to_disjunction([]) = false.
conjunctions_to_disjunction([A]) = A.
conjunctions_to_disjunction([A, B | As]) = or(A, conjunctions_to_disjunction([B | As])).

:- end_module bdd_alt.
