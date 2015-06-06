%-----------------------------------------------------------------------------%
:- module cache.
% Copyright (c) Mission Critical IT
% MIT License
%
% This module implements a mechanism for accessing a hidden cache.  This
% is useful if you want to cache results while runing nondet code, or from
% within a function call.
%
% To use this module you would first define your cache type.  Then you
% would make it an instance of the cache typeclass.  Then you would write
% query predicates that use the cache as an explicit argument, e.g:
%
%   myquery(Snapshot, Query, Result, !Cache)
%
% To run the predicate where the cache cannot be threaded through (e.g.
% from within a function, or nondet code), one would call:
%
%   run_query_with_cache(myquery(Snapshot, Query), Result)
%
% One can also define a predicate to clean up the cache and call it using
% cleanup_cache, e.g.:
%
%   cleanup_cache(delete_old_cache_entries, !IO)
%
% NOTE:
%   To ensure sound behaviour, the following guidelines should be followed:
%
%   - Query predicates should always generate the same results for the same
%     inputs (ignoring the cache arguments).  In the example above,
%     myquery(Snapshot, Query, Result, Cache0, Cache) should always generate
%     the same Result for a given Snapshot and Query, no matter what the value
%     of Cache0.  This can be done by using abstract types to ensure that
%     nothing besides myquery can add new entries to the cache, and ensuring
%     that any cache entries are always indexed on *all* inputs.
%     To index on large data structures (such as a snapshot), one
%     can use a fast_loose_id.  A fast_loose_id is a value based on the
%     type and pointer value of a term.  It is fast, because working out the
%     pointer value and type of a term is quick.  It is loose, because
%     two semantically equivalent values may have different pointer values.
%
%   - Cache cleanup predicates should only delete entries or modify
%     bookkeeping infomation.  New entries should never be added to the
%     cache by a cleanup predicate.
%
%-----------------------------------------------------------------------------%

:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- typeclass cache(C) where [
    func init = C
].

:- pred run_query_with_cache(pred(T, C, C), T) <= cache(C).
:- mode run_query_with_cache(pred(out, in, out) is det, out) is det.
:- mode run_query_with_cache(pred(out, in, out) is cc_multi, out) is cc_multi.

:- pred cleanup_cache(pred(C, C)::in(pred(in, out) is det), io::di, io::uo)
    is det <= cache(C).

%-----------------------------------------------------------------------------%

:- type fast_loose_id.

    % If two fast_loose_id's are equivalent, then the terms those
    % fast_loose_ids were derived from are equivalent.  The converse is
    % however not true, which is why this predicate is cc_multi and not det.
    %
:- pred get_fast_loose_id(T::in, fast_loose_id::out) is cc_multi.

:- pred write_fast_loose_id(fast_loose_id::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module require.
:- import_module string.
:- import_module type_desc.
:- import_module univ.

%-----------------------------------------------------------------------------%

:- mutable(caches, map(type_desc, univ), map.init, ground,
    [untrailed, attach_to_io_state]).

:- type ptr
    --->    ptr(int).

:- pragma foreign_type("Java", ptr, "Object")
    where equality is ptr_equal.
    % 10.04 compiler does not recognise this
:- pragma foreign_type("C#", ptr, "object")
    where equality is ptr_equal.

:- type fast_loose_id
    --->    fast_loose_id(
                ptr,
                type_desc
            ).

run_query_with_cache(Pred:pred(T, C, C), Output) :-
    %
    % This is pure because we should always get the same results
    % irrespective of what is in the cache, assuming the user has
    % followed the guidelines listed at the top of this module.
    %
    promise_pure (
        semipure get_caches(Caches0),
        Id = type_of(_:C),
        ( map.search(Caches0, Id, Univ) ->
            % The cache should always be of the correct type, since we
            % index by the type_desc.
            det_univ_to_type(Univ, Cache0:C)
        ;
            Cache0 = cache.init
        ),
        Pred(Output, Cache0, Cache),
        map.set(Id, univ(Cache), Caches0, Caches),
        impure set_caches(Caches)
    ).

cleanup_cache(CleanupPred, !IO) :-
    get_caches(Caches0, !IO),
    Id = type_of(_:C),
    ( map.search(Caches0, Id, Univ) ->
        % The cache should always be of the correct type, since we
        % index by the type_desc.
        det_univ_to_type(Univ, Cache0:C)
    ;
        Cache0 = cache.init
    ),
    CleanupPred(Cache0, Cache),
    map.set(Id, univ(Cache), Caches0, Caches),
    set_caches(Caches, !IO).

get_fast_loose_id(X, Id) :-
    Id = fast_loose_id(Ptr, type_of(X)),
    get_pointer(X, Ptr).

:- pred get_pointer(T::in, ptr::out) is cc_multi.

:- pragma foreign_proc("C", get_pointer(T::in, Ptr::out),
    [will_not_call_mercury, promise_pure, thread_safe, terminates],
"
    /* This depends on ptr being a no-tag type. */
    Ptr = (MR_Integer)T;
").

:- pragma foreign_proc("Java", get_pointer(T::in, Ptr::out),
    [will_not_call_mercury, promise_pure, thread_safe, terminates],
"
    Ptr = (Object)T;
").

:- pragma foreign_proc("C#", get_pointer(T::in, Ptr::out),
   [will_not_call_mercury, promise_pure, thread_safe, terminates],
"
    Ptr = (object)T;
").

:- pred ptr_equal(ptr::in, ptr::in) is semidet.

ptr_equal(_, _) :-
    ( semidet_true ->
        error("ptr_equal must be overridden")
    ;
        semidet_fail
    ).

:- pragma foreign_proc("Java",
    ptr_equal(PtrA::in, PtrB::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (PtrA == PtrB);
").

:- pragma foreign_proc("C#",
    ptr_equal(PtrA::in, PtrB::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (PtrA == PtrB);
").

write_fast_loose_id(fast_loose_id(Ptr, TypeDesc), !IO) :-
    io.write_string("fast_loose_id(", !IO),
    get_ptr_string(Ptr, PtrStr),
    io.write_string(PtrStr, !IO),
    io.write_string(", ", !IO),
    io.write(TypeDesc, !IO),
    io.write_string(")", !IO).


:- pred get_ptr_string(ptr::in, string::out) is det.

get_ptr_string(Ptr, string(Ptr)).

:- pragma foreign_proc("Java",
    get_ptr_string(Ptr::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Ptr != null) {
        Str = Ptr.toString();
    } else {
        Str = ""null"";
    }
").

:- pragma foreign_proc("C#",
    get_ptr_string(Ptr::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Ptr != null) {
        Str = Ptr.ToString();
    } else {
        Str = ""null"";
    }
").

%-----------------------------------------------------------------------------%
:- end_module cache.
%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et fileencoding=utf8
% -*- coding:utf8; -*-
