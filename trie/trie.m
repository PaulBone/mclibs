%-----------------------------------------------------------------------------%
:- module trie.
% This module implements tries.  A trie is a fast, string-indexed map.
% Although tries are implemented in foreign code, they are not destructively
% updated, so it is safe to refer to old versions of tries.
%-----------------------------------------------------------------------------%

:- interface.

:- import_module pickle.

:- import_module bool.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type trie(V).

:- func init = trie(V).

    % Search for a key and return the corresponding value if it is found.
    %
:- pred search(trie(V)::in, string::in, V::out) is semidet.

    % Find the shortest key that is a prefix of the given string and
    % return the key and associated value.  Fails if there are no prefixes of
    % the string in the trie.
    % If the empty string is a key in the trie, then it will always be
    % returned, because the empty string is a prefix of any string.
    %
:- pred search_shortest_prefix(trie(V)::in, string::in, string::out, V::out)
    is semidet.

    % search_shortest_prefix_with_min(Trie, Key, Min, Prefix, Value).
    % Same as above, but with the added constraint that length(Prefix) >= Min.
    %
:- pred search_shortest_prefix_with_min(trie(V)::in, string::in, int::in,
    string::out, V::out) is semidet.

    % Replace a value if the key exists, otherwise insert the key-value.
    %
:- pred set(string::in, V::in, trie(V)::in, trie(V)::out) is det.

    % Delete a key.  If the key is not found, the trie is unchanged.
    %
:- pred delete(string::in, trie(V)::in, trie(V)::out) is det.

    % Return the number of keys in the trie.
    %
:- func count(trie(V)) = int.

    % Succeed if the trie is empty.
    %
:- pred is_empty(trie(V)::in) is semidet.

    % Traverse the key-values pairs in the trie, applying an accumulator.
    % The nodes are traversed in the order of the keys.
    %
:- pred foldl(pred(string, V, A, A), trie(V), A, A).
:- mode foldl(in(pred(in, in, in, out) is det), in, in, out) is det.
:- mode foldl(in(pred(in, in, in, out) is semidet), in, in, out) is semidet.
:- mode foldl(in(pred(in, in, di, uo) is det), in, di, uo) is det.
:- mode foldl(in(pred(in, in, in, out) is cc_multi), in, in, out)
    is cc_multi.
:- mode foldl(in(pred(in, in, di, uo) is cc_multi), in, di, uo) is cc_multi.

    % Same as foldl, but with 2 accumulators.
    %
:- pred foldl2(pred(string, V, A, A, B, B), trie(V), A, A, B, B).
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

    % Same as foldl, but with 3 accumulators.
    %
:- pred foldl3(pred(string, V, A, A, B, B, C, C), trie(V), A, A, B, B, C, C).
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

    % foldl_while(Pred, Trie, Complete, !A)
    % foldl_while is similar to foldl, except that the closure returns an extra
    % argument of type bool.  If the closure returns no for this argument then
    % the traversal terminates and the value of the accumulator just after
    % the last call to the closure is returned.  Complete will be 'yes' if
    % Pred never returned 'no'.
    %
:- pred foldl_while(pred(string, V, bool, A, A), trie(V), bool, A, A).
:- mode foldl_while(in(pred(in, in, out, in, out) is det), in, out, in, out)
    is det.
:- mode foldl_while(in(pred(in, in, out, di, uo) is det), in, out, di, uo)
    is det.

    % Same as foldl_while, but with 2 accumulators.
    %
:- pred foldl2_while(pred(string, V, bool, A, A, B, B), trie(V), bool,
    A, A, B, B).
:- mode foldl2_while(in(pred(in, in, out, in, out, in, out) is det), in,
    out, in, out, in, out) is det.
:- mode foldl2_while(in(pred(in, in, out, in, out, di, uo) is det), in,
    out, in, out, di, uo) is det.

    % Same as foldl_while, but with 3 accumulators.
    %
:- pred foldl3_while(pred(string, V, bool, A, A, B, B, C, C), trie(V), bool,
    A, A, B, B, C, C).
:- mode foldl3_while(in(pred(in, in, out, in, out, in, out, in, out) is det),
    in, out, in, out, in, out, in, out) is det.
:- mode foldl3_while(in(pred(in, in, out, in, out, in, out, di, uo) is det), in,
    out, in, out, in, out, di, uo) is det.

    % Return the keys in a trie (the keys are in reverse order).
    %
:- func keys(trie(V)) = list(string).

    % Return the values in a trie.
    %
:- func values(trie(V)) = list(V).

    % Register a pickler and unpickler for tries.
    %
:- pred register_trie_pickle(pickles::in, pickles::out) is det.
:- pred register_trie_unpickle(unpickles::in, unpickles::out) is det.

    % Dump a representation of the trie to stdout (not the current output
    % stream).
    %
:- pred dump(trie(V)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module byte_buffer.

:- import_module exception.
:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module stream.
:- import_module type_desc.
:- import_module univ.

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C",
"

#include <string.h>

struct trie_s {
    char*           key;         /* A pointer to the key used when this node
                                  * was added.
                                  */
    char*           run_start;   /* A pointer into the key to the start of a
                                  * common run that this node, and all its
                                  * descendents, contain.
                                  */
    char*           run_end;     /* A pointer to the character immediately
                                  * after the end of the run (equal to
                                  * run_start if the run is empty).
                                  */
    char            first_char;  /* '\\0' when no children */
    char            last_char;   /* '\\0' when no children */
    MR_bool         has_value;   /* MR_TRUE if there is a value associated with
                                  * this node, MR_FALSE otherwise.
                                  */
    MR_Word         value;       /* The Mercury value stored at this node. */
    struct trie_s   *children[]; /* The children as a variable length array
                                  * at the end of the struct.  The array
                                  * is indexed by (c - first_char), where c is
                                  * the character from the search string.
                                  * If there is no child for the character,
                                  * the corresponding array entry will be NULL.
                                  */
};

typedef struct trie_s trie_t;

extern MR_bool trie_search(trie_t *t, char *key, MR_Word *value);
extern MR_bool trie_search_shortest_prefix(trie_t *t, char *key,
    char **prefix, MR_Word *value);
extern MR_bool trie_search_shortest_prefix_with_min(trie_t *t, char *key, 
    MR_Integer min, char **prefix, MR_Word *value);
extern trie_t *trie_set(char *key, char *key_ptr, MR_Word value, trie_t *t0);
extern trie_t *trie_delete(char *key, trie_t *t0);
extern void trie_dump(int ind, trie_t *t);
extern void trie_check(const char *message, trie_t *t);
extern MR_Integer trie_count(trie_t *t);

#ifdef DEBUG_TRIE
#define CHECK_TRIE(msg, t) trie_check(msg, t)
#else
#define CHECK_TRIE(msg, t) 
#endif
").

:- pragma foreign_code("C",
"

static void set_run(char *key, char *run, int len, trie_t *t);
static trie_t *new_trie(char *key, char *run_start, MR_Word value);
static trie_t *copy_trie(trie_t *t0);
static trie_t *decrease_lower_bound(char new_lower_bound, trie_t *t0);
static trie_t *increase_upper_bound(char new_upper_bound, trie_t *t0);
static trie_t *fixup_children(trie_t *t);
static int count_children(trie_t *t);
static void print_spaces(int ind);

MR_bool trie_search(trie_t *t, char *key, MR_Word *value)
{
    char    *run_ptr;
    char    *run_end;

    CHECK_TRIE(\"trie_search: \", t);

    do {

        if (t == NULL) {
            return MR_FALSE;
        }

        /* Match the common run. */
        run_ptr = t->run_start;
        run_end = t->run_end;
        while (run_ptr != run_end && *run_ptr == *key) {
            run_ptr++;
            key++;
        }
        if (run_ptr != run_end) {
            return MR_FALSE;
        }

        /* If we're at the end of the key, then we've found the matching node.
         */
        if (*key == '\\0') {
            *value = t->value;
            return t->has_value;
        }

        /* Follow the child node if there is one. */
        if (*key >= t->first_char && *key <= t->last_char) {
            t = t->children[*key - t->first_char];
            key++;
        } else {
            return MR_FALSE;
        }

    } while (1);
}

MR_bool trie_search_shortest_prefix(trie_t *t, char *key, char **prefix,
    MR_Word *value)
{
    char    *run_ptr;
    char    *run_end;

    CHECK_TRIE(\"trie_search_shortest_prefix: \", t);

    do {

        if (t == NULL) {
            return MR_FALSE;
        }

        /* Match the common run. */
        run_ptr = t->run_start;
        run_end = t->run_end;
        while (run_ptr != run_end && *run_ptr == *key) {
            run_ptr++;
            key++;
        }
        if (run_ptr != run_end) {
            return MR_FALSE;
        }

        /* If we're at the end of the key, then we've found the matching node.
         * If the node has a value, then we've found a matching prefix.
         */
        if (*key == '\\0' || t->has_value) {
            *value = t->value;
            *prefix = t->key;
            return t->has_value;
        }

        /* Follow the child node if there is one. */
        if (*key >= t->first_char && *key <= t->last_char) {
            t = t->children[*key - t->first_char];
            key++;
        } else {
            return MR_FALSE;
        }

    } while (1);
}

MR_bool trie_search_shortest_prefix_with_min(trie_t *t, char *key, 
    MR_Integer min, char **prefix, MR_Word *value)
{
    char    *run_ptr;
    char    *run_end;

    CHECK_TRIE(\"trie_search_shortest_prefix_with_min: \", t);

    do {

        if (t == NULL) {
            return MR_FALSE;
        }

        /* Match the common run. */
        run_ptr = t->run_start;
        run_end = t->run_end;
        while (run_ptr != run_end && *run_ptr == *key) {
            run_ptr++;
            key++;
        }
        if (run_ptr != run_end) {
            return MR_FALSE;
        }

        /* If we're at the end of the key, then we've found the matching node.
         * If the node has a value and the length of the key is >=
         * min, then we've found the shortest matching prefix with length at
         * least min.
         */
        if (*key == '\\0' || (t->has_value && (strlen(t->key) >= min))) {
            *value = t->value;
            *prefix = t->key;
            return t->has_value;
        }

        /* Follow the child node if there is one. */
        if (*key >= t->first_char && *key <= t->last_char) {
            t = t->children[*key - t->first_char];
            key++;
        } else {
            return MR_FALSE;
        }

    } while (1);
}

static void set_run(char *key, char *run, int len, trie_t *t)
{
    t->key = key;
    t->run_start = run;
    t->run_end = run + len;
}

static trie_t *new_trie(char *key, char *run_start, MR_Word value)
{
    trie_t    *t;
    
    t = MR_GC_malloc(sizeof(trie_t));
    set_run(key, run_start, strlen(run_start), t);
    t->first_char = '\\0';
    t->last_char = '\\0';
    t->has_value = MR_TRUE;
    t->value = value;

    CHECK_TRIE(\"new_trie: \", t);
    return t;
}

static trie_t *copy_trie(trie_t *t0) {
    trie_t  *t;
    int     size;

    if (t0->first_char == '\\0') {
        size = sizeof(trie_t);
    } else {
        size = sizeof(trie_t) +
            sizeof(trie_t*) * (t0->last_char - t0->first_char + 1);
    }
    t = MR_GC_malloc(size);
    memcpy(t, t0, size);
    
    CHECK_TRIE(\"copy_trie: \", t);
    return t;
}

static trie_t *decrease_lower_bound(char new_lower_bound, trie_t *t0) {
    trie_t  *t;
    int     num_children;
    int     i;

    if (t0->first_char == '\\0') {
        num_children = 1;
    } else {
        num_children = (t0->last_char - new_lower_bound + 1);
    }
    t = MR_GC_malloc(sizeof(trie_t) + sizeof(trie_t*) * num_children);

    t->key = t0->key;
    t->run_start = t0->run_start;
    t->run_end = t0->run_end;
    t->first_char = new_lower_bound;
    t->last_char = t0->last_char;
    t->has_value = t0->has_value;
    t->value = t0->value;
    
    if (t0->first_char == '\\0') {
        t->children[0] = NULL;
    } else {
        /* Copy t0's children to t, shifted to make room for the
         * new lower bound.
         */
        memcpy(&t->children[t0->first_char - new_lower_bound], t0->children,
            sizeof(trie_t *) * (t0->last_char - t0->first_char + 1));
        /* Make the new beginning of the children array NULL */
        memset(t->children, 0,
            sizeof(trie_t *) * (t0->first_char - new_lower_bound));
    }

    CHECK_TRIE(\"decrease_lower_bound: \", t);
    return t;
}

static trie_t *increase_upper_bound(char new_upper_bound, trie_t *t0) {
    trie_t  *t;
    int     num_children;
    int     i;

    if (t0->first_char == '\\0') {
        num_children = 1;
    } else {
        num_children = (new_upper_bound - t0->first_char + 1);
    }
    t = MR_GC_malloc(sizeof(trie_t) + sizeof(trie_t*) * num_children);

    t->key = t0->key;
    t->run_start = t0->run_start;
    t->run_end = t0->run_end;
    if (t0->first_char == '\\0') {
        t->first_char = new_upper_bound;
    } else {
        t->first_char = t0->first_char;
    }
    t->last_char = new_upper_bound;
    t->has_value = t0->has_value;
    t->value = t0->value;
    
    if (t0->first_char == '\\0') {
        t->children[0] = NULL;
    } else {
        /* Copy t0's children to t. */
        memcpy(t->children, t0->children,
            sizeof(trie_t *) * (t0->last_char - t0->first_char + 1));
        /* Make the new end of the children array NULL */
        memset(&t->children[t0->last_char - t0->first_char + 1], 0,
            sizeof(trie_t *) * (new_upper_bound - t0->last_char));
    }
    
    CHECK_TRIE(\"increase_upper_bound: \", t);
    return t;
}

trie_t *trie_set(char *key, char *key_ptr, MR_Word value, trie_t *t0)
{
    char    *run0_ptr;
    trie_t  *t;
    trie_t  *child0;
    trie_t  *child;
    trie_t  *child1;
    trie_t  *child2;
    int     i;
    int     num_children;
    int     match_len;

    if (t0 == NULL) {
        t = new_trie(key, key_ptr, value);
        return t;
    }

    run0_ptr = t0->run_start;
    match_len = 0;

    /* Match as much of the run as possible */
    while (run0_ptr != t0->run_end && *run0_ptr == *key_ptr) {
        run0_ptr++;
        key_ptr++;
        match_len++;
    }
    
    if (run0_ptr == t0->run_end) {
        /* We matched the whole run. */
        if (*key_ptr == '\\0') {
            /* We found the matching node. */
            t = copy_trie(t0);
            t->has_value = MR_TRUE;
            t->value = value;
            /* Update the key to the key for the new value. */
            set_run(key, key_ptr - match_len, match_len, t);
            CHECK_TRIE(\"trie_set(1): \", t);
            return t;
        }
        if (*key_ptr >= t0->first_char) {
            if (*key_ptr <= t0->last_char) {
                /* We are within the array bounds, so we need to set
                 * the appropriate entry. */
                child0 = t0->children[*key_ptr - t0->first_char];
                if (child0 != NULL) {
                    child = trie_set(key, key_ptr + 1, value, child0);
                } else {
                    child = new_trie(key, key_ptr + 1, value);
                }
                t = copy_trie(t0);
                t->children[*key_ptr - t0->first_char] = child;
                CHECK_TRIE(\"trie_set(2): \", t);
                return t;
            } else {
                /* We need increase the upper bound of the array. */
                t = increase_upper_bound(*key_ptr, t0);
                child = new_trie(key, key_ptr + 1, value);
                t->children[*key_ptr - t->first_char] = child;
                CHECK_TRIE(\"trie_set(3): \", t);
                return t;
            }
        } else {
            /* We need to decrease the lower bound of the array. */
            t = decrease_lower_bound(*key_ptr, t0);
            child = new_trie(key, key_ptr + 1, value);
            t->children[0] = child;
            CHECK_TRIE(\"trie_set(4): \", t);
            return t;
        }
    } else {
        /* We didn't match the entire run, so we need to split the run.
         */
        if (*key_ptr == '\\0') {
            /* We've reached the end of the key.
             * In this case we need to create two nodes.  One that contains
             * the common prefix of t0 and key_ptr, and one that contains
             * the rest of t0.  The former node will contain the value
             * we are inserting.
             */
            t = MR_GC_malloc(sizeof(trie_t) + sizeof(trie_t*));
            set_run(key, key_ptr - match_len, match_len, t);
            t->has_value = MR_TRUE;
            t->value = value;
            t->first_char = *run0_ptr;
            t->last_char = *run0_ptr;
            child = copy_trie(t0);
            set_run(t0->key, run0_ptr + 1, t0->run_end - run0_ptr - 1, child);
            t->children[0] = child;
            CHECK_TRIE(\"trie_set(5): \", t);
            return t;
        }
        /* There is still some more of the key left, so we need to create
         * three nodes: One containing the common prefix of t0 and
         * key_ptr, another containing the rest of the key and another
         * containing the rest of t0.
         */
        if (*run0_ptr < *key_ptr) {
            num_children = *key_ptr - *run0_ptr + 1;
            t = MR_GC_malloc(sizeof(trie_t) + num_children * sizeof(trie_t*));
            t->first_char = *run0_ptr;
            t->last_char = *key_ptr;
            child1 = copy_trie(t0);
            child1->run_start += match_len + 1;
            child2 = new_trie(key, key_ptr + 1, value);
        } else {
            num_children = *run0_ptr - *key_ptr + 1;
            t = MR_GC_malloc(sizeof(trie_t) + num_children * sizeof(trie_t*));
            t->first_char = *key_ptr;
            t->last_char = *run0_ptr;
            child1 = new_trie(key, key_ptr + 1, value);
            child2 = copy_trie(t0);
            child2->run_start += match_len + 1;
        }
        set_run(key, key_ptr - match_len, match_len, t);
        t->has_value = MR_FALSE;
        t->value = (MR_Word)NULL;

        for (i = 0; i < num_children; i++) {
            t->children[i] = NULL;
        }
        t->children[0] = child1;
        t->children[t->last_char - t->first_char] = child2;
        CHECK_TRIE(\"trie_set(6): \", t);
        return t;
    }
}

static int count_children(trie_t *t)
{
    char    c;
    int     count = 0;

    if (t->first_char == '\\0') {
        return 0;
    } else {
        for (c = t->first_char; c <= t->last_char; c++) {
            if (t->children[c - t->first_char] != NULL) {
                count++;
            }
        }
    }

    return count;
}

/* 
 * This function fixes up the children after a child has been deleted
 * (i.e. made NULL).  It adjusts the first_char and last_char fields
 * and shifts the children so that the first element is non-null, but
 * doesn't resize the node.  The modified node is returned.  If there
 * are no children and the node doesn't contain a value, then the empty
 * node (i.e. NULL) is returned.
 */
static trie_t *fixup_children(trie_t *t)
{
    char    lower_bound;
    char    upper_bound;
    MR_bool found_lower_bound;
    MR_bool found_upper_bound;
    char    c;
    int     shift;
    int     count = 0;
    int     run_len;

    if (t->first_char != '\\0') {
        found_upper_bound = MR_FALSE;
        found_lower_bound = MR_FALSE;
        for (c = t->first_char; c <= t->last_char; c++) {
            if (t->children[c - t->first_char] != NULL) {
                if (!found_lower_bound || c < lower_bound) {
                    lower_bound = c;
                    found_lower_bound = MR_TRUE;
                }
                if (!found_upper_bound || c > upper_bound) {
                    upper_bound = c;
                    found_upper_bound = MR_TRUE;
                }
                count++;
            }
        }
        if (count == 0) {
            /* If the node has no children and doesn't hold a value,
             * return NULL.
             */
            if (t->has_value == MR_FALSE) {
                return NULL;
            } else {
                t->first_char = '\\0';
                t->last_char = '\\0';
                CHECK_TRIE(\"fixup_children(1): \", t);
                return t;
            }
        } if (count == 1 && t->has_value == MR_FALSE) {
            /* If the node has exactly one node, and doesn't hold a value,
             * then we can move the only child up a level, adjusting its
             * run accordingly.
             */
            run_len = t->run_end - t->run_start;
            t = copy_trie(t->children[lower_bound - t->first_char]);
            t->run_start -= run_len + 1;
            CHECK_TRIE(\"fixup_children(2): \", t);
            return t;
        } else {
            /* Shift the children so that they start at the new lower bound.
             */
            shift = lower_bound - t->first_char;
            t->first_char = lower_bound;
            t->last_char = upper_bound;
            if (shift > 0) {
                for (c = lower_bound; c <= upper_bound; c++) {
                    t->children[c - lower_bound] =
                        t->children[c - lower_bound + shift];
                }
            }
            CHECK_TRIE(\"fixup_children(3): \", t);
            return t;
        }
    } else {
        if (t->has_value == MR_FALSE) {
            /* The node has no children and doesn't hold a value,
             * so return NULL.
             */
            return NULL;
        } else {
            CHECK_TRIE(\"fixup_children(5): \", t);
            return t;
        }
    }
}

trie_t *trie_delete(char *key_ptr, trie_t *t0)
{
    trie_t  *t;
    trie_t  *child;
    int     num_children;
    int     run_len;
    int     run_len0;
    int     child_run_len;
    char    *run0_ptr;

    /* If the trie is empty, return an empty trie, since there is nothing to
     * delete.
     */
    if (t0 == NULL) {
        return NULL;
    }

    /* Match the common run. */
    run0_ptr = t0->run_start;
    while (run0_ptr != t0->run_end && *run0_ptr == *key_ptr) {
        run0_ptr++;
        key_ptr++;
    }
    if (run0_ptr != t0->run_end) {
        CHECK_TRIE(\"trie_delete(0): \", t0);
        return t0;
    }

    /* We've found the matching node. */
    if (*key_ptr == '\\0') {
        num_children = count_children(t0);
        if (num_children == 0) {
            /* The node has no children, so we can delete it entirely. */
            return NULL;
        } else if (num_children == 1) {
            /* The node has only one child, so we can delete it and return
             * the child node, with this node's run prefixed to the child
             * node's run.
             */
            t = copy_trie(t0->children[0]);
            t->run_start -= run0_ptr - t0->run_start + 1;
            CHECK_TRIE(\"trie_delete(1): \", t);
            return t;
        } else {
            /* There are multiple children, so just remove the value from
             * the node.
             */
            t = copy_trie(t0);
            t->has_value = MR_FALSE;
            t->value = (MR_Word)NULL;
            CHECK_TRIE(\"trie_delete(2): \", t);
            return t;
        }
    }

    /* Follow a child node if there is one. */
    if (*key_ptr >= t0->first_char && *key_ptr <= t0->last_char) {
        child = t0->children[*key_ptr - t0->first_char];
        if (child == NULL) {
            CHECK_TRIE(\"trie_delete(3): \", t0);
            return t0;
        } else {
            t = copy_trie(t0);
            child = trie_delete(key_ptr + 1, child);
            t->children[*key_ptr - t0->first_char] = child;
            if (child == NULL) {
                t = fixup_children(t);
            }
            CHECK_TRIE(\"trie_delete(4): \", t);
            return t;
        }
    } else {
        CHECK_TRIE(\"trie_delete(5): \", t0);
        return t0;
    }
}

static void print_spaces(int ind)
{
    int i;
    for (i = 0; i < ind; i++) {
        putchar(' ');
    }
}

void trie_dump(int ind, trie_t *t)
{
    char    c;
    trie_t  *child;
    char    *run;

    if (t != NULL) {
        print_spaces(ind);
        run = MR_malloc(t->run_end - t->run_start + 1);
        memcpy(run, t->run_start, t->run_end - t->run_start);
        run[t->run_end - t->run_start] = '\\0';
        printf(\"\\\"%s\\\"\", run);
        MR_free(run);
        if (t->has_value) {
            putchar('*');
        }
        printf(\":\\n\");
        
        if (t->first_char != '\\0') {
            for (c = t->first_char; c <= t->last_char; c++) {
                child = t->children[c - t->first_char];
                if (child != NULL) {
                    print_spaces(ind + 2);
                    printf(\"%c:\\n\", c);
                    trie_dump(ind + 4, child);
                }
            }
        }
    }
}

void trie_check(const char* message, trie_t *t)
{
    char    c;
    trie_t  *child;

    if (t != NULL) {
        if (((MR_Integer)t & (sizeof(MR_Word) - 1)) != 0) {
            fprintf(stderr, \"\\n*** trie_check: %s %s: %p ***\\n\",
                message, \"t not word-aligned\", &t);
            fflush(stderr);
            MR_fatal_error(\"trie_check failed\");
        }
        if (t->key == NULL) {
            fprintf(stderr, \"\\n*** trie_check: %s %s: %p ***\\n\",
                message, \"key is NULL\", &t->key);
            fflush(stderr);
            MR_fatal_error(\"trie_check failed\");
        }
        /*
        if (((MR_Integer)t->key & (sizeof(MR_Word) - 1)) != 0) {
            fprintf(stderr, \"\\n*** trie_check: %s %s: %s: %p ***\\n\",
                message, \"t->key not word-aligned\", t->key, t->key);
            fflush(stderr);
            MR_fatal_error(\"trie_check failed\");
        }
        */
        if (t->run_start == NULL) {
            fprintf(stderr, \"\\n*** trie_check: %s %s: %p ***\\n\",
                message, \"run_start is NULL\", &t->run_start);
            fflush(stderr);
            MR_fatal_error(\"trie_check failed\");
        }
        if (t->run_end == NULL) {
            fprintf(stderr, \"\\n*** trie_check: %s %s: %p ***\\n\",
                message, \"run_end is NULL\", &t->run_end);
            fflush(stderr);
            MR_fatal_error(\"trie_check failed\");
        }
        if (t->run_end < t->run_start) {
            fprintf(stderr, \"\\n*** trie_check: %s %s: %p ***\\n\",
                message, \"run_end < run_start\", t);
            fflush(stderr);
            fprintf(stderr, \"run_start = '%s' run_end = '%s'\\n\",
                t->run_start, t->run_end);
            fflush(stderr);
            MR_fatal_error(\"trie_check failed\");
        }
        if (t->first_char != '\\0') {
            for (c = t->first_char; c <= t->last_char; c++) {
                child = t->children[c - t->first_char];
                if (((MR_Integer)&child & (sizeof(MR_Word) - 1)) != 0) {
                    fprintf(stderr, \"\\n*** trie_check: %s: %s: %p ***\\n\",
                        message, \"&child not word-aligned\", &child);
                    fflush(stderr);
                    MR_fatal_error(\"trie_check failed\");
                }
                trie_check(message, child);
            }
        }
    }
}

MR_Integer trie_count(trie_t *t)
{
    char            c;
    trie_t          *child;
    MR_Integer      count;

    CHECK_TRIE(\"trie_count: \", t);

    if (t == NULL) {
        return 0;
    } else {
        if (t->has_value) {
            count = 1;
        } else {
            count = 0;
        }
        if (t->first_char != '\\0') {
            for (c = t->first_char; c <= t->last_char; c++) {
                child = t->children[c - t->first_char];
                if (child != NULL) {
                    count += trie_count(child);
                }
            }
        }
        return count;
    }
}

").

%-----------------------------------------------------------------------------%

% This is a straightforward translation of the above C to C#.
% Refer to the comments in the C code.
%
:- pragma foreign_code("C#",
"
[System.Serializable]
public class Trie {
    public string   key;
    public int      run_start;
    public int      run_end;
    public char     first_char;
    public char     last_char;
    public bool     has_value;
    public object   val;
    public Trie[]   children;
}

/**
 * Same as str.charAt(index), but returns '\\0' if index == str.length().
 * This makes it easier to port the C code to Java.
 */
private static char chr(string str, int index)
{
    if (str.Length == index) {
        return '\\0';
    } else {
        return str[index];
    }
}

private static void set_run(string key, int run, int len, Trie t)
{
    t.key = key;
    t.run_start = run;
    t.run_end = run + len;
}

public static bool trie_search(Trie t, string key, out object val)
{
    int     run_len;
    int     key_ptr = 0;
    char    c;

    do {
        if (t == null) {
            val = null;
            return false;
        }

        run_len = t.run_end - t.run_start;
        if (System.String.CompareOrdinal(key, key_ptr, t.key, t.run_start,
                run_len) != 0) {
            val = null;
            return false;
        }
        key_ptr += run_len;

        if (key_ptr == key.Length) {
            val = t.val;
            return t.has_value;
        }

        c = key[key_ptr];
        if (c >= t.first_char && c <= t.last_char) {
            t = t.children[c - t.first_char];
            key_ptr++;
        } else {
            val = null;
            return false;
        }

    } while (true);
}

public static bool trie_search_shortest_prefix(Trie t, string key,
    out string prefix, out object val)
{
    int run_ptr;
    int run_end;
    int key_ptr = 0;

    do {
        if (t == null) {
            prefix = null;
            val = null;
            return false;
        }

        run_ptr = t.run_start;
        run_end = t.run_end;
        while (run_ptr != run_end && chr(t.key, run_ptr) == chr(key, key_ptr)) {
            run_ptr++;
            key_ptr++;
        }
        if (run_ptr != run_end) {
            prefix = null;
            val = null;
            return false;
        }

        if (chr(key, key_ptr) == '\\0' || t.has_value) {
            prefix = t.key;
            val = t.val;
            return t.has_value;
        }

        if (chr(key, key_ptr) >= t.first_char && chr(key, key_ptr) <= t.last_char) {
            t = t.children[chr(key, key_ptr) - t.first_char];
            key_ptr++;
        } else {
            prefix = null;
            val = null;
            return false;
        }

    } while (true);
}

public static bool trie_search_shortest_prefix_with_min(Trie t, string key,
    int min, out string prefix, out object val)
{
    int     run_ptr;
    int     run_end;
    int     key_ptr = 0;

    do {
        if (t == null) {
            prefix = null;
            val = null;
            return false;
        }

        run_ptr = t.run_start;
        run_end = t.run_end;
        while (run_ptr != run_end && chr(t.key, run_ptr) == chr(key, key_ptr)) {
            run_ptr++;
            key_ptr++;
        }
        if (run_ptr != run_end) {
            prefix = null;
            val = null;
            return false;
        }

        if (chr(key, key_ptr) == '\\0' || (t.has_value && t.key.Length >= min)) {
            prefix = t.key;
            val = t.val;
            return t.has_value;
        }
        
        if (chr(key, key_ptr) >= t.first_char && chr(key, key_ptr) <= t.last_char) {
            t = t.children[chr(key, key_ptr) - t.first_char];
            key_ptr++;
        } else {
            prefix = null;
            val = null;
            return false;
        }

    } while (true);
}

public static Trie new_trie(string key, int run_start, object val)
{
    Trie t;

    t = new Trie();
    set_run(key, run_start, key.Length - run_start, t);
    t.first_char = '\\0';
    t.last_char = '\\0';
    t.has_value = true;
    t.val = val;

    return t;
}

private static Trie copy_trie(Trie t0)
{
    Trie t;
    int i;

    t = new Trie();
    t.key = t0.key;
    t.run_start = t0.run_start;
    t.run_end = t0.run_end;
    t.first_char = t0.first_char;
    t.last_char = t0.last_char;
    t.has_value = t0.has_value;
    t.val = t0.val;
    if (t0.children != null) {
        t.children = new Trie[t0.children.Length];
        for (i = 0; i < t0.children.Length; i++) {
            t.children[i] = t0.children[i];
        }
    }

    return t;
}

private static Trie decrease_lower_bound(char new_lower_bound, Trie t0)
{
    Trie t;
    int num_children;

    if (t0.first_char == '\\0') {
        num_children = 1;
    } else {
        num_children = (t0.last_char - new_lower_bound + 1);
    }
    t = new Trie();
    t.children = new Trie[num_children];
    t.key = t0.key;
    t.run_start = t0.run_start;
    t.run_end = t0.run_end;
    t.first_char = new_lower_bound;
    t.last_char = t0.last_char;
    t.has_value = t0.has_value;
    t.val = t0.val;

    if (t0.first_char == '\\0') {
        t.children[0] = null;
    } else {
        int i = t0.first_char - new_lower_bound;
        int j = 0;
        int n = t0.last_char - t0.first_char + 1;
        while (j < n) {
            t.children[i] = t0.children[j];
            j++;
            i++;
        }
    }

    return t;
}

private static Trie increase_upper_bound(char new_upper_bound, Trie t0)
{
    Trie    t;
    int     num_children;
    int     i;

    if (t0.first_char == '\\0') {
        num_children = 1;
    } else {
        num_children = new_upper_bound - t0.first_char + 1;
    }
    t = new Trie();
    t.children = new Trie[num_children];

    t.key = t0.key;
    t.run_start = t0.run_start;
    t.run_end = t0.run_end;
    if (t0.first_char == '\\0') {
        t.first_char = new_upper_bound;
    } else {
        t.first_char = t0.first_char;
    }
    t.last_char = new_upper_bound;
    t.has_value = t0.has_value;
    t.val = t0.val;

    if (t0.first_char == '\\0') {
        t.children[0] = null;
    } else {
        for (i = 0; i < t0.last_char - t0.first_char + 1; i++) {
            t.children[i] = t0.children[i];
        }
    }

    return t;
}

public static Trie trie_set(string key, int key_ptr, object val, Trie t0)
{
    int     run0_ptr;
    Trie    t;
    Trie    child0;
    Trie    child;
    Trie    child1;
    Trie    child2;
    int     i;
    int     num_children;
    int     match_len;

    if (t0 == null) {
        t = new_trie(key, key_ptr, val);
        return t;
    }

    run0_ptr = t0.run_start;
    match_len = 0;

    while (run0_ptr != t0.run_end && chr(t0.key, run0_ptr) == chr(key, key_ptr)) {
        run0_ptr++;
        key_ptr++;
        match_len++;
    }

    if (run0_ptr == t0.run_end) {
        if (chr(key, key_ptr) == '\\0') {
            t = copy_trie(t0);
            t.has_value = true;
            t.val = val;
            set_run(key, key_ptr - match_len, match_len, t);
            return t;
        }
        if (chr(key, key_ptr) >= t0.first_char) {
            if (chr(key, key_ptr) <= t0.last_char) {
                child0 = t0.children[chr(key, key_ptr) - t0.first_char];
                if (child0 != null) {
                    child = trie_set(key, key_ptr + 1, val, child0);
                } else {
                    child = new_trie(key, key_ptr + 1, val);
                }
                t = copy_trie(t0);
                t.children[chr(key, key_ptr) - t0.first_char] = child;

                return t;
            } else {
                t = increase_upper_bound(chr(key, key_ptr), t0);
                child = new_trie(key, key_ptr + 1, val);
                t.children[chr(key, key_ptr) - t.first_char] = child;
                return t;
            }
        } else {
            t = decrease_lower_bound(chr(key, key_ptr), t0);
            child = new_trie(key, key_ptr + 1, val);
            t.children[0] = child;
            return t;
        }
    } else {
        if (chr(key, key_ptr) == '\\0') {
            t = new Trie();
            t.children = new Trie[1];
            set_run(key, key_ptr - match_len, match_len, t);
            t.has_value = true;
            t.val = val;
            t.first_char = chr(t0.key, run0_ptr);
            t.last_char = chr(t0.key, run0_ptr);
            child = copy_trie(t0);
            set_run(t0.key, run0_ptr + 1, t0.run_end - run0_ptr - 1, child);
            t.children[0] = child;
            return t;
        }
        if (chr(t0.key, run0_ptr) < chr(key, key_ptr)) {
            num_children = chr(key, key_ptr) - chr(t0.key, run0_ptr) + 1;
            t = new Trie();
            t.children = new Trie[num_children];
            t.first_char = chr(t0.key, run0_ptr);
            t.last_char = chr(key, key_ptr);
            child1 = copy_trie(t0);
            child1.run_start += match_len + 1;
            child2 = new_trie(key, key_ptr + 1, val);
        } else {
            num_children = chr(t0.key, run0_ptr) - chr(key, key_ptr) + 1;
            t = new Trie();
            t.children = new Trie[num_children];
            t.first_char = chr(key, key_ptr);
            t.last_char = chr(t0.key, run0_ptr);
            child1 = new_trie(key, key_ptr + 1, val);
            child2 = copy_trie(t0);
            child2.run_start += match_len + 1;
        }
        set_run(key, key_ptr - match_len, match_len, t);
        t.has_value = false;
        t.val = null;

        for (i = 0; i < num_children; i++) {
            t.children[i] = null;
        }
        t.children[0] = child1;
        t.children[t.last_char - t.first_char] = child2;
        return t;
    }
}

private static int count_children(Trie t)
{
    char    c;
    int     count = 0;

    if (t.first_char == '\\0') {
        return 0;
    } else {
        for (c = t.first_char; c <= t.last_char; c++) {
            if (t.children[c - t.first_char] != null) {
                count++;
            }
        }
    }

    return count;
}

private static Trie fixup_children(Trie t) {
    char    lower_bound = '\\0';
    char    upper_bound = '\\0';
    bool    found_lower_bound;
    bool    found_upper_bound;
    char    c;
    int     shift;
    int     count = 0;
    int     run_len;

    if (t.first_char != '\\0') {
        found_upper_bound = false;
        found_lower_bound = false;
        for (c = t.first_char; c <= t.last_char; c++) {
            if (t.children[c - t.first_char] != null) {
                if (!found_lower_bound || c < lower_bound) {
                    lower_bound = c;
                    found_lower_bound = true;
                }
                if (!found_upper_bound || c > upper_bound) {
                    upper_bound = c;
                    found_upper_bound = true;
                }
                count++;
            }
        }
        if (count == 0) {
            if (t.has_value == false) {
                return null;
            } else {
                t.first_char = '\\0';
                t.last_char = '\\0';
                return t;
            }
        } if (count == 1 && t.has_value == false) {
            run_len = t.run_end - t.run_start;
            t = copy_trie(t.children[lower_bound - t.first_char]);
            t.run_start -= run_len + 1;
            return t;
        } else {
            shift = lower_bound - t.first_char;
            t.first_char = lower_bound;
            t.last_char = upper_bound;
            if (shift > 0) {
                for (c = lower_bound; c <= upper_bound; c++) {
                    t.children[c - lower_bound] = 
                        t.children[c - lower_bound + shift];
                }
            }
            return t;
        }
    } else {
        if (t.has_value == false) {
            return null;
        } else {
            return t;
        }
    }
}

public static Trie trie_delete(string key, int key_ptr, Trie t0)
{
    Trie    t;
    Trie    child;
    int     num_children;
    int     run_len;
    int     run_len0;
    int     child_run_len;
    int     run0_ptr;

    if (t0 == null) {
        return null;
    }

    run0_ptr = t0.run_start;
    while (run0_ptr != t0.run_end && chr(t0.key, run0_ptr) == chr(key, key_ptr)) {
        run0_ptr++;
        key_ptr++;
    }
    if (run0_ptr != t0.run_end) {
        return t0;
    }

    if (chr(key, key_ptr) == '\\0') {
        num_children = count_children(t0);
        if (num_children == 0) {
            return null;
        } else if (num_children == 1) {
            t = copy_trie(t0.children[0]);
            t.run_start -= run0_ptr - t0.run_start + 1;
            return t;
        } else {
            t = copy_trie(t0);
            t.has_value = false;
            t.val = null;
            return t;
        }
    }

    if (chr(key, key_ptr) >= t0.first_char && chr(key, key_ptr) <= t0.last_char) {
        child = t0.children[chr(key, key_ptr) - t0.first_char];
        if (child == null) {
            return t0;
        } else {
            t = copy_trie(t0);
            child = trie_delete(key, key_ptr + 1, child);
            t.children[chr(key, key_ptr) - t0.first_char] = child;
            if (child == null) {
                t = fixup_children(t);
            }
            return t;
        }
    } else {
        return t0;
    }
}

public static int trie_count(Trie t)
{
    char            c;
    Trie            child;
    int             count;

    if (t == null) {
        return 0;
    } else {
        if (t.has_value) {
            count = 1;
        } else {
            count = 0;
        }
        if (t.first_char != '\\0') {
            for (c = t.first_char; c <= t.last_char; c++) {
                child = t.children[c - t.first_char];
                if (child != null) {
                    count += trie_count(child);
                }
            }
        }
        return count;
    }
}

private static void print_spaces(int ind)
{
    int i;
    for (i = 0; i < ind; i++) {
        System.Console.Write(' ');
    }
}

public static void trie_dump(int ind, Trie t)
{
    char    c;
    Trie    child;
    string  run;

    if (t != null) {
        print_spaces(ind);
        run = t.key.Substring(t.run_start, t.run_end);
        System.Console.WriteLine(\"\\\"\" + run + \"\\\"\");
        if (t.has_value) {
            System.Console.WriteLine('*');
        }
        System.Console.WriteLine(\":\\n\");

        if (t.first_char != '\\0') {
            for (c = t.first_char; c <= t.last_char; c++) {
                child = t.children[c - t.first_char];
                if (child != null) {
                    print_spaces(ind + 2);
                    System.Console.WriteLine(c);
                    System.Console.WriteLine(\":\\n\");
                    trie_dump(ind + 4, child);
                }
            }
        }
    }
}

").
 
%-----------------------------------------------------------------------------%

% This is a straightforward translation of the above C to Java.
% Refer to the comments in the C code.
%
:- pragma foreign_code("Java",
"
public static class Trie implements java.io.Serializable {
    public String   key;
    public int      run_start;
    public int      run_end;
    public char     first_char;
    public char     last_char;
    public boolean  has_value;
    public Object   value;
    public Trie[]   children;
}

/**
 * Used for getting multiple outputs from Java methods.
 */
public static class Ref<T> {
    public T val;
}

/**
 * Same as str.charAt(index), but returns '\\0' if index == str.length().
 * This makes it easier to port the C code to Java.
 */
private static char chr(String str, int index)
{
    if (str.length() == index) {
        return '\\0';
    } else {
        return str.charAt(index);
    }
}

private static void set_run(String key, int run, int len, Trie t)
{
    t.key = key;
    t.run_start = run;
    t.run_end = run + len;
}

public static boolean trie_search(Trie t, String key, Ref<Object> value)
{
    int     run_len;
    int     key_ptr = 0;
    char    c;

    do {
        if (t == null) {
            return false;
        }

        run_len = t.run_end - t.run_start;
        if (!key.regionMatches(key_ptr, t.key, t.run_start, run_len)) {
            return false;
        }
        key_ptr += run_len;

        if (key_ptr == key.length()) {
            value.val = t.value;
            return t.has_value;
        }

        c = key.charAt(key_ptr);
        if (c >= t.first_char && c <= t.last_char) {
            t = t.children[c - t.first_char];
            key_ptr++;
        } else {
            return false;
        }

    } while (true);
}

public static boolean trie_search_shortest_prefix(Trie t, String key, Ref<String> prefix,
    Ref<Object> value)
{
    int run_ptr;
    int run_end;
    int key_ptr = 0;

    do {
        if (t == null) {
            return false;
        }

        run_ptr = t.run_start;
        run_end = t.run_end;
        while (run_ptr != run_end && chr(t.key, run_ptr) == chr(key, key_ptr)) {
            run_ptr++;
            key_ptr++;
        }
        if (run_ptr != run_end) {
            return false;
        }

        if (chr(key, key_ptr) == '\\0' || t.has_value) {
            prefix.val = t.key;
            value.val = t.value;
            return t.has_value;
        }

        if (chr(key, key_ptr) >= t.first_char && chr(key, key_ptr) <= t.last_char) {
            t = t.children[chr(key, key_ptr) - t.first_char];
            key_ptr++;
        } else {
            return false;
        }

    } while (true);
}

public static boolean trie_search_shortest_prefix_with_min(Trie t, String key,
    int min, Ref<String> prefix, Ref<Object> value)
{
    int     run_ptr;
    int     run_end;
    int     key_ptr = 0;

    do {
        if (t == null) {
            return false;
        }

        run_ptr = t.run_start;
        run_end = t.run_end;
        while (run_ptr != run_end && chr(t.key, run_ptr) == chr(key, key_ptr)) {
            run_ptr++;
            key_ptr++;
        }
        if (run_ptr != run_end) {
            return false;
        }

        if (chr(key, key_ptr) == '\\0' || (t.has_value && t.key.length() >= min)) {
            prefix.val = t.key;
            value.val = t.value;
            return t.has_value;
        }
        
        if (chr(key, key_ptr) >= t.first_char && chr(key, key_ptr) <= t.last_char) {
            t = t.children[chr(key, key_ptr) - t.first_char];
            key_ptr++;
        } else {
            return false;
        }

    } while (true);
}

public static Trie new_trie(String key, int run_start, Object value)
{
    Trie t;

    t = new Trie();
    set_run(key, run_start, key.length() - run_start, t);
    t.first_char = '\\0';
    t.last_char = '\\0';
    t.has_value = true;
    t.value = value;

    return t;
}

private static Trie copy_trie(Trie t0)
{
    Trie t;
    int i;

    t = new Trie();
    t.key = t0.key;
    t.run_start = t0.run_start;
    t.run_end = t0.run_end;
    t.first_char = t0.first_char;
    t.last_char = t0.last_char;
    t.has_value = t0.has_value;
    t.value = t0.value;
    if (t0.children != null) {
        t.children = new Trie[t0.children.length];
        for (i = 0; i < t0.children.length; i++) {
            t.children[i] = t0.children[i];
        }
    }

    return t;
}

private static Trie decrease_lower_bound(char new_lower_bound, Trie t0)
{
    Trie t;
    int num_children;

    if (t0.first_char == '\\0') {
        num_children = 1;
    } else {
        num_children = (t0.last_char - new_lower_bound + 1);
    }
    t = new Trie();
    t.children = new Trie[num_children];
    t.key = t0.key;
    t.run_start = t0.run_start;
    t.run_end = t0.run_end;
    t.first_char = new_lower_bound;
    t.last_char = t0.last_char;
    t.has_value = t0.has_value;
    t.value = t0.value;

    if (t0.first_char == '\\0') {
        t.children[0] = null;
    } else {
        int i = t0.first_char - new_lower_bound;
        int j = 0;
        int n = t0.last_char - t0.first_char + 1;
        while (j < n) {
            t.children[i] = t0.children[j];
            j++;
            i++;
        }
    }

    return t;
}

private static Trie increase_upper_bound(char new_upper_bound, Trie t0)
{
    Trie    t;
    int     num_children;
    int     i;

    if (t0.first_char == '\\0') {
        num_children = 1;
    } else {
        num_children = new_upper_bound - t0.first_char + 1;
    }
    t = new Trie();
    t.children = new Trie[num_children];

    t.key = t0.key;
    t.run_start = t0.run_start;
    t.run_end = t0.run_end;
    if (t0.first_char == '\\0') {
        t.first_char = new_upper_bound;
    } else {
        t.first_char = t0.first_char;
    }
    t.last_char = new_upper_bound;
    t.has_value = t0.has_value;
    t.value = t0.value;

    if (t0.first_char == '\\0') {
        t.children[0] = null;
    } else {
        for (i = 0; i < t0.last_char - t0.first_char + 1; i++) {
            t.children[i] = t0.children[i];
        }
    }

    return t;
}

public static Trie trie_set(String key, int key_ptr, Object value, Trie t0)
{
    int     run0_ptr;
    Trie    t;
    Trie    child0;
    Trie    child;
    Trie    child1;
    Trie    child2;
    int     i;
    int     num_children;
    int     match_len;

    if (t0 == null) {
        t = new_trie(key, key_ptr, value);
        return t;
    }

    run0_ptr = t0.run_start;
    match_len = 0;

    while (run0_ptr != t0.run_end && chr(t0.key, run0_ptr) == chr(key, key_ptr)) {
        run0_ptr++;
        key_ptr++;
        match_len++;
    }

    if (run0_ptr == t0.run_end) {
        if (chr(key, key_ptr) == '\\0') {
            t = copy_trie(t0);
            t.has_value = true;
            t.value = value;
            set_run(key, key_ptr - match_len, match_len, t);
            return t;
        }
        if (chr(key, key_ptr) >= t0.first_char) {
            if (chr(key, key_ptr) <= t0.last_char) {
                child0 = t0.children[chr(key, key_ptr) - t0.first_char];
                if (child0 != null) {
                    child = trie_set(key, key_ptr + 1, value, child0);
                } else {
                    child = new_trie(key, key_ptr + 1, value);
                }
                t = copy_trie(t0);
                t.children[chr(key, key_ptr) - t0.first_char] = child;

                return t;
            } else {
                t = increase_upper_bound(chr(key, key_ptr), t0);
                child = new_trie(key, key_ptr + 1, value);
                t.children[chr(key, key_ptr) - t.first_char] = child;
                return t;
            }
        } else {
            t = decrease_lower_bound(chr(key, key_ptr), t0);
            child = new_trie(key, key_ptr + 1, value);
            t.children[0] = child;
            return t;
        }
    } else {
        if (chr(key, key_ptr) == '\\0') {
            t = new Trie();
            t.children = new Trie[1];
            set_run(key, key_ptr - match_len, match_len, t);
            t.has_value = true;
            t.value = value;
            t.first_char = chr(t0.key, run0_ptr);
            t.last_char = chr(t0.key, run0_ptr);
            child = copy_trie(t0);
            set_run(t0.key, run0_ptr + 1, t0.run_end - run0_ptr - 1, child);
            t.children[0] = child;
            return t;
        }
        if (chr(t0.key, run0_ptr) < chr(key, key_ptr)) {
            num_children = chr(key, key_ptr) - chr(t0.key, run0_ptr) + 1;
            t = new Trie();
            t.children = new Trie[num_children];
            t.first_char = chr(t0.key, run0_ptr);
            t.last_char = chr(key, key_ptr);
            child1 = copy_trie(t0);
            child1.run_start += match_len + 1;
            child2 = new_trie(key, key_ptr + 1, value);
        } else {
            num_children = chr(t0.key, run0_ptr) - chr(key, key_ptr) + 1;
            t = new Trie();
            t.children = new Trie[num_children];
            t.first_char = chr(key, key_ptr);
            t.last_char = chr(t0.key, run0_ptr);
            child1 = new_trie(key, key_ptr + 1, value);
            child2 = copy_trie(t0);
            child2.run_start += match_len + 1;
        }
        set_run(key, key_ptr - match_len, match_len, t);
        t.has_value = false;
        t.value = null;

        for (i = 0; i < num_children; i++) {
            t.children[i] = null;
        }
        t.children[0] = child1;
        t.children[t.last_char - t.first_char] = child2;
        return t;
    }
}

private static int count_children(Trie t)
{
    char    c;
    int     count = 0;

    if (t.first_char == '\\0') {
        return 0;
    } else {
        for (c = t.first_char; c <= t.last_char; c++) {
            if (t.children[c - t.first_char] != null) {
                count++;
            }
        }
    }

    return count;
}

private static Trie fixup_children(Trie t) {
    char    lower_bound = '\\0';
    char    upper_bound = '\\0';
    boolean found_lower_bound;
    boolean found_upper_bound;
    char    c;
    int     shift;
    int     count = 0;
    int     run_len;

    if (t.first_char != '\\0') {
        found_upper_bound = false;
        found_lower_bound = false;
        for (c = t.first_char; c <= t.last_char; c++) {
            if (t.children[c - t.first_char] != null) {
                if (!found_lower_bound || c < lower_bound) {
                    lower_bound = c;
                    found_lower_bound = true;
                }
                if (!found_upper_bound || c > upper_bound) {
                    upper_bound = c;
                    found_upper_bound = true;
                }
                count++;
            }
        }
        if (count == 0) {
            if (t.has_value == false) {
                return null;
            } else {
                t.first_char = '\\0';
                t.last_char = '\\0';
                return t;
            }
        } if (count == 1 && t.has_value == false) {
            run_len = t.run_end - t.run_start;
            t = copy_trie(t.children[lower_bound - t.first_char]);
            t.run_start -= run_len + 1;
            return t;
        } else {
            shift = lower_bound - t.first_char;
            t.first_char = lower_bound;
            t.last_char = upper_bound;
            if (shift > 0) {
                for (c = lower_bound; c <= upper_bound; c++) {
                    t.children[c - lower_bound] = 
                        t.children[c - lower_bound + shift];
                }
            }
            return t;
        }
    } else {
        if (t.has_value == false) {
            return null;
        } else {
            return t;
        }
    }
}

public static Trie trie_delete(String key, int key_ptr, Trie t0)
{
    Trie    t;
    Trie    child;
    int     num_children;
    int     run_len;
    int     run_len0;
    int     child_run_len;
    int     run0_ptr;

    if (t0 == null) {
        return null;
    }

    run0_ptr = t0.run_start;
    while (run0_ptr != t0.run_end && chr(t0.key, run0_ptr) == chr(key, key_ptr)) {
        run0_ptr++;
        key_ptr++;
    }
    if (run0_ptr != t0.run_end) {
        return t0;
    }

    if (chr(key, key_ptr) == '\\0') {
        num_children = count_children(t0);
        if (num_children == 0) {
            return null;
        } else if (num_children == 1) {
            t = copy_trie(t0.children[0]);
            t.run_start -= run0_ptr - t0.run_start + 1;
            return t;
        } else {
            t = copy_trie(t0);
            t.has_value = false;
            t.value = null;
            return t;
        }
    }

    if (chr(key, key_ptr) >= t0.first_char && chr(key, key_ptr) <= t0.last_char) {
        child = t0.children[chr(key, key_ptr) - t0.first_char];
        if (child == null) {
            return t0;
        } else {
            t = copy_trie(t0);
            child = trie_delete(key, key_ptr + 1, child);
            t.children[chr(key, key_ptr) - t0.first_char] = child;
            if (child == null) {
                t = fixup_children(t);
            }
            return t;
        }
    } else {
        return t0;
    }
}

public static int trie_count(Trie t)
{
    char            c;
    Trie            child;
    int             count;

    if (t == null) {
        return 0;
    } else {
        if (t.has_value) {
            count = 1;
        } else {
            count = 0;
        }
        if (t.first_char != '\\0') {
            for (c = t.first_char; c <= t.last_char; c++) {
                child = t.children[c - t.first_char];
                if (child != null) {
                    count += trie_count(child);
                }
            }
        }
        return count;
    }
}

private static void print_spaces(int ind)
{
    int i;
    for (i = 0; i < ind; i++) {
        System.out.print(' ');
    }
}

public static void trie_dump(int ind, Trie t)
{
    char    c;
    Trie    child;
    String  run;

    if (t != null) {
        print_spaces(ind);
        run = t.key.substring(t.run_start, t.run_end);
        System.out.print(\"\\\"\" + run + \"\\\"\");
        if (t.has_value) {
            System.out.print('*');
        }
        System.out.print(\":\\n\");

        if (t.first_char != '\\0') {
            for (c = t.first_char; c <= t.last_char; c++) {
                child = t.children[c - t.first_char];
                if (child != null) {
                    print_spaces(ind + 2);
                    System.out.print(c);
                    System.out.print(\":\\n\");
                    trie_dump(ind + 4, child);
                }
            }
        }
    }
}

").

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", trie(V), "trie_t *",
    [can_pass_as_mercury_type, stable])
    where comparison is trie_compare.

    % 10.04 compiler does not recognise this
:- pragma foreign_type("C#", trie(V), "trie.Trie",
    [can_pass_as_mercury_type])
    where comparison is trie_compare.

:- pragma foreign_type("Java", trie(V), "jmercury.trie.Trie",
    [can_pass_as_mercury_type])
    where comparison is trie_compare.

:- pragma foreign_proc("C", (init = (Trie::out)),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_duplicate],
"
    Trie = NULL;
").

:- pragma foreign_proc("C#", (init = (Trie::out)),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_not_duplicate],
"
    Trie = null;
").

:- pragma foreign_proc("Java", (init = (Trie::out)),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_not_duplicate],
"
    Trie = null;
").

:- pragma foreign_proc("C", search(Trie::in, Key::in, Val::out),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_duplicate],
"
    SUCCESS_INDICATOR = trie_search(Trie, Key, &Val);
").

:- pragma foreign_proc("C#", search(Trie::in, Key::in, Val::out),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_not_duplicate],
"
    SUCCESS_INDICATOR = trie_search(Trie, Key, out Val);
").

:- pragma foreign_proc("Java", search(Trie::in, Key::in, Val::out),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_not_duplicate],
"
    Ref<Object> v = new Ref<Object>();
    //trie_dump(0, Trie);
    SUCCESS_INDICATOR = trie_search(Trie, Key, v);
    Val = v.val;
").

:- pragma foreign_proc("C", search_shortest_prefix(Trie::in, Key::in,
        Prefix::out, Val::out),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_duplicate],
"
    SUCCESS_INDICATOR = trie_search_shortest_prefix(Trie, Key, &Prefix, &Val);
").

:- pragma foreign_proc("C#", search_shortest_prefix(Trie::in, Key::in,
        Prefix::out, Val::out),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_not_duplicate],
"
    SUCCESS_INDICATOR = trie_search_shortest_prefix(Trie, Key,
        out Prefix, out Val);
").

:- pragma foreign_proc("Java", search_shortest_prefix(Trie::in, Key::in,
        Prefix::out, Val::out),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_not_duplicate],
"
    Ref<String> p = new Ref<String>();
    Ref<Object> v = new Ref<Object>();
    SUCCESS_INDICATOR = trie_search_shortest_prefix(Trie, Key, p, v);
    Prefix = p.val;
    Val = v.val;
").

:- pragma foreign_proc("C", search_shortest_prefix_with_min(Trie::in, 
        Key::in, Min::in, Prefix::out, Val::out),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_duplicate],
"
    SUCCESS_INDICATOR = trie_search_shortest_prefix_with_min(Trie, Key,
        Min, &Prefix, &Val);
").

:- pragma foreign_proc("C#", search_shortest_prefix_with_min(Trie::in, 
        Key::in, Min::in, Prefix::out, Val::out),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_not_duplicate],
"
    SUCCESS_INDICATOR = trie_search_shortest_prefix_with_min(Trie, Key,
        Min, out Prefix, out Val);
").

:- pragma foreign_proc("Java", search_shortest_prefix_with_min(Trie::in, 
        Key::in, Min::in, Prefix::out, Val::out),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_not_duplicate],
"
    Ref<String> p = new Ref<String>();
    Ref<Object> v = new Ref<Object>();
    SUCCESS_INDICATOR = trie_search_shortest_prefix_with_min(Trie, Key,
        Min, p, v);
    Val = v.val;
    Prefix = p.val;
").

:- pragma foreign_proc("C", set(Key::in, Val::in, Trie0::in, Trie::out),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_duplicate],
"
    Trie = trie_set(Key, Key, Val, Trie0);
").

:- pragma foreign_proc("C#", set(Key::in, Val::in, Trie0::in, Trie::out),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_not_duplicate],
"
    Trie = trie_set(Key, 0, Val, Trie0);
").

:- pragma foreign_proc("Java", set(Key::in, Val::in, Trie0::in, Trie::out),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_not_duplicate],
"
    Trie = trie_set(Key, 0, Val, Trie0);
").

:- pragma foreign_proc("C", delete(Key::in, Trie0::in, Trie::out),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_duplicate],
"
    Trie = trie_delete(Key, Trie0);
").

:- pragma foreign_proc("C#", delete(Key::in, Trie0::in, Trie::out),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_not_duplicate],
"
    Trie = trie_delete(Key, 0, Trie0);
").

:- pragma foreign_proc("Java", delete(Key::in, Trie0::in, Trie::out),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_not_duplicate],
"
    Trie = trie_delete(Key, 0, Trie0);
").

:- pragma foreign_proc("C", dump(Trie::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_duplicate],
"
    trie_dump(0, Trie);
").

:- pragma foreign_proc("C#", dump(Trie::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_not_duplicate],
"
    trie_dump(0, Trie);
").

:- pragma foreign_proc("Java", dump(Trie::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_not_duplicate],
"
    trie_dump(0, Trie);
").

:- pragma foreign_proc("C", (count(Trie::in) = (Count::out)),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_duplicate],
"
    Count = trie_count(Trie);
").

:- pragma foreign_proc("C#", (count(Trie::in) = (Count::out)),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_not_duplicate],
"
    Count = trie_count(Trie);
").

:- pragma foreign_proc("Java", (count(Trie::in) = (Count::out)),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_not_duplicate],
"
    Count = trie_count(Trie);
").

:- pragma foreign_proc("C", is_empty(Trie::in),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_duplicate],
"
    SUCCESS_INDICATOR = (Trie == NULL);
").

:- pragma foreign_proc("C#", is_empty(Trie::in),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_duplicate],
"
    SUCCESS_INDICATOR = (Trie == null);
").

:- pragma foreign_proc("Java", is_empty(Trie::in),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_duplicate],
"
    SUCCESS_INDICATOR = (Trie == null);
").

    % Returns the Nth entry of the child array of a trie.  Does not do bounds
    % checking.
    %
:- func unsafe_child(trie(V), int) = trie(V).

:- pragma foreign_proc("C", (unsafe_child(Trie::in, N::in) = (Child::out)),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_duplicate],
"
    Child = Trie->children[N];
").

:- pragma foreign_proc("C#", (unsafe_child(Trie::in, N::in) = (Child::out)),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_not_duplicate],
"
    Child = Trie.children[N];
").

:- pragma foreign_proc("Java", (unsafe_child(Trie::in, N::in) = (Child::out)),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_not_duplicate],
"
    Child = Trie.children[N];
").

    % Returns the number of slots in the children array of a trie.
    %
:- func num_child_slots(trie(V)) = int.

:- pragma foreign_proc("C", (num_child_slots(Trie::in) = (N::out)),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_duplicate],
"
    if (Trie == NULL) {
        N = 0;
    } else if (Trie->first_char == '\\0') {
        N = 0;
    } else {
        N = Trie->last_char - Trie->first_char + 1;
    }
").

:- pragma foreign_proc("C#", (num_child_slots(Trie::in) = (N::out)),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_not_duplicate],
"
    if (Trie == null) {
        N = 0;
    } else if (Trie.first_char == '\\0') {
        N = 0;
    } else {
        N = Trie.last_char - Trie.first_char + 1;
    }
").

:- pragma foreign_proc("Java", (num_child_slots(Trie::in) = (N::out)),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_not_duplicate],
"
    if (Trie == null) {
        N = 0;
    } else if (Trie.first_char == '\\0') {
        N = 0;
    } else {
        N = Trie.last_char - Trie.first_char + 1;
    }
").

    % Return the key and value associated with a trie node.
    % Fails if there is no value associated with the node.
    %
:- pred has_value(trie(V)::in, string::out, V::out) is semidet.

:- pragma foreign_proc("C", has_value(Trie::in, Key::out, Val::out),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_duplicate],
"
    if (Trie == NULL) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        Val = Trie->value;
        Key = Trie->key;
        SUCCESS_INDICATOR = Trie->has_value;
    }
").

:- pragma foreign_proc("C#", has_value(Trie::in, Key::out, Val::out),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_not_duplicate],
"
    if (Trie == null) {
        Val = null;
        Key = null;
        SUCCESS_INDICATOR = false;
    } else {
        Val = Trie.val;
        Key = Trie.key;
        SUCCESS_INDICATOR = Trie.has_value;
    }
").

:- pragma foreign_proc("Java", has_value(Trie::in, Key::out, Val::out),
    [promise_pure, will_not_call_mercury, thread_safe, terminates,
     will_not_throw_exception, will_not_modify_trail, does_not_affect_liveness,
     may_not_duplicate],
"
    if (Trie == null) {
        Val = null;
        Key = null;
        SUCCESS_INDICATOR = false;
    } else {
        Val = Trie.value;
        Key = Trie.key;
        SUCCESS_INDICATOR = Trie.has_value;
    }
").

%-----------------------------------------------------------------------------%

:- pred trie_compare(comparison_result::uo, trie(V)::in, trie(V)::in) is det.

trie_compare(Result, T1, T2) :-
    ( has_value(T1, K1, V1) ->
        ( has_value(T2, K2, V2) ->
            builtin.compare(KResult, K1, K2),
            ( KResult = (=) ->
                builtin.compare(VResult, V1, V2),
                ( VResult = (=) ->
                    compare_children(T1, T2, Result)
                ;
                    Result = VResult
                )
            ;
                Result = KResult
            )
        ;
            Result = (>)
        )
    ;
        ( has_value(T2, _, _) ->
            Result = (<)
        ;
            compare_children(T1, T2, Result)
        )
    ).

:- pred compare_children(trie(V)::in, trie(V)::in, comparison_result::uo)
    is det.

compare_children(T1, T2, Result) :-
    N1 = num_child_slots(T1),
    N2 = num_child_slots(T2),
    ( N1 > N2 ->
        Result = (>)
    ; N1 < N2 ->
        Result = (<)
    ; N1 = 0 ->
        Result = (=)
    ;
        compare_children_2(T1, T2, 0, N1 - 1, Result)
    ).

:- pred compare_children_2(trie(V)::in, trie(V)::in, int::in, int::in,
    comparison_result::uo) is det.

compare_children_2(T1, T2, I, N, Result) :-
    ( I =< N ->
        Child1 = unsafe_child(T1, I),
        Child2 = unsafe_child(T2, I),
        trie_compare(Result0, Child1, Child2),
        ( Result0 = (=) ->
            compare_children_2(T1, T2, I + 1, N, Result)
        ;
            Result = Result0
        )
    ;
        Result = (=)
    ).

%-----------------------------------------------------------------------------%

foldl(Pred, Trie, !A) :-
    ( has_value(Trie, Key, Val) ->
        Pred(Key, Val, !A)
    ;
        true
    ),
    foldl_2(Pred, Trie, 0, num_child_slots(Trie) - 1, !A).

:- pred foldl_2(pred(string, V, A, A), trie(V), int, int, A, A).
:- mode foldl_2(in(pred(in, in, in, out) is det), in, in, in, in, out)
    is det.
:- mode foldl_2(in(pred(in, in, in, out) is semidet), in, in, in, in, out)
    is semidet.
:- mode foldl_2(in(pred(in, in, di, uo) is det), in, in, in, di, uo)
    is det.
:- mode foldl_2(in(pred(in, in, in, out) is cc_multi), in, in, in, in, out)
    is cc_multi.
:- mode foldl_2(in(pred(in, in, di, uo) is cc_multi), in, in, in, di, uo)
    is cc_multi.

foldl_2(Pred, Trie, I, N, !A) :-
    ( I =< N ->
        Child = unsafe_child(Trie, I),
        ( has_value(Child, Key, Val) ->
            Pred(Key, Val, !A)
        ;
            true
        ),
        foldl_2(Pred, Child, 0, num_child_slots(Child) - 1, !A),
        foldl_2(Pred, Trie, I + 1, N, !A)
    ;
        true
    ).

foldl2(Pred, Trie, !A, !B) :-
    ( has_value(Trie, Key, Val) ->
        Pred(Key, Val, !A, !B)
    ;
        true
    ),
    foldl2_2(Pred, Trie, 0, num_child_slots(Trie) - 1, !A, !B).

:- pred foldl2_2(pred(string, V, A, A, B, B), trie(V), int, int, A, A, B, B).
:- mode foldl2_2(in(pred(in, in, in, out, in, out) is det), in, in, in,
    in, out, in, out) is det.
:- mode foldl2_2(in(pred(in, in, in, out, in, out) is semidet), in, in, in,
    in, out, in, out) is semidet.
:- mode foldl2_2(in(pred(in, in, in, out, di, uo) is det), in, in, in,
    in, out, di, uo) is det.
:- mode foldl2_2(in(pred(in, in, in, out, in, out) is cc_multi), in, in, in,
    in, out, in, out) is cc_multi.
:- mode foldl2_2(in(pred(in, in, in, out, di, uo) is cc_multi), in, in, in,
    in, out, di, uo) is cc_multi.

foldl2_2(Pred, Trie, I, N, !A, !B) :-
    ( I =< N ->
        Child = unsafe_child(Trie, I),
        ( has_value(Child, Key, Val) ->
            Pred(Key, Val, !A, !B)
        ;
            true
        ),
        foldl2_2(Pred, Child, 0, num_child_slots(Child) - 1, !A, !B),
        foldl2_2(Pred, Trie, I + 1, N, !A, !B)
    ;
        true
    ).

foldl3(Pred, Trie, !A, !B, !C) :-
    ( has_value(Trie, Key, Val) ->
        Pred(Key, Val, !A, !B, !C)
    ;
        true
    ),
    foldl3_2(Pred, Trie, 0, num_child_slots(Trie) - 1, !A, !B, !C).

:- pred foldl3_2(pred(string, V, A, A, B, B, C, C), trie(V), int, int, A, A,
    B, B, C, C).
:- mode foldl3_2(in(pred(in, in, in, out, in, out, in, out) is det), in,
    in, in, in, out, in, out, in, out) is det.
:- mode foldl3_2(in(pred(in, in, in, out, in, out, in, out) is semidet),
    in, in, in, in, out, in, out, in, out) is semidet.
:- mode foldl3_2(in(pred(in, in, in, out, in, out, di, uo) is det), in, in,
    in, in, out, in, out, di, uo) is det.
:- mode foldl3_2(in(pred(in, in, in, out, in, out, in, out) is cc_multi),
    in, in, in, in, out, in, out, in, out) is cc_multi.
:- mode foldl3_2(in(pred(in, in, in, out, in, out, di, uo) is cc_multi),
    in, in, in, in, out, in, out, di, uo) is cc_multi.

foldl3_2(Pred, Trie, I, N, !A, !B, !C) :-
    ( I =< N ->
        Child = unsafe_child(Trie, I),
        ( has_value(Child, Key, Val) ->
            Pred(Key, Val, !A, !B, !C)
        ;
            true
        ),
        foldl3_2(Pred, Child, 0, num_child_slots(Child) - 1, !A, !B, !C),
        foldl3_2(Pred, Trie, I + 1, N, !A, !B, !C)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

foldl_while(Pred, Trie, Complete, !A) :-
    ( has_value(Trie, Key, Val) ->
        Pred(Key, Val, Continue, !A)
    ;
        Continue = yes
    ),
    foldl_while_2(Pred, Trie, 0, num_child_slots(Trie) - 1, Continue,
        Complete, !A).

:- pred foldl_while_2(pred(string, V, bool, A, A), trie(V), int, int,
    bool, bool, A, A).
:- mode foldl_while_2(in(pred(in, in, out, in, out) is det), in, in, in, in,
    out, in, out) is det.
:- mode foldl_while_2(in(pred(in, in, out, di, uo) is det), in, in, in, in,
    out, di, uo) is det.

foldl_while_2(Pred, Trie, I, N, !Continue, !A) :-
    ( I =< N, !.Continue = yes ->
        Child = unsafe_child(Trie, I),
        ( has_value(Child, Key, Val) ->
            Pred(Key, Val, !:Continue, !A)
        ;
            true
        ),
        foldl_while_2(Pred, Child, 0, num_child_slots(Child) - 1, !Continue,
            !A),
        foldl_while_2(Pred, Trie, I + 1, N, !Continue, !A)
    ;
        true
    ).

foldl2_while(Pred, Trie, Complete, !A, !B) :-
    ( has_value(Trie, Key, Val) ->
        Pred(Key, Val, Continue, !A, !B)
    ;
        Continue = yes
    ),
    foldl2_while_2(Pred, Trie, 0, num_child_slots(Trie) - 1, Continue,
        Complete, !A, !B).

:- pred foldl2_while_2(pred(string, V, bool, A, A, B, B), trie(V), int, int,
    bool, bool, A, A, B, B).
:- mode foldl2_while_2(in(pred(in, in, out, in, out, in, out) is det),
    in, in, in, in, out, in, out, in, out) is det.
:- mode foldl2_while_2(in(pred(in, in, out, in, out, di, uo) is det),
    in, in, in, in, out, in, out, di, uo) is det.

foldl2_while_2(Pred, Trie, I, N, !Continue, !A, !B) :-
    ( I =< N, !.Continue = yes ->
        Child = unsafe_child(Trie, I),
        ( has_value(Child, Key, Val) ->
            Pred(Key, Val, !:Continue, !A, !B)
        ;
            true
        ),
        foldl2_while_2(Pred, Child, 0, num_child_slots(Child) - 1, !Continue,
            !A, !B),
        foldl2_while_2(Pred, Trie, I + 1, N, !Continue, !A, !B)
    ;
        true
    ).

foldl3_while(Pred, Trie, Complete, !A, !B, !C) :-
    ( has_value(Trie, Key, Val) ->
        Pred(Key, Val, Continue, !A, !B, !C)
    ;
        Continue = yes
    ),
    foldl3_while_2(Pred, Trie, 0, num_child_slots(Trie) - 1, Continue,
        Complete, !A, !B, !C).

:- pred foldl3_while_2(pred(string, V, bool, A, A, B, B, C, C), trie(V),
    int, int, bool, bool, A, A, B, B, C, C).
:- mode foldl3_while_2(in(pred(in, in, out, in, out, in, out, in, out) is det),
    in, in, in, in, out, in, out, in, out, in, out) is det.
:- mode foldl3_while_2(in(pred(in, in, out, in, out, in, out, di, uo) is det),
    in, in, in, in, out, in, out, in, out, di, uo) is det.

foldl3_while_2(Pred, Trie, I, N, !Continue, !A, !B, !C) :-
    ( I =< N, !.Continue = yes ->
        Child = unsafe_child(Trie, I),
        ( has_value(Child, Key, Val) ->
            Pred(Key, Val, !:Continue, !A, !B, !C)
        ;
            true
        ),
        foldl3_while_2(Pred, Child, 0, num_child_slots(Child) - 1, !Continue,
            !A, !B, !C),
        foldl3_while_2(Pred, Trie, I + 1, N, !Continue, !A, !B, !C)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

keys(Trie) = Keys :-
    trie.foldl(add_key, Trie, [], Keys).

:- pred add_key(string::in, V::in, list(string)::in, list(string)::out) is det.

add_key(K, _, Ks, [K | Ks]).

values(Trie) = Values :-
    trie.foldl(add_value, Trie, [], Values).

:- pred add_value(string::in, V::in, list(V)::in, list(V)::out) is det.

add_value(_, V, Vs, [V | Vs]).

%-----------------------------------------------------------------------------%

register_trie_pickle(!Pickles) :-
    pickle.register_pickle(type_ctor(type_of(_:trie(int))), 
        pickle_univ_trie, !Pickles).

:- pred pickle_univ_trie(pickles::in, univ::in,
    byte_buffer::di, byte_buffer::uo) is det.

pickle_univ_trie(Pickles, Univ, !ByteBuffer) :-
    TypeDesc = univ_type(Univ),
    type_ctor_and_args(TypeDesc, _, Args),
    ( Args = [ArgTypeDesc] ->
        has_type(_:V, ArgTypeDesc),
        det_univ_to_type(Univ, Trie:trie(V)),
        trie.pickle(Pickles, Trie, !ByteBuffer)
    ;
        error("pickle: expected one argument for type trie")
    ).

    % Pickle a trie to a byte buffer.
    %
:- pred pickle(pickles::in, trie(V)::in, byte_buffer::di, byte_buffer::uo)
    is det.

pickle(Pickles, Trie, !ByteBuffer) :-
    add_int_to_byte_buffer(trie.count(Trie), !ByteBuffer),
    trie.foldl(pickle_trie_key_value(Pickles), Trie, !ByteBuffer).

:- pred pickle_trie_key_value(pickles::in, string::in, V::in, byte_buffer::di,
    byte_buffer::uo) is det.

pickle_trie_key_value(Pickles, Key, Val, !ByteBuffer) :-
    add_string_to_byte_buffer(Key, !ByteBuffer),
    pickle(Pickles, Val, !ByteBuffer).

%-----------------------------------------------------------------------------%

register_trie_unpickle(!UnPickles) :-
    pickle.register_unpickle(type_ctor(type_of(_:trie(int))),
        unpickle_univ_trie, !UnPickles).

:- pred unpickle_univ_trie(unpickles::in, type_desc::in, univ::out, 
    byte_buffer::di, byte_buffer::uo) is det.

unpickle_univ_trie(Unpickles, TypeDesc, univ(Trie), !ByteBuffer) :-
    type_ctor_and_args(TypeDesc, _, Args),
    ( Args = [ArgTypeDesc] ->
        has_type(_:V, ArgTypeDesc),
        trie.unpickle(Unpickles, Trie:trie(V), !ByteBuffer)
    ;
        error("unpickle: expected one argument for type trie")
    ).

    % Unpickle a trie from a byte buffer.
    %
:- pred unpickle(unpickles::in, trie(V)::out, byte_buffer::di, byte_buffer::uo)
    is det.

unpickle(UnPickles, Trie, !ByteBuffer) :-
    det_get_int_from_byte_buffer(Count, !ByteBuffer),
    int.fold_up2(unpickle_trie_key_value(UnPickles), 1, Count, trie.init,
        Trie, !ByteBuffer).

:- pred unpickle_trie_key_value(unpickles::in, int::in,
    trie(V)::in, trie(V)::out, byte_buffer::di, byte_buffer::uo) is det.

unpickle_trie_key_value(UnPickles, _, !Trie, !ByteBuffer) :-
    det_get_string_from_byte_buffer(Key, !ByteBuffer),
    unpickle(UnPickles, Val, !ByteBuffer),
    trie.set(Key, Val, !Trie).

%-----------------------------------------------------------------------------%

:- pred det_get_int_from_byte_buffer(int::out,
    byte_buffer::di, byte_buffer::uo) is det.

det_get_int_from_byte_buffer(Int, !ByteBuffer) :-
    get_int_from_byte_buffer(Res, !ByteBuffer),
    ( Res = ok(Int)
    ; Res = eof,
        throw(Res)
    ; Res = error(_),
        throw(Res)
    ).

:- pred det_get_byte_from_byte_buffer(int::out,
    byte_buffer::di, byte_buffer::uo) is det.

det_get_byte_from_byte_buffer(Byte, !ByteBuffer) :-
    get_byte_from_byte_buffer(Res, !ByteBuffer),
    ( Res = ok(Byte)
    ; Res = eof,
        throw(Res)
    ; Res = error(_),
        throw(Res)
    ).

:- pred det_get_string_from_byte_buffer(string::out,
    byte_buffer::di, byte_buffer::uo) is det.

det_get_string_from_byte_buffer(Str, !ByteBuffer) :-
    get_string_from_byte_buffer(Res, !ByteBuffer),
    ( Res = ok(Str)
    ; Res = eof,
        throw(Res)
    ; Res = error(_),
        throw(Res)
    ).

%-----------------------------------------------------------------------------%
:- end_module trie.
%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et fileencoding=utf8
% -*- coding:utf8; -*-
