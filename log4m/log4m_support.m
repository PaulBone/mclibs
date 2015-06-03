%-----------------------------------------------------------------------------%
% this module contains functionalities to support typed log4m message logging
% Copyright (c) Mission Critical Australia Pty Ltd
% MIT License
%-----------------------------------------------------------------------------%
:- module log4m_support.

:- interface.

% standard libs
:- import_module io.
:- import_module maybe.
:- import_module int.
:- import_module bool.

% libs
:- import_module log4m.
:- import_module application_exception.

%-----------------------------------------------------------------------------%
% These typeclasses are usefull to make typed log4m structures

:- typeclass logger(T) where [
    (func child(T) = maybe(some_logger)),
    (func node_id(T) = string)
].

:- type some_logger --->
            some [T] some_logger(T) => logger(T).

:- instance logger(some_logger).
 
    % use only this form of logging
    % as it constructs only the string on matching the level.
:- pred log_tf(T::in, level::in, ((func) = string)::in, io::di, io::uo) is det
        <= logger(T).

    % same as above but then with extra logging of the context location
:- pred log_tf(error_context::in, T::in, level::in, ((func) = string)::in, io::di, io::uo) is det
        <= logger(T).


:- pred will_log(T::in, level::in, bool::out, io::di, io::uo) is det
    <= logger(T).

    % use this function with care
:- func logger_id(T) = log4m.id <= logger(T).   

%-----------------------------------------------------------------------------%
% message functions
:- func space = string.
:- func nl = string.
:- func nl(int) = string.






:- implementation.

% standard libs
:- import_module list.
:- import_module string.

:- instance logger(some_logger) where [
    (child(some_logger(T)) = child(T)),
    (node_id(some_logger(T)) = node_id(T))
].



logger_id(T) = LOG:-
    CH = child(T),
    ( CH = no,
        LOG = [node_id(T)]
    ; CH = yes(C),
        LOG = logger_id(C) ++ [node_id(T)]
    ).


log_tf(T, Level, Func, !IO):-
    log_f(logger_id(T), Level, Func, !IO).

log_tf(Context, T, Level, Func, !IO):-
    log_f(logger_id(T), Level, ((func) = E:- apply(Func) = V, E = string.append(log_context_to_string(Context), V )), !IO).
    
log4m_support.will_log(T, Level, Bool, !IO):-
    log4m.will_log(logger_id(T), Level, Bool, !IO).


%-----------------------------------------------------------------------------%


space=" ".
nl = "\n".

nl(N) = string.duplicate_char('\n', N).
    

%-----------------------------------------------------------------------------%
:- end_module log4m_support.
% vim: ft=mercury ts=4 sw=4 et fileencoding=utf8
% -*- coding:utf8; -*-
