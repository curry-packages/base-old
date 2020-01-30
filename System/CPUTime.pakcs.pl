%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module System.CPUTime
%

:- (current_module(prologbasics) -> true ; use_module('../prologbasics')).

'System.CPUTime.getCPUTime'(MS) :- getRunTime(MS).

'System.CPUTime.getElapsedTime'(MS) :- getElapsedTime(MS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

