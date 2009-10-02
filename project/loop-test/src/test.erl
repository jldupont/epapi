%% Author: jldupont
%% Created: 2009-09-29

-module(test).

-define(TIMEOUT, 100).
-define(DRV, "/usr/bin/epapi_loop_drv").

-define(TESTDATA, {test, "test", [test, "test777", 1.0, "long string to possibly detect memory leaks faster............................................................................................................................................................................."]}).
%-define(TESTDATA, {test, "test"}).
%-define(TESTDATA, {test, [1,2,3,4,5]}).
%-define(TESTDATA, {test, [test]}).            %% generates NIL at the end
%-define(TESTDATA, {test, [test, 123456]}).
%-define(TESTDATA, {}).                        %% generates TUPLE size 0
%-define(TESTDATA, {{}}).                       %% TUPLE size 1, TUPLE size 0
%-define(TESTDATA, []).                        %% just generates NIL



%%
%% Exported Functions
%%
-export([
		 go/0
		 ]).

go() ->
	start_drv(),
	loop().


loop() ->
	receive
		{port, Port} ->
			io:format("Port: ~p~n", [Port]),
			put(port, Port);
		
		{_Port, {exit_status, Reason}} ->
			io:format("EXIT REASON: ~p~n", [Reason]),
			exit(Reason);
		
		{_Port, {data, Data}} ->
			Term=binary_to_term(Data),
			io:format("data: ~p~n", [Term]);
			
		Other ->
			io:format("Other: ~p~n", [Other])
	after ?TIMEOUT ->
		
		TestData=erlang:term_to_binary(?TESTDATA),
		Port=get(port),
		Port ! {self(), {command, TestData}}
		  
	end,
	loop().
	

start_drv() ->
	Port = open_port({spawn, ?DRV}, [{packet, 4}, binary, exit_status]),
	self() ! {port, Port}.	


