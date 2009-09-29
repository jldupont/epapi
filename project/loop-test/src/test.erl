%% Author: jldupont
%% Created: 2009-09-29

-module(test).

-define(TIMEOUT, 2000).
-define(DRV, "epapi_loop_drv").


%%
%% Exported Functions
%%
-export([
		 go/0
		 ]).

go() ->
	{ok, Cwd}=file:get_cwd(),
	io:format("expecting driver in path: ~p~n", [Cwd]),
	start_drv(),
	loop().


loop() ->
	receive
		{port, Port} ->
			put(port, Port);
		
		{_Port, {exit_status, Reason}} ->
			exit(Reason);
		
		{_Port, {data, Data}} ->
			io:format("data: ~p~n", [Data]);
			
		Other ->
			io:format("Other: ~p~n", [Other])
	after ?TIMEOUT ->
		
		Port=get(port),
		Port ! {command, {test}}
		  
	end,
	loop().
	

start_drv() ->
	{ok, Cwd}=file:get_cwd(),
	Drv=Cwd++"/"++?DRV,
	Port = open_port({spawn, Drv}, [{packet, 2}, binary, exit_status]),
	self() ! {port, Port}.	


