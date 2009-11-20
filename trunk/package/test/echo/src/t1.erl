%% Author: Jean-Lou Dupont
%% Created: 2009-06-03
-module(t1).

%%
%% API
%%
-export([start/0, start/1, stop/0, echo/1]).

%% internal
-export([init/2, loop/1]).

%%
%% Local Functions
%%

echo(X) ->
	?MODULE ! {echo, {X}}.

start() ->
    start("").

start(Param) ->
    spawn_link(?MODULE, init, ["/tmp/decho", Param]).

stop() ->
    ?MODULE ! stop.

init(ExtPrg, Param) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg++" "++Param}, [{packet, 2}, binary, exit_status]),
    loop(Port).

loop(Port) ->
    receive
		
		{echo,Msg} ->
			{Counter} = Msg,
			BinMsg = {echo, {Counter}},
			io:format("request to send 'echo' message [~p]~n", [BinMsg]),
			erlang:port_command(Port, term_to_binary(BinMsg));
		
		stop ->
			io:format("called [stop]"),
			erlang:port_close(Port),
			exit(normal);
		
		{Port, {data, Data}} ->
			io:format("Message raw[~p]~n", [Data]),
			Decoded = binary_to_term(Data),
			io:format("Message! Decoded[~p]~n", [Decoded]);
					
		Result ->
			Result
    end,
	loop(Port).
