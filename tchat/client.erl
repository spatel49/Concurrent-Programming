% I pledge my honor that I have abided by the Stevens Honor System
% Siddhanth Patel and Fabricio Flores
-module(client).

-export([main/1, initial_state/2]).

-include_lib("./defs.hrl").

-spec main(_InitialState) -> _.
-spec listen(_State) -> _.
-spec initial_state(_Nick, _GuiName) -> _InitialClientState.
-spec loop(_State, _Request, _Ref) -> _.
-spec do_join(_State, _Ref, _ChatName) -> _.
-spec do_leave(_State, _Ref, _ChatName) -> _.
-spec do_new_nick(_State, _Ref, _NewNick) -> _.
-spec do_new_incoming_msg(_State, _Ref, _SenderNick, _ChatName, _Message) -> _.

%% Receive messages from GUI and handle them accordingly
%% All handling can be done in loop(...)
main(InitialState) ->
    %% The client tells the server it is connecting with its initial nickname.
    %% This nickname is guaranteed unique system-wide as long as you do not assign a client
    %% the nickname in the form "user[number]" manually such that a new client happens
    %% to generate the same random number as you assigned to your client.
    whereis(server)!{self(), connect, InitialState#cl_st.nick},
    %% if running test suite, tell test suite that client is up
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{client_up, self()}
    end,
    %% Begins listening
    listen(InitialState).

%% This method handles all incoming messages from either the GUI or the
%% chatrooms that are not directly tied to an ongoing request cycle.
listen(State) ->
    receive
        {request, From, Ref, Request} ->
	    %% the loop method will return a response as well as an updated
	    %% state to pass along to the next cycle
            {Response, NextState} = loop(State, Request, Ref),
	    case Response of
		{dummy_target, Resp} ->
		    io:format("Use this for whatever you would like~n"),
		    From!{result, self(), Ref, {dummy_target, Resp}},
		    listen(NextState);
		%% if shutdown is received, terminate
		shutdown ->
		    ok_shutdown;
		%% if ok_msg_received, then we don't need to reply to sender.
		ok_msg_received ->
		    listen(NextState);
		%% otherwise, reply to sender with response
		_ ->
		    From!{result, self(), Ref, Response},
		    listen(NextState)
	    end
    end.

%% This function just initializes the default state of a client.
%% This should only be used by the GUI. Do not change it, as the
%% GUI code we provide depends on it.
initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick, con_ch = maps:new() }.

%% ------------------------------------------
%% loop handles each kind of request from GUI
%% ------------------------------------------
loop(State, Request, Ref) ->
    case Request of
	%% GUI requests to join a chatroom with name ChatName
	{join, ChatName} ->
	    do_join(State, Ref, ChatName);

	%% GUI requests to leave a chatroom with name ChatName
	{leave, ChatName} ->
	    do_leave(State, Ref, ChatName);

	%% GUI requests to send an outgoing message Message to chatroom ChatName
	{outgoing_msg, ChatName, Message} ->
	    do_msg_send(State, Ref, ChatName, Message);

	%% GUI requests the nickname of client
	whoami ->
		{State#cl_st.nick, State};

	%% GUI requests to update nickname to Nick
	{nick, Nick} ->
            do_new_nick(State, Ref, Nick);

	%% GUI requesting to quit completely
	quit ->
	    do_quit(State, Ref);

	%% Chatroom with name ChatName has sent an incoming message Message
	%% from sender with nickname SenderNick
	{incoming_msg, SenderNick, ChatName, Message} ->
	    do_new_incoming_msg(State, Ref, SenderNick, ChatName, Message);
	
	{get_state} ->
	    {{get_state, State}, State};

	%% Somehow reached a state where we have an unhandled request.
	%% Without bugs, this should never be reached.
	_ ->
	    io:format("Client: Unhandled Request: ~w~n", [Request]),
	    {unhandled_request, State}
    end.

%% executes `/join` protocol from client perspective
do_join(State, Ref, ChatName) ->
	S = whereis(server),
    G = whereis(list_to_atom(State#cl_st.gui)),
	ChatRooms = State#cl_st.con_ch,
	case maps:is_key(ChatName, ChatRooms) of
		true ->
			G!{result, self(), Ref, err},
			UpdatedRooms = State#cl_st.con_ch;
		false ->
			S!{self(), Ref, join, ChatName},
			receive
				{ChatPid, Ref, connect, ChatHistory} ->
					UpdatedRooms = maps:put(ChatName, ChatPid, State#cl_st.con_ch),
					G!{result, self(), Ref, ChatHistory}
			end
	end,
	{ok_msg_received, #cl_st{
		gui = State#cl_st.gui,
		nick = State#cl_st.nick,
		con_ch = UpdatedRooms
	}}.


%% executes `/leave` protocol from client perspective
do_leave(State, Ref, ChatName) ->
	S = whereis(server),
	G = whereis(list_to_atom(State#cl_st.gui)),
	case maps:is_key(ChatName, State#cl_st.con_ch) of
		false ->
			G!{result, self(), Ref, err},
			{ok, State#cl_st{con_ch = maps:remove(ChatName, State#cl_st.con_ch)}};
		true ->
			S!{self(), Ref, leave, ChatName},
			receive
				{_From, Ref, ack_leave} ->
					G!{result, self(), Ref, ok},
					{ok, State#cl_st{con_ch = maps:remove(ChatName, State#cl_st.con_ch)}}
			end
	end.

%% executes `/nick` protocol from client perspective
do_new_nick(State, Ref, NewNick) ->
    S = whereis(server),
	case NewNick == State#cl_st.nick of
		false -> 
			S!{self(), Ref, nick, NewNick},
			receive
				{_From, Ref, ok_nick} ->
					{ok_nick, State#cl_st{nick = NewNick}};
				{_From, Ref, err_nick_used} ->
					{err_nick_used, State}
			end;
		true ->
			{err_same, State}
	end.
			

%% executes send message protocol from client perspective
do_msg_send(State, Ref, ChatName, Message) ->
    Chatroom = maps:get(ChatName, State#cl_st.con_ch),
	Chatroom!{self(), Ref, message, Message},
	GuiPID = whereis(list_to_atom(State#cl_st.gui)),
	receive
		{_Chat, Ref, ack_msg} -> GuiPID!{result, self(), Ref, {msg_sent, State#cl_st.nick}},
		{ok, State}
	end.


%% executes new incoming message protocol from client perspective
do_new_incoming_msg(State, _Ref, CliNick, ChatName, Msg) ->
    %% pass message along to gui
    gen_server:call(list_to_atom(State#cl_st.gui), {msg_to_GUI, ChatName, CliNick, Msg}),
    {ok_msg_received, State}.

%% executes quit protocol from client perspective
do_quit(State, Ref) ->
	GuiPID = whereis(list_to_atom(State#cl_st.gui)),
	Server = whereis(server),
	Server!{self(),Ref, quit},
	receive
		{_From, Ref, ack_quit} ->
			GuiPID!{self(), Ref, ack_quit}
	end,
	{ok, State}.