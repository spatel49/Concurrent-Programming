% I pledge my honor that I have abided by the Stevens Honor System
% Siddhanth Patel and Fabricio Flores
-module(server).

-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch(unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{server_up, self()}
    end,
    loop(
      #serv_st{
	 nicks = maps:new(), %% nickname map. client_pid => "nickname"
	 registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
	 chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
	}
     ).

loop(State) ->
    receive 
	%% initial connection
	{ClientPID, connect, ClientNick} ->
	    NewState =
		#serv_st{
		   nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
		   registrations = State#serv_st.registrations,
		   chatrooms = State#serv_st.chatrooms
		  },
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, join, ChatName} ->
	    NewState = do_join(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, leave, ChatName} ->
	    NewState = do_leave(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to register a new nickname
	{ClientPID, Ref, nick, NewNick} ->
	    NewState = do_new_nick(State, Ref, ClientPID, NewNick),
	    loop(NewState);
	%% client requests to quit
	{ClientPID, Ref, quit} ->
	    NewState = do_client_quit(State, Ref, ClientPID),
	    loop(NewState);
	{TEST_PID, get_state} ->
	    TEST_PID!{get_state, State},
	    loop(State)
    end.

%% executes join protocol from server perspective
do_join(ChatName, ClientPID, Ref, State) ->
	ClientNick = maps:get(ClientPID, State#serv_st.nicks),
    case maps:is_key(ChatName, State#serv_st.chatrooms) of
		true ->
			ChatPID = maps:get(ChatName, State#serv_st.chatrooms),
			ChatPID!{self(), Ref, register, ClientPID, ClientNick},
			PrevReg = maps:get(ChatName, State#serv_st.registrations),
			State#serv_st{
				registrations = maps:put(ChatName, [ClientPID] ++ PrevReg, State#serv_st.registrations)
			};
		false ->
			ChatPID = spawn(chatroom, start_chatroom, [ChatName]),	
			ChatPID!{self(), Ref, register, ClientPID, ClientNick},
			State#serv_st{
				registrations = maps:put(ChatName, [ClientPID], State#serv_st.registrations),
			 	chatrooms = maps:put(ChatName, ChatPID, State#serv_st.chatrooms)
			}
	end.

%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
    ChatPID = maps:get(ChatName, State#serv_st.chatrooms),
	UpdatedRegistration = lists:delete(ClientPID, maps:get(ChatName, State#serv_st.registrations)),
    NewState = State#serv_st{
					registrations = maps:put(ChatName, UpdatedRegistration, State#serv_st.registrations)
				},
    ChatPID!{self(), Ref, unregister, ClientPID},
    ClientPID!{self(), Ref, ack_leave},
    NewState.

%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
    case lists:member(NewNick, maps:values(State#serv_st.nicks)) of
		true ->
			ClientPID!{self(), Ref, err_nick_used},
			State;
		false ->
			CurRooms = maps:filter(fun (_, ClientsReg) -> lists:member(ClientPID, ClientsReg) end, State#serv_st.registrations),
			lists:foreach(fun(ClientChatRoom) ->
				PID = maps:get(ClientChatRoom, State#serv_st.chatrooms),
				PID!{self(), Ref, update_nick, ClientPID, NewNick} 
			end, maps:keys(CurRooms)),
			ClientPID!{self(), Ref, ok_nick},
			State#serv_st{
				nicks = maps:put(ClientPID, NewNick, State#serv_st.nicks),
				registrations = State#serv_st.registrations,
				chatrooms = State#serv_st.chatrooms
			}
	end.

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
	UpdateState = State#serv_st{nicks = maps:remove(ClientPID, State#serv_st.nicks)},
    ChatNames = maps:keys(maps:filter(fun(_CN,ListOfClientPIDS) ->
				IsMember = lists:member(ClientPID, ListOfClientPIDS),
				IsMember == true 
				end, UpdateState#serv_st.registrations)),
	lists:map(fun(CName) ->
					ChatRoomPID = maps:get(CName,UpdateState#serv_st.chatrooms),
					ChatRoomPID!{self(), Ref, unregister, ClientPID}
				end, ChatNames),
	ClientPID!{self(), Ref, ack_quit},
	UpdateState#serv_st{
		registrations = maps:map( 
			fun(_ChatName,ListofPIDS) ->
				A = lists:member(ClientPID, ListofPIDS),
				if
					A == true -> 
						ListofPIDS -- [ClientPID];
					true ->
						ListofPIDS
				end
			end, UpdateState#serv_st.registrations)
	}.
