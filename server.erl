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
	IsIn = maps:is_key(ChatName,State#serv_st.chatrooms),
	Nickname = maps:get(ClientPID,State#serv_st.nicks),
	if
		IsIn ->
			ChatPid = maps:get(ChatName,State#serv_st.chatrooms),
			% ChatPid = maps:get(ChatName,ChatName,State#serv_st.chatrooms),
			ChatPid!{self(),Ref,register,ClientPID,Nickname},

			CpidList = maps:get(ChatName ,State#serv_st.registrations),
			NewList = lists:append([ClientPID],CpidList),
			NewMap = maps:update(ChatName,NewList,State#serv_st.registrations),
			State#serv_st{registrations = NewMap};
		true ->
			ChatPid = spawn(chatroom,start_chatroom,[ChatName]),
			ChatPid!{self(),Ref,register,ClientPID,Nickname},

			% MapSize = maps:size(State#serv_st.registrations),
			% if
			% 	MapSize > 0 ->
			% 		CpidList = maps:get(ChatName ,State#serv_st.registrations);
			% 	true ->
			% 		CpidList = []
			% end,
			CpidList = [],
			NewList = lists:append(CpidList,[ClientPID]),
			NewMap = maps:put(ChatName,NewList,State#serv_st.registrations),
			NewState1 = State#serv_st{registrations = NewMap},

			NewCMap = maps:put(ChatName,ChatPid,State#serv_st.chatrooms),
			NewState1#serv_st{chatrooms = NewCMap}
	end.
    % NewState.

%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
	ChatroomPID = maps:get(ChatName,State#serv_st.chatrooms),
	ClientPIDsList = maps:get(ChatName, State#serv_st.registrations),
	NewClientPIDsList = [X || X <- ClientPIDsList, not(X =:= ClientPID)],
	NewMap = maps:update(ChatName,NewClientPIDsList,State#serv_st.registrations),
	NewState = State#serv_st{registrations = NewMap},

	ChatroomPID!{self(), Ref, unregister, ClientPID},
	ClientPID!{self(), Ref, ack_leave},
	NewState.

%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
	NicknamesList = maps:values(State#serv_st.nicks),
	IsMember = lists:member(NewNick,NicknamesList),
	if
		IsMember ->
			ClientPID!{self(), Ref, err_nick_used},
			State;
		true ->
			NewMap = maps:update(ClientPID,NewNick,State#serv_st.nicks),
			NewState = State#serv_st{nicks = NewMap},
			ChatMap = maps:filter(fun(_K,V) -> lists:member(ClientPID,V)end,NewState#serv_st.registrations),
			ListofChatrooms = maps:keys(ChatMap),
			ListOfChatroomPIDs = [maps:get(X,NewState#serv_st.chatrooms) || X <- ListofChatrooms],
			[X!{self(),Ref,update_nick,ClientPID,NewNick} || X <- ListOfChatroomPIDs],
			ClientPID!{self(),Ref,ok_nick},
			NewState
	end.

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
	NewMap = maps:remove(ClientPID, State#serv_st.nicks),
	NewState = State#serv_st{nicks = NewMap},
	ChatMap = maps:filter(fun(_K,V) -> lists:member(ClientPID,V)end,NewState#serv_st.registrations),
	ListofChatrooms = maps:keys(ChatMap),
	ListOfChatroomPIDs = [maps:get(X,NewState#serv_st.chatrooms) || X <- ListofChatrooms],
	[X!{self(), Ref, unregister, ClientPID} || X <- ListOfChatroomPIDs],

	NewMapC = maps:map(fun(_K,V) -> [X || X <- V,X =/= ClientPID] end,NewState#serv_st.registrations),
	NewStateWithoutClientPIDs = NewState#serv_st{registrations = NewMapC},
	ClientPID!{self(), Ref, ack_quit},
	NewStateWithoutClientPIDs.

