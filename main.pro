% beyzanur bektan
% 2019400174
% compiling: yes
% complete: yes
:- ['cmpecraft.pro'].

:- init_from_map.



% 10 points
% this predicate finds the min dict between two given points
manhattan_distance([X, Y], [Z, T], D) :- D is (abs(Z-X) + abs(T-Y)).




% 10 points
% this predicate finds the minimum value of the given list
minimum_of_list([M], M).
minimum_of_list([M|T], M) :- minimum_of_list(T, Min), M =< Min, !.
minimum_of_list([M|T], Min) :- minimum_of_list(T, Min), M > Min.





% 10 points

% predicate to append a list to a list
append_to_list([],L, L).
append_to_list([Head|Tail], List2, [Head|List]):-
     append_to_list(Tail, List2, List).


% predicate to append an elem to a list
append_elem([],X,[X]).
append_elem([H|T],X,[H|L]):-append_elem(T,X,L).


% iter predicate finds the object with minimum distance in the given list (listofids)
% listofids represents the keys of specific type of objects
iter(Listofids, X1, Y1, [H], Dist, Key, Object) :-
	manhattan_distance([X1, Y1], [Listofids.H.x, Listofids.H.y], Dist),
	Key = H,
	Object = Listofids.H, !.

iter(Listofids, X1, Y1, [H|T], Dist, Key, Object) :- 
    manhattan_distance([X1, Y1], [Listofids.H.x, Listofids.H.y], Dist),
    iter(Listofids, X1, Y1, T, Dist2, _, _),
    Key = H,
	Object = Listofids.H,
    Dist =< Dist2, !.

iter(Listofids, X1, Y1, [H|T], Dist2, Key, Object) :- 
    manhattan_distance([X1, Y1], [Listofids.H.x, Listofids.H.y], Dist),
    iter(Listofids, X1, Y1, T, Dist2, Key, Object),
    Dist >= Dist2.
    


% find nearest type predicate calls iter predicate with specific type of objects to find one with min distance
find_nearest_type(State, ObjectType, Key, Obj, Dist) :- 

    nth0(0, State, AgentDict),
    nth0(1, State, ObjectDict),
    findall(X, ObjectDict.X.type == ObjectType, ObjectsList),
    iter(ObjectDict, AgentDict.x, AgentDict.y, ObjectsList, Dist, Key, Obj).

          




% 10 points
% this predicate finds the shortest path from location of the agent to the given target
navigate_to(State, X, Y, ActionList, DepthLimit) :- 
    nth0(0, State, AgentDict),
    manhattan_distance([AgentDict.x, AgentDict.y], [X, Y], D),
    ( (AgentDict.y < Y) -> 
    	Nr is Y - AgentDict.y,
        findall(go_down, between(1,Nr,_), YList)
    ;  
    	Nr2 is AgentDict.y - Y,
    	findall(go_up, between(1,Nr2,_), YList) ),
    
    ( (AgentDict.x < X) -> 
    	Nr3 is X - AgentDict.x,
    	findall(go_right, between(1,Nr3,_), XList)
    ;  
    	Nr4 is AgentDict.x - X,
    	findall(go_left, between(1,Nr4,_), XList) ),

    append_to_list(XList, YList, TotalList),
    append_to_list([], TotalList, ActionList),
    D =< DepthLimit. 
    


% navigate to without depth limit
nvg_to(State, X, Y, ActionList) :- 
    nth0(0, State, AgentDict),
    ( (AgentDict.y < Y) -> 
    	Nr is Y - AgentDict.y,
        findall(go_down, between(1,Nr,_), YList)
    ;  
    	Nr2 is AgentDict.y - Y,
    	findall(go_up, between(1,Nr2,_), YList) ),
    
    ( (AgentDict.x < X) -> 
    	Nr3 is X - AgentDict.x,
    	findall(go_right, between(1,Nr3,_), XList)
    ;  
    	Nr4 is AgentDict.x - X,
    	findall(go_left, between(1,Nr4,_), XList) ),
    append_to_list(XList, YList, TotalList),
    append_to_list([], TotalList, ActionList). 
    


% 10 points
% this predicate returns the actions that agent should execute to chop nearest tree
chop_nearest_tree(State, ActionList) :- 
   %If there is no tree in the state, then the predicate should be false kismi yok
    % gecici olarak depthlimiti 9999 yaptim
    find_nearest_type(State, tree, _, Obj, _),
    nvg_to(State, Obj.x, Obj.y, NvgList),
    findall(left_click_c, between(1,4,_), LCList),
	append_to_list(NvgList, LCList, TotalList),
    append_to_list([], TotalList, ActionList).


% 10 points
% this predicate returns the actions that agent should execute to mine nearest stone
mine_nearest_stone(State, ActionList) :- 
    find_nearest_type(State, stone, _, Obj, _),
    nvg_to(State, Obj.x, Obj.y, NvgList),
    findall(left_click_c, between(1,4,_), LCList),
	append_to_list(NvgList, LCList, TotalList),
    append_to_list([], TotalList, ActionList).


% 10 points
% this predicate returns the actions that agent should execute to gather nearest food
gather_nearest_food(State, ActionList) :-
    find_nearest_type(State, food, _, Obj, _),
    nvg_to(State, Obj.x, Obj.y, NvgList),
    findall(left_click_c, between(1,1,_), LCList),
	append_to_list(NvgList, LCList, TotalList),
    append_to_list([], TotalList, ActionList).




% 10 points
% this predicate returns the actions that agent should execute to collect requirements to craft stick
collect_requirements(State, stick, ActionList) :- 
    nth0(0, State, AgentDict),
    get_dict(inventory, AgentDict, Inv),
    findall(LogCount, get_dict(log, Inv, LogCount), Bag),
    (length(Bag,0) ->  LogCount is 0 ;   get_dict(log, Inv, LogCount) ),
    (   (LogCount < 2) ->  
    	chop_nearest_tree(State, ActionList)
    ;   append_to_list(ActionList, [], ActionList)).



% this predicate returns the actions that agent should execute to collect requirements to craft stone pickaxe
collect_requirements(State, stone_pickaxe, ActionList) :- 
    nth0(0, State, AgentDict),
    get_dict(inventory, AgentDict, Inv),
    findall(LogCount, get_dict(log, Inv, LogCount), Bag),
    (length(Bag,0) ->  LogCount is 0 ;   get_dict(log, Inv, LogCount) ),
    findall(StickCount, get_dict(stick, Inv, StickCount), Bag1),
    (length(Bag1,0) ->  StickCount is 0 ;   get_dict(stick, Inv, StickCount) ),
    findall(CobblestoneCount, get_dict(cobblestone, Inv, CobblestoneCount), Bag2),
    (length(Bag2,0) ->  CobblestoneCount is 0 ;   get_dict(cobblestone, Inv, CobblestoneCount) ),
    
    
    (   (CobblestoneCount < 3) ->  
    	mine_nearest_stone(State, ActionList1),
        execute_actions(State, ActionList1, FinalState1)
    ;   
    % actlist bos olunca error veriyo?
    	append_to_list([], [], ActionList1),
    	FinalState1 = State
    ),
    
    
    
    (   (LogCount >= 3 , StickCount >= 2) ->  
        ActionList = ActionList1
    ;   
        (  (LogCount >= 5) -> 
        	append_to_list(ActionList1, [craft_stick], ActionList2),
        	ActionList = ActionList2
    	;   
        	chop_nearest_tree(FinalState1, ActionList3),
            append_to_list(ActionList1, ActionList3, ActionList4),
            execute_actions(FinalState1, ActionList3, FinalState2),
            chop_nearest_tree(FinalState2, ActionList5),
            append_to_list(ActionList4, ActionList5, ActionList6),
            append_to_list(ActionList6, [craft_stick], ActionList7),
            ActionList = ActionList7
    	)
    ).


    
    
 
% this predicate returns the actions that agent should execute to collect requirements to craft stone axe
collect_requirements(State, stone_axe, ActionList) :- 
    nth0(0, State, AgentDict),
    get_dict(inventory, AgentDict, Inv),
    findall(LogCount, get_dict(log, Inv, LogCount), Bag),
    (length(Bag,0) ->  LogCount is 0 ;   get_dict(log, Inv, LogCount) ),
    findall(StickCount, get_dict(stick, Inv, StickCount), Bag1),
    (length(Bag1,0) ->  StickCount is 0 ;   get_dict(stick, Inv, StickCount) ),
    findall(CobblestoneCount, get_dict(cobblestone, Inv, CobblestoneCount), Bag2),
    (length(Bag2,0) ->  CobblestoneCount is 0 ;   get_dict(cobblestone, Inv, CobblestoneCount) ),
    
    
    (   (CobblestoneCount < 3) ->  
    	mine_nearest_stone(State, ActionList1),
        execute_actions(State, ActionList1, FinalState1)
    ;   
    % actlist bos olunca error veriyo?
    	append_to_list([], [], ActionList1),
    	FinalState1 = State
    ),
    
    
    
    (   (LogCount >= 3 , StickCount >= 2) ->  
        ActionList = ActionList1
    ;   
        (  (LogCount >= 5) -> 
        	append_to_list(ActionList1, [craft_stick], ActionList2),
        	ActionList = ActionList2
    	;   
        	chop_nearest_tree(FinalState1, ActionList3),
            append_to_list(ActionList1, ActionList3, ActionList4),
            execute_actions(FinalState1, ActionList3, FinalState2),
            chop_nearest_tree(FinalState2, ActionList5),
            append_to_list(ActionList4, ActionList5, ActionList6),
            append_to_list(ActionList6, [craft_stick], ActionList7),
            ActionList = ActionList7
    	)
    ).




% this predicate checks if given location is empty, if it is empty, returns false
my_tile_occupied(X, Y, State) :-
    State = [_, StateDict, _],
    get_dict(_, StateDict, Object),
    get_dict(x, Object, Ox),
    get_dict(y, Object, Oy),
    X = Ox, Y = Oy.


% this predicate iterates over 3x3 areas to find castle location, it checks x coordinates from 1 to (max-2), y coordinates from 1 to (ymax-2)
is_empty_x(MyX, MyY, State, Empty_X1, Empty_X2, Empty_X3, Empty_Y1, Empty_Y2, Empty_Y3):-     
	width(W),
	height(H),
	WdthMax is W-4,
	HghtMax is H-4,
    MyX2 is MyX+1,
    MyX3 is MyX+2,
    MyY2 is MyY+1,
    MyY3 is MyY+2,
    
    (   (MyX =< WdthMax) ->
    	(   (\+ my_tile_occupied(MyX, MyY, State)), (\+ my_tile_occupied(MyX2, MyY, State)), (\+ my_tile_occupied(MyX3, MyY, State)),
            (\+ my_tile_occupied(MyX, MyY2, State)), (\+ my_tile_occupied(MyX2, MyY2, State)), (\+ my_tile_occupied(MyX3, MyY2, State)),
            (\+ my_tile_occupied(MyX, MyY3, State)), (\+ my_tile_occupied(MyX2, MyY3, State)), (\+ my_tile_occupied(MyX3, MyY3, State)) ->
        
        		Empty_X1 = MyX, Empty_X2 = MyX2, Empty_X3 = MyX3, Empty_Y1 = MyY, Empty_Y2 = MyY2, Empty_Y3 = MyY3
        
        
        
        ;   
        
                NewMyX is MyX + 1,
                is_empty_x(NewMyX, MyY, State, Empty_X1, Empty_X2, Empty_X3, Empty_Y1, Empty_Y2, Empty_Y3)
            
            
        )
        
      ;
    	(   NewMyY is MyY + 1,(NewMyY =< HghtMax)  ->
			
        	is_empty_x(1, NewMyY, State, Empty_X1, Empty_X2, Empty_X3, Empty_Y1, Empty_Y2, Empty_Y3)
    	; 
        false
        )
      ).  
        
        
       

% 5 points
% this predicate calls the is_empty_x predicate and if it is true, it returns min and max coordinates of empty area 
find_castle_location(State, XMin, YMin, XMax, YMax) :-
        (   (   is_empty_x(1, 1, State, Empty_X1, _, Empty_X3, Empty_Y1, _, Empty_Y3)) ->  
        XMin = Empty_X1, YMin = Empty_Y1, XMax = Empty_X3, YMax = Empty_Y3
        ;   false
        ).
        
    	
        


% 15 points
% this predicate first collects 9 cobblestones to make a castle, then it checks if there is a 3x3 empty area, if there is, it places cobblestones to that area
make_castle(State, ActionList) :-
    nth0(0, State, AgentDict),
    get_dict(inventory, AgentDict, Inv),
    findall(CobblestoneCount, get_dict(cobblestone, Inv, CobblestoneCount), Bag),
    (length(Bag,0) ->  CobblestoneCount is 0 ;   get_dict(cobblestone, Inv, CobblestoneCount) ),
    (   (CobblestoneCount < 9) ->  
    	mine_nearest_stone(State, ActionList1),
        append_to_list([], ActionList1, Acts1),
		execute_actions(State, ActionList1, State2),
    	mine_nearest_stone(State2, ActionList2),
        append_to_list(Acts1, ActionList2, Acts2),
    	execute_actions(State2, ActionList2, State3),
    	mine_nearest_stone(State3, ActionList3),
        append_to_list(Acts2, ActionList3, Acts3),
    	execute_actions(State3, ActionList3, State4),
        find_castle_location(State4, XMin, YMin, _, _),
        X2 is XMin +1,
        Y2 is YMin +1,
        nvg_to(State4, X2, Y2, ActionList4),
        append_to_list(Acts3, ActionList4, Acts4),
        execute_actions(State4, ActionList4, _),
        append_to_list(Acts4, [place_c, place_e, place_n, place_w, place_s, place_ne, place_nw, place_sw, place_se], ActionList)
    ;   
    
    	find_castle_location(State, XMin, YMin, _, _),
        X2 is XMin +1,
        Y2 is YMin +1,
        nvg_to(State, X2, Y2, ActionList1),
        append_to_list([], ActionList1, Acts1),
	(length(Acts1,0) -> append_to_list(Acts1, [place_c, place_e, place_n, place_w, place_s, place_ne, place_nw, place_sw, place_se], ActionList)
	;   
    execute_actions(State, Acts1, _),
    append_to_list(Acts1, [place_c, place_e, place_n, place_w, place_s, place_ne, place_nw, place_sw, place_se], ActionList)

    
    )




        	)
    .



