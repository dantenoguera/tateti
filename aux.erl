-module(aux).
-export([findminload/1]).


findminload(LoadList) ->
    [{HeadN,HeadL} | LoadListTail] = LoadList,
    {Node, _} = mymin({HeadN,HeadL}, LoadListTail),
    Node.


mymin({Node, Load}, []) ->
    {Node, Load};
mymin({HeadNM,HeadLM}, LoadList) ->
    [{HeadN,HeadL} | LoadListTail] = LoadList,
    if
        HeadLM > HeadL ->
            mymin({HeadN,HeadL},LoadListTail);
        true ->
            mymin({HeadNM, HeadLM}, LoadListTail)
    end.

updateloadlist([{Node1, Load1} | Tail], {Node, Load}) ->
    case Node1 == Node of
        true -> [{Node, Load} | Tail];
        false -> [{Node1, Load1} | updateloadlist(Tail, {Node, Load})]
    end.
