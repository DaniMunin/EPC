%
%% webLib.erl
%%
%% @author Iago Lastra Rodriguez <iago.lastra@gmail.com>

-module(webLib).
-export([getData/1]).

-include_lib("xmerl/include/xmerl.hrl").

%Retunrs title if url = "http://www.youtube.com"
getData(URL) ->
	inets:start(),
	{_, { _, _, Data}} = httpc:request(URL),
        DataTree = mochiweb_html:parse(Data),
        ATags = getTags(<<"a">>, DataTree),
        ImgTags = getTags(<<"img">>, DataTree),
        Links = [ V || {P, V} <- ATags, P == <<"href">>],
	Images = [ V || {P, V} <- ImgTags, P == <<"src">>],
	{Links, Images}.


getChildrenTags(Tag, Children) ->
        lists:flatten(lists:map(fun (Child) -> getTags(Tag, Child) end,
                                Children)).

getTags(Tag, {_DataTag, Properties, Children}) when Tag =:= _DataTag ->
        [Properties | getChildrenTags(Tag, Children)];

getTags(Tag, {_DataTag, _Properties, Children}) when Tag =/= _DataTag ->
        getChildrenTags(Tag, Children);

getTags(_Tag, _) ->
        [].
