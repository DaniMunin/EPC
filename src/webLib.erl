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
	ATags = lists:flatten(filterTags(<<"a">>, DataTree)),
	ImgTags = lists:flatten(filterTags(<<"img">>, DataTree)),
	Links = [ V || {P, V} <- ATags, P == <<"href">>],
	Images = [ V || {P, V} <- ImgTags, P == <<"src">>],
	{Links, Images}.


getChildrenTags(Tag, Children) ->
	lists:map(fun (Child) -> filterTags(Tag, Child) end, Children).

filterTags(Tag, {_DataTag, Properties, Children}) when Tag =:= _DataTag ->
	[Properties | getChildrenTags(Tag, Children)];

filterTags(Tag, {_DataTag, _Properties, Children}) when Tag =/= _DataTag ->
	getChildrenTags(Tag, Children);

filterTags(_Tag, _) ->
	[].
