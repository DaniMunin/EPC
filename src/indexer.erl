%%% @doc
%%% Downloads, processes and indexes the given images.
%%% @end

-module(indexer).
-export([indexImage/1, start/0]).

-export([indexer/0]).

%%% Indexer service implementation
%% Extract image featureç
extractPerception(ImageData) ->
    phash ! {self(), {call, ImageData}},
    receive
        {phash, Perception} ->
            Perception
    end.


%% Add image url and perception to index database
addToIndex(ImageUrl, Perception) ->
    epc_dba:put_im(ImageUrl, Perception).


%% Main function
indexImage(ImageUrl) ->
    io:format("ImageURL: ~p~n", [ImageUrl]),
    indexer ! {self(), {index, ImageUrl}}.


%% indexing loop
indexer_loop() ->
    receive
        {_From, {index, ImageUrl}} ->
	    io:format("Indexer: ~p~n", [ImageUrl]),
            {ReturnCode, Data} = httpc:request(ImageUrl),
            if ReturnCode == ok ->
                    {_ReturnedCode, _ReturnedHeaders, ImageData} = Data,
                    Perception = extractPerception(ImageData),
                    io:format("Perception: ~p~n", [Perception]),
                    addToIndex(ImageUrl, Perception);
               true ->
                    ok
            end,
            indexer_loop()
    end.


%% indexer service
indexer() ->
    register(indexer, self()),
    indexer_loop().


%% Executes on module load, initializes the needed structures
start() ->
    % Inets required for HTTP client requests
    inets:start(),
    % indexer service
    spawn(?MODULE, indexer, []),
    ok.
