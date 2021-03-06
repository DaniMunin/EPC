%%% @doc
%%% Downloads, processes and indexes the given images.
%%% @end

-module(indexer).
-export([indexImage/1, start/0, stop/0]).

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
indexImage(ImageURL) ->
    indexer ! {self(), {index, ImageURL}}.

%% Stop indexer service
stop() ->
    indexer ! stop.


%% indexing loop
indexer_loop() ->
    receive
        {_From, {index, ImageURL}} ->
            Image = web_lib:downloadImage(ImageURL),
            case Image of
            	noImage -> ok;
            	_ ->
                    Perception = extractPerception(Image),
                    %io:format("Url: ~p~nPerception: ~p~n", [ImageURL, Perception]),
                    addToIndex(ImageURL, Perception)

            end,
            indexer_loop();
        stop ->
            ok
    end.


%% indexer service
indexer() ->
    register(indexer, self()),
    indexer_loop().


%% Executes on module load, initializes the needed structures
start() ->
    % indexer service
    spawn(?MODULE, indexer, []),
    ok.
